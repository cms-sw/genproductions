## import the required files
# example: import maddm_interface as maddm_interface # local file
#          import madgraph.various.cluster as cluster #MG5 distribution file

from madgraph.various.cluster import *
import time

#local file

################################################################################
# Copyright (c) 2009 The MadGraph5_aMC@NLO Development team and Contributors             
#
# This file is a part of the MadGraph5_aMC@NLO project, an application which           
# automatically generates Feynman diagrams and matrix elements for arbitrary    
# high-energy processes in the Standard Model and beyond.                       
#
# It is subject to the MadGraph5_aMC@NLO license which should accompany this             
# distribution.                                                                 
#                                                                               
# For more information, visit madgraph.phys.ucl.ac.be and amcatnlo.web.cern.ch            
#                                                                               
################################################################################
import subprocess
import logging
import os
import time
import re
import glob
import inspect
import sys
import platform
import signal
import uuid
import socket
import atexit

logger = logging.getLogger('madgraph.cluster') 

try:
    from madgraph import MadGraph5Error
    import madgraph.various.misc as misc
except Exception, error:
    if __debug__:
        print  str(error)
    from internal import MadGraph5Error
    import internal.misc as misc

pjoin = os.path.join



multiple_try = misc.multiple_try
pjoin = os.path.join


def cleansubproc(subproc):
    subproc.terminate()

class CMSCondorCluster(CondorCluster):
    """Basic class for dealing with cluster submission"""
    
    name = 'condor'
    job_id = 'CONDOR_ID'
    condor_q_max_retries = int(os.environ.get("CONDOR_QUERY_MAX_RETRIES",10))
    condor_q_sleep_per_retry = int(os.environ.get("CONDOR_QUERY_SLEEP_PER_RETRY",10))

    def __init__(self, *args, **opt):
        """Init the cluster """

        super(CMSCondorCluster, self).__init__(self, *args, **opt)
        try:
            import htcondor
            self.schedd = htcondor.Schedd()
            self._action = htcondor.JobAction
        except Exception, error:
            raise ClusterManagmentError, 'could not import htcondor python API: \n%s' % error
        self.hold_list = os.environ.get("CONDOR_RELEASE_HOLDCODES", "")
        self.max_shadows = os.environ.get("CONDOR_RELEASE_HOLDCODES_SHADOW_LIM", "")
        self.walltimes = os.environ.get("CONDOR_SET_MAXWALLTIMES", "")
        self.spool = False
        self.debug_print = os.environ.get("CONDOR_DEBUG_PRINT", "")
        
    def query(self, ids, ads=[], lim=-1):
        """Query the Schedd via HTCondor Python API"""
        q = self.schedd.query(
            'stringListmember(string(ClusterId),"{0}")'.format(
                ",".join(str(id) for id in ids)), ads, limit=lim)
        return q

    def edit(self, ids, ad, value):
        """ Edit a single parameter classAd in a list of ClusterIds"""
        q = self.schedd.edit(
            'stringListmember(string(ClusterId), "{0}")'.format(
                ",".join(str(id) for id in ids)), str(ad), str(value))
        return q

    def status_map(self, status):
        if status == 0:
            return 'U'
        elif status == 1:
            return 'I'
        elif status == 2:
            return 'R'
        elif status == 3:
            return 'X'
        elif status == 4:
            return 'C'
        elif status == 5:
            return 'H'
        elif status == 6:
            return 'E'
        else:
            return str(status)

    def _release_holdcode(self, id, holdcode, holdsubcode):
        """ Automatically release job if it is held with the specified
        hold code and subcode"""
        q = self.query([str(id)], ["JobStatus", "HoldReasonCode", "HoldReasonSubCode"], lim=1)
        if len(q)>0:
            status = self.status_map(q[0]["JobStatus"])
        else:
            return False

        if status == 'H':
            job_hold_code = q[0]["HoldReasonCode"]
            job_hold_subcode = q[0]["HoldReasonSubCode"]
            if (job_hold_code == holdcode) and (job_hold_subcode == holdsubcode or holdsubcode == -1):
                logger.info("ClusterId {0} was held with code {1}, subcode {2}. Releasing it.".format(id, job_hold_code, job_hold_subcode))
                self.schedd.act(self._action.Release, "ClusterId =?= {0}".format(id))
                return True
        return False

    def release_holdcodes(self, id, holdcodes_list, numshadow_limit=10):
        """ Automatically release job if it is held with any of the specified
        list of hold codes and subcodes. The convention is hold_code:hold_subcode
        separated by a comma.If no hold_subcode is specified, only hold_code is
        taken into account. E.g:
        holdcodes_list="26:119,13,30:1"
        """
        if not isinstance(holdcodes_list, (str, unicode)):
            logger.info("Holdcodes_list is not a string.")
            return False

        q = self.query([str(id)], ["NumShadowStarts"], lim=1)
        num_shadow_starts = q[0]["NumShadowStarts"] if "NumShadowStarts" in q[0] else 0
        if num_shadow_starts > numshadow_limit:
            return False

        holdcodes_list = holdcodes_list.split(",")
        for holdcodes in holdcodes_list:
            holdcodes = holdcodes.split(":")
            if False in [i.strip().isdigit() for i in holdcodes]:
                logger.info("Could not parse holdcodes list, please verify the format.")
                return False
            holdcode = int(holdcodes[0].strip())
            holdsubcode = int(holdcodes[1].strip()) if len(holdcodes)>=2 else -1
            if self._release_holdcode(id, int(holdcode), int(holdsubcode)):
                return True
        return False

    def update_maxwalltime(self, id, walltimes_minutes):
        """ Automatically increase MaxWallTimeMins classAd from condor job
        if such job was evicted because its current max promised time was not enough.
        E.g: walltimes_min="480,960,2400"
        """
        if not isinstance(walltimes_minutes, (str, unicode)):
            return 0
        else:
            walltimes_minutes = walltimes_minutes.split(",")

        if False in [i.isdigit() for i in walltimes_minutes]:
            return 0

        q = self.query([str(id)], ["JobDuration", "MaxWallTimeMins", "LastMaxWalltimeUpdate_JobDuration"], lim=1)
        job_maxwalltime = q[0]["MaxWallTimeMins"] if "MaxWallTimeMins" in q[0] else 0
        if hasattr(job_maxwalltime, "eval"):
            job_maxwalltime = job_maxwalltime.eval()
        job_duration = q[0]["JobDuration"] if "JobDuration" in q[0] else -1.0
        last_update_job_duration = \
                q[0]["LastMaxWalltimeUpdate_JobDuration"] if "LastMaxWalltimeUpdate_JobDuration" in q[0] else -1.0

        if job_duration > job_maxwalltime*60 and job_duration != last_update_job_duration:
            remaining_walltimes = [int(i) for i in walltimes_minutes
                                   if int(i) > int(job_maxwalltime) and
                                   int(i) > (int(job_duration / 60) + int((job_duration / 60) % 1 > 0))]

            if remaining_walltimes:
                new_maxwalltime = min(remaining_walltimes)
                self.edit([str(id)], "LastMaxWalltimeUpdate_JobDuration", job_duration)
                self.edit([str(id)], "MaxWallTimeMins", new_maxwalltime)
                return new_maxwalltime

        return 0

    @store_input()
    @multiple_try()
    def submit2(self, prog, argument=[], cwd=None, stdout=None, stderr=None, 
                log=None, input_files=[], output_files=[], required_output=[], 
                nb_submit=0):
        """Submit the job on the cluster NO SHARE DISK
           input/output file should be give relative to cwd
        """
        
        if not required_output and output_files:
            required_output = output_files
        
        if (input_files == [] == output_files):
            return self.submit(prog, argument, cwd, stdout, stderr, log, 
                               required_output=required_output, nb_submit=nb_submit)
        
        text = """Executable = %(prog)s
                  output = %(stdout)s
                  error = %(stderr)s
                  log = %(log)s
                  %(argument)s
                  should_transfer_files = YES
                  when_to_transfer_output = ON_EXIT
                  transfer_input_files = %(input_files)s
                  %(output_files)s
                  Universe = vanilla
                  notification = Error
                  Initialdir = %(cwd)s
                  %(requirement)s
                  getenv=True
                  
                  +JobFlavour = "%(job_flavour)s"
                  
                  queue 1
               """
        
        if self.cluster_queue not in ['None', None]:
            requirement = 'Requirements = %s=?=True' % self.cluster_queue
        else:
            requirement = ''

        if cwd is None:
            cwd = os.getcwd()
        if stdout is None:
            stdout = '/dev/null'
        if stderr is None:
            stderr = '/dev/null'
        if log is None:
            log = '/dev/null'
        if not os.path.exists(prog):
            prog = os.path.join(cwd, prog)
        if argument:
            argument = 'Arguments = %s' % ' '.join([str(a) for a in argument])
        else:
            argument = ''
        # input/output file treatment
        if input_files:
            input_files = ','.join(input_files)
        else: 
            input_files = ''
        if output_files:
            output_files = 'transfer_output_files = %s' % ','.join(output_files)
        else:
            output_files = ''
        
        # keep condor logs if CONDOR_DEBUG_OUTPUT_PATH variable is defined
        debug_output_path = os.environ.get("CONDOR_DEBUG_OUTPUT_PATH", "")
        if debug_output_path : 
          stdout = os.path.normpath(debug_output_path) + "/" + "job_$(ClusterId)_$(JobId)_stdout.txt"
          stderr = os.path.normpath(debug_output_path) + "/" + "job_$(ClusterId)_$(JobId)_stderr.txt"
          log    = os.path.normpath(debug_output_path) + "/" + "job_$(ClusterId)_$(JobId)_condor.txt"
        
        # set job max work time
        job_flavour = os.environ.get("CONDOR_JOB_FLAVOUR", "nextweek")

        dico = {'prog': prog, 'cwd': cwd, 'stdout': stdout, 
                'stderr': stderr,'log': log,'argument': argument,
                'requirement': requirement, 'input_files':input_files, 
                'output_files':output_files, 'job_flavour':job_flavour}

        #open('submit_condor','w').write(text % dico)
        if self.debug_print : logger.info( text % dico )
        cmd = ['condor_submit']
        if(self.spool):
            cmd.append("-spool")
        a = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                             stdin=subprocess.PIPE)
        output, _ = a.communicate(text % dico)
        #output = a.stdout.read()
        #Submitting job(s).
        #Logging submit event(s).
        #1 job(s) submitted to cluster 2253622.
        if self.debug_print : logger.info( output )
            
        pat = re.compile("submitted to cluster (\d*)",re.MULTILINE)
        try:
            id = pat.search(output).groups()[0]
        except:
            raise ClusterManagmentError, 'fail to submit to the cluster: \n%s' \
                                                                        % output 
        self.submitted += 1
        self.submitted_ids.append(id)
        return id




    
    @multiple_try(nb_try=condor_q_max_retries, sleep=condor_q_sleep_per_retry)
    def control_one_job(self, id):
        """ control the status of a single job with it's cluster id """
        q = self.query([str(id)], ["JobStatus", "HoldReason"], lim=1)
        try:
            status = q[0]["JobStatus"]
        except Exception, error:
            raise ClusterManagmentError, 'could not retrieve job query:\n%s' % error

        s = self.status_map(status)

        if s == 'H':
            hold_reason = q[0]["HoldReason"]
            logger.warning("ClusterId %s held with HoldReason: %s" % (str(id), hold_reason))

        return s
    
    @check_interupt()
    @multiple_try(nb_try=condor_q_max_retries, sleep=condor_q_sleep_per_retry)
    def control(self, me_dir):
        """ control the status of a single job with it's cluster id """

        if not self.submitted_ids:
            return 0, 0, 0, 0

        packet = 15000
        idle, run, fail = 0, 0, 0
        ongoing = []
        for i in range(1+(len(self.submitted_ids)-1)//packet):
            start = i * packet
            stop = (i+1) * packet
            q = self.query(self.submitted_ids[start:stop], ["ClusterId", "JobStatus", "HoldReason"])

            for job in q:
                id, status = job["ClusterId"], self.status_map(job["JobStatus"])
                ongoing.append(int(id))
                if self.walltimes:
                    maxwalltime = self.update_maxwalltime(id, self.walltimes)
                    if maxwalltime:
                        logger.info("Updated ClusterId {0} MaxWallTimeMins to: {1}".format(id, maxwalltime))
                if status in ['I','U']:
                    idle += 1
                elif status == 'R':
                    run += 1
                elif status == 'H':
                    released = False
                    if self.hold_list:
                        released = self.release_holdcodes(id, self.hold_list, int(self.max_shadows)) \
                            if self.max_shadows else \
                            self.release_holdcodes(id, self.hold_list)
                    if not released:
                        reason = job["HoldReason"]
                        if ("Spool" in reason):
                            run += 1
                        else:
                            self.hold_msg = "ClusterId %s with HoldReason: %s" % (str(id), job["HoldReason"])
                            fail += 1
                elif status == 'C' and self.spool:
                    self.retrieve_output(id)
                else:
                    logger.warning("Failed condor job = ", id, job)
                    fail += 1

        for id in list(self.submitted_ids):
            if int(id) not in ongoing:
                status = self.check_termination(id)
                if status == 'wait':
                    run += 1
                elif status == 'resubmit':
                    idle += 1

        return idle, run, self.submitted - (idle+run+fail), fail

    def retrieve_output(self,id):
        # TODO: Error handling
        a = subprocess.Popen(['condor_transfer_data',str(id)], stdout=subprocess.PIPE,
                             stdin=subprocess.PIPE)
        output, err_ = a.communicate()

class CMSCondorSpoolCluster(CMSCondorCluster):
    """Same as CMSCondorCluster, except that we use 'spool' to transfer files."""

    name = 'condor'
    job_id = 'CONDOR_ID'
    condor_q_max_retries = int(os.environ.get("CONDOR_QUERY_MAX_RETRIES",10))
    condor_q_sleep_per_retry = int(os.environ.get("CONDOR_QUERY_SLEEP_PER_RETRY",10))

    def __init__(self, *args, **opt):
        """Init the cluster """

        super(CMSCondorSpoolCluster, self).__init__(self, *args, **opt)
        self.spool = True
        self.cluster_queue = None

class CMSLSFCluster(LSFCluster):
    """Basic class for dealing with cluster submission"""
    
    name = 'lsf'
    job_id = 'LSB_JOBID'

    def __init__(self,*args, **opts):
        """Init the cluster"""
        Cluster.__init__(self,*args, **opts)

        if self.temp_dir!=None:
            self.dorsync = True
            #print "starting rsync"
          
            cwd = os.getcwd()

            self.rsyncroot = cwd
            
            self.rsyncmodule = str(uuid.uuid4())
            
            #get free port number for rsyncd
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.bind(('localhost', 0))
            addr, port = sock.getsockname()
            sock.close()    
            
            self.rsyncport = port
            #print self.rsyncport
            
            rsynclog = os.path.join(cwd, 'rsyncd_%i.log' % self.rsyncport)
            rsynclock = os.path.join(cwd, 'rsyncd_%i.lock' % self.rsyncport)
            rsyncpid = os.path.join(cwd, 'rsyncd_%i.pid' % self.rsyncport)

            rsyncpasswd = str(uuid.uuid4())
            
            self.rsyncuser = 'madgraph'
            
            rsyncsecrets = "%s:%s" % (self.rsyncuser,rsyncpasswd)
            rsyncsecretsfile = os.path.join(cwd, 'rsyncsecrets_%i' % self.rsyncport)
            secretsh = open(rsyncsecretsfile,'w')
            os.chmod(rsyncsecretsfile, 0600)
            secretsh.write(rsyncsecrets)
          
            os.environ["MADGRAPHRSYNCPASSWD_%i" % self.rsyncport] = rsyncpasswd
            #print rsyncpasswd

            rsyncconf = """
              port = %(rsyncport)s
              pid file = %(rsyncpid)s
              log file = %(rsynclog)s
             
                [%(rsyncmodule)s]
                  comment = Random things available for download
                  lock file = %(rsynclock)s
                  secrets file = %(rsyncsecrets)s
                  path = %(path)s
                  list = yes 
                  use chroot = no
                  munge symlinks = no
                  read only = no
                  auth users = %(rsyncuser)s
            """ % {'rsyncport': self.rsyncport,
                   'rsyncmodule': self.rsyncmodule,
                   'path': cwd,
                  'rsynclog' : rsynclog,
                  'rsynclock' : rsynclock,
                  'rsyncpid' : rsyncpid,
                  'rsyncsecrets' : rsyncsecretsfile,
                  'rsyncuser' : self.rsyncuser,
                  }
            
            rsyncconffile = os.path.join(cwd, 'rsyncd_%i.conf' % self.rsyncport)
            open(rsyncconffile,'w').write(rsyncconf)
            
            self.rsyncd = subprocess.Popen(['rsync','--daemon', '--no-detach', '--config=%s' % rsyncconffile],cwd=cwd,stdout=subprocess.PIPE,stdin=subprocess.PIPE,stderr=subprocess.PIPE)
            atexit.register(cleansubproc,self.rsyncd)
            
        else:
            self.dorsync = False

    @multiple_try()
    def submit(self, prog, argument=[], cwd=None, stdout=None, stderr=None, log=None,
               required_output=[], nb_submit=0):
        """Submit the job prog to an LSF cluster"""
        
        
        me_dir = self.get_jobs_identifier(cwd, prog)
        
        text = ""
        command = ['bsub', '-C0', '-J', me_dir]
        if cwd is None:
            cwd = os.getcwd()
        else: 
            text = " cd %s;" % cwd
        if stdout and isinstance(stdout, str):
            command.extend(['-o', stdout])
        if stderr and isinstance(stdout, str):
            command.extend(['-e', stderr])
        elif stderr == -2: # -2 is subprocess.STDOUT
            pass
        if log is None:
            log = '/dev/null'
        
        text += 'if [ -n $CMSSW_BASE ]; then cd $CMSSW_BASE; eval `scramv1 runtime -sh`; cd -; fi;'
        
        text += prog
        if argument:
            text += ' ' + ' '.join(argument)
        
        if self.cluster_queue and self.cluster_queue != 'None':
            command.extend(['-q', self.cluster_queue])

        a = misc.Popen(command, stdout=subprocess.PIPE, 
                                      stderr=subprocess.STDOUT,
                                      stdin=subprocess.PIPE, cwd=cwd)
            
        output = a.communicate(text)[0]
        #Job <nnnn> is submitted to default queue <normal>.
        try:
            id = output.split('>',1)[0].split('<')[1]
        except:
            raise ClusterManagmentError, 'fail to submit to the cluster: \n%s' \
                                                                        % output 
        if not id.isdigit():
            raise ClusterManagmentError, 'fail to submit to the cluster: \n%s' \
                                                                        % output 
        self.submitted += 1
        self.submitted_ids.append(id)
        return id        
        
        
    @store_input()
    @multiple_try()
    def submit2(self, prog, argument=[], cwd=None, stdout=None, stderr=None,
            log=None, input_files=[], output_files=[], required_output=[],nb_submit=0):
        """How to make one submission. Return status id on the cluster.
        NO SHARE DISK"""

        #print "running lsf submit2"

        if cwd is None:
            cwd = os.getcwd()
        if not os.path.exists(prog):
            prog = os.path.join(cwd, prog)

        if not required_output and output_files:
            required_output = output_files

        if not self.dorsync or (not input_files and not output_files):
            # not input/output so not using submit2
            return self.submit(prog, argument, cwd, stdout, stderr, log,
        required_output=required_output, nb_submit=nb_submit)

        if self.rsyncd.poll()!=None:
            raise RuntimeError("rsyncd not running")

        if cwd is None:
            cwd = os.getcwd()
        if not os.path.exists(prog):
            prog = os.path.join(cwd, prog)
        temp_file_name = "sub." + os.path.basename(prog) + '.'.join(argument)
               
        input_files.append(prog)                
               
        hostname = platform.node()
               
        rsynccwd = cwd
        if rsynccwd.startswith(self.rsyncroot):
            rsynccwd = rsynccwd[len(self.rsyncroot):]                   
               
        infilelist = ""
        for input_file in input_files:
            #make sure input_files are absolute paths
            if not input_file.startswith('/'):
                input_file = os.path.join(cwd,input_file)
            #convert to paths relative to rsyncd root
            if input_file.startswith(self.rsyncroot):
                input_file = input_file[len(self.rsyncroot):]
            infilelist += "%s@%s::%s/%s " % (self.rsyncuser,hostname,self.rsyncmodule, input_file)
        infilelist += "./"
        
        outfilelist = ""
        for output_file in output_files:
            outfilelist += "%s " % (output_file)
        outfilelist += "%s@%s::%s/%s" % (self.rsyncuser,hostname,self.rsyncmodule,rsynccwd)
            
        text = """#!/bin/bash
        
            SUBMITTERHOST=%(hostname)s            

            if [ -n $CMSSW_VERSION ]
            then
              scramv1 project CMSSW $CMSSW_VERSION
              cd $CMSSW_VERSION
              eval `scramv1 runtime -sh`
              cd -
            fi
                             
            export RSYNC_PASSWORD=$MADGRAPHRSYNCPASSWD_%(rsyncport)s
                 
            #dereference symlinks for input
            rsync -vvv --timeout=600 --contimeout=600 --port %(rsyncport)s -rptL %(infilelist)s

            echo '%(arguments)s' > arguments
            chmod +x ./%(script)s        
            %(program)s ./%(script)s %(arguments)s
            
            #copy symlinks as symlinks for output and don't overwrite existing files unless updated
            rsync -vvv --timeout=600 --contimeout=600 --port %(rsyncport)s -rptul %(outfilelist)s
            
            """
        dico = {'script': os.path.basename(prog),
        'hostname': hostname,
        'infilelist': infilelist,
        'outfilelist': outfilelist,
        'output_files': ' '.join(output_files),
        'rsyncport': self.rsyncport,
        'arguments': ' '.join([str(a) for a in argument]),
        'program': ' ' if '.py' in prog else 'bash'}

        me_dir = self.get_jobs_identifier(cwd, prog)

        text = text % dico
        cwdpath = "/tmp/" + os.environ.get("USER", '')
        command = ['bsub', '-cwd', cwdpath, '-C0', '-J', me_dir]
        if cwd is None:
            cwd = os.getcwd()
        #else:
            #text += " cd %s;" % cwd
        if stdout and isinstance(stdout, str):
            command.extend(['-o', stdout])
        if stderr and isinstance(stdout, str):
            command.extend(['-e', stderr])
        elif stderr == -2: # -2 is subprocess.STDOUT
            pass
        if log is None:
            log = '/dev/null'

        if self.cluster_queue and self.cluster_queue != 'None':
            command.extend(['-q', self.cluster_queue])

        submitenv = os.environ.copy()
        submitenv["TMPDIR"] = "/tmp/" + submitenv.get("USER", '')
        a = misc.Popen(command, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        stdin=subprocess.PIPE, cwd=cwd,
        env=submitenv)

        output = a.communicate(text)[0]
        #Job <nnnn> is submitted to default queue <normal>.
        try:
            id = output.split('>',1)[0].split('<')[1]
        except:
            raise ClusterManagmentError, 'fail to submit to the cluster: \n%s' \
            % output
        if not id.isdigit():
            raise ClusterManagmentError, 'fail to submit to the cluster: \n%s' \
            % output
        self.submitted += 1
        self.submitted_ids.append(id)
        return id         

    @multiple_try()   
    def control(self, me_dir):
        """ control the status of a single job with it's cluster id """
        
        if not self.submitted_ids:
            return 0, 0, 0, 0        

        jobstatus = {}
        
        #split into smaller groups of 200 jobs to avoid problems with truncated output
        idsplitting = 200
        splitids = [self.submitted_ids[i:i+idsplitting] for i in range(0, len(self.submitted_ids), idsplitting)]
        
        for ids in splitids:
            cmd = "bjobs " + ' '.join(ids) 
            status = misc.Popen([cmd], shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

            for line in status.stdout:
                line = line.strip()
                if 'JOBID' in line:
                    continue
                splitline = line.split()
                if splitline[0] in ids:
                    id = splitline[0]
                    jobstatus[id] = splitline[2]
                else:
                    splitline = re.split('[><]',line)
                    if len(splitline)==3 and splitline[0] == 'Job ' and splitline[2] == " is not found" and splitline[1] in ids:
                        id = splitline[1]
                        jobstatus[id] = 'MISSING'        

        idle, run, fail = 0, 0, 0
        for id in self.submitted_ids[:]:
            if id in jobstatus:
                status = jobstatus[id]
            else:
                status = 'PEND'

            if status == 'RUN':
                run += 1
            elif status in ['DONE', 'EXIT', 'MISSING']:
                status = self.check_termination(id)
                if status == 'wait':
                    run += 1
                elif status == 'resubmit':
                    idle += 1                
            else:
                idle += 1

        return idle, run, self.submitted - (idle+run+fail), fail

# Three types of functionality are allowed in a plugin
#   1. new output mode
#   2. new cluster support
#   3. new interface

# 1. Define new output mode
#    example: new_output = {'myformat': MYCLASS}
#    madgraph will then allow the command "output myformat PATH"
#    MYCLASS should inherated of the class madgraph.iolibs.export_v4.VirtualExporter 
new_output = {}

# 2. Define new way to handle the cluster.
#    example new_cluster = {'mycluster': MYCLUSTERCLASS}
#    allow "set cluster_type mycluster" in madgraph
#    MYCLUSTERCLASS should inherated from madgraph.various.cluster.Cluster
new_cluster = {'cms_condor': CMSCondorCluster, 'cms_condor_spool': CMSCondorSpoolCluster, 'cms_lsf':CMSLSFCluster}


# 3. Define a new interface (allow to add/modify MG5 command)
#    This can be activated via ./bin/mg5_aMC --mode=PLUGINNAME
## Put None if no dedicated command are required
new_interface = None
 

 
########################## CONTROL VARIABLE ####################################
__author__ = 'Andreas Albert'
__email__ = 'andreas.albert@cern.ch'
__version__ = (1,0,0)
minimal_mg5amcnlo_version = (2,6,0) 
maximal_mg5amcnlo_version = (1000,1000,1000)
latest_validated_version = (2,6,0)
