#!/usr/bin/env python

'''
Script for POWHEG generator production
By Roberto Covarelli 10/30/2018
Based on Yuan Chao's script
'''

import commands
import fileinput
import argparse
import sys
import os
from Utilities import helpers

TESTING = 0
QUEUE = ''


#POWHEG_SOURCE = "powhegboxV2_rev3728_date20200429.tar.gz"
POWHEG_SOURCE = "powhegboxV2_rev3987_date20220622.tar.gz"
POWHEGRES_SOURCE = "powhegboxRES_rev4004_date20221025.tar.gz"


rootfolder = os.getcwd()


def runCommand(command, printIt = False, doIt = 1) :
    if args.fordag and 'condor_submit' in command:
        print('No job submission when preparing DAG')
        return
    if TESTING :
        printIt = 1
        doIt = 0
    if printIt : print ('> ' + command)
    if doIt :
        commandOutput = commands.getstatusoutput(command)
        if printIt : print commandOutput[1]
        return commandOutput[0]
    else :    print ('    jobs not submitted')
    return 1

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

def prepareCondorScript( tag, i, folderName, queue, SCALE = '0', njobs = 0, runInBatchDir = False, slc6 = 0):
   '''prepare the Condor submission script'''
   
   if (slc6):
       print('Preparing to run in slc6 using singularity')

   filename = 'run_' + folderName + '_' + tag + '.condorConf'
   execname = 'run_' + tag
   logname =  'run_' + tag
   if runInBatchDir:
       folderName = rootfolder + '/' + folderName
       execname = folderName + '/' + execname
       logname =  folderName + '/run_' + tag
   f = open(filename, 'w')

   if (i == 'multiple') :
       if (slc6) :
          f.write('executable             = %s/slc6wrapper.sh \n' % rootfolder)
          f.write('arguments              = ' + execname + '_$(ProcId).sh \n')
       else:
          f.write('executable              = ' + execname + '_$(ProcId).sh \n')
   elif (i == 'hnnlo') :
       f.write('executable              = ' + folderName + '/' + SCALE + '/' + 'launch_NNLO.sh \n')
       f.write('arguments               = HNNLO-LHC13-R04-APX2-' + SCALE + '.input $(ClusterId)$(ProcId) \n')
   else :
       f.write('executable              = ' + execname + '.sh \n')
   f.write('getenv                 =  True \n')
   f.write('output                  = ' + logname + '_$(ProcId).out \n')
   f.write('error                   = ' + logname + '_$(ProcId).err \n')
   f.write('log                     = ' + logname + '.log \n')
   if not runInBatchDir:
       f.write('initialdir              = ' + rootfolder + '/' + folderName + '\n')

   f.write('+JobFlavour             = "'+ queue +'" \n')

   f.write('periodic_remove         = JobStatus == 5  \n')
   f.write('WhenToTransferOutput    = ON_EXIT_OR_EVICT \n')
   f.write('transfer_output_files   = "" \n')
   if njobs > 0:
       f.write('queue '+str(njobs)+'\n')
   f.write('\n')

   f.close()

   if os.path.exists('additional.condorConf') :
       filenamenew = 'run_' + tag + 'new.condorConf'
       ff = open('mergeCondorConf.sh', 'w')
       ff.write('#!/bin/bash \n \n')
       ff.write('mv ' + filename + ' ' + filenamenew + '\n')
       ff.write('cat ' + filenamenew + ' additional.condorConf > ' + filename + '\n')
       ff.write('rm -f ' + filenamenew + '\n')
       ff.close()
       runCommand('sh mergeCondorConf.sh')

   return filename


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

def prepareJob(tag, i, folderName,process) :
    filename = folderName+'/run_%s.sh' % tag

    template_dict = {
        "process" : process,
        "folderName" : folderName,
        "rootfolder" : rootfolder,
    }

    template_file = "%s/Templates/prepareJob_template.sh" % rootfolder
    helpers.fillTemplatedFile(template_file, filename, template_dict, "w")

    return filename

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def prepareJobForEvents (tag, i, folderName, EOSfolder) :
    runCommand('rm ' + rootfolder + '/' + folderName + '/log_' + tag + '.log')
    filename = 'run_%s.sh' % tag

    prepareJob(tag, i, folderName,args.prcName)

    template_dict = {
        "folderName" : folderName,
        "rootfolder" : rootfolder,
        "iJob"       : i,
        "tag"        : tag,
    }

    template_file = "%s/Templates/prepareJobForEvents_template.sh" % rootfolder
    helpers.fillTemplatedFile(template_file, filename, template_dict)

    return filename

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def runParallelXgrid(parstage, xgrid, folderName, nEvents, njobs, powInputName, jobtag, rndSeed, process) :
    # parstage, xgrid are strings!

    print 'Running parallel jobs for grid'
    #print folderName

    inputName = folderName + "/powheg.input"

    sedcommand = 'sed -i "s/NEVENTS/'+nEvents+'/ ; s/SEED/'+rndSeed+'/ ; s/.*parallelstage.*/parallelstage '+parstage+'/ ; s/.*xgriditeration.*/xgriditeration '+xgrid+'/ ; s/.*manyseeds.*/manyseeds 1/ ; s/fakevirt.*// " '+inputName

    with open(os.path.join(folderName, "pwgseeds.dat"), "w") as f:
        for i in xrange(njobs):
            f.write(str(int(rndSeed) + i)+"\n")

    #print sedcommand
    runCommand(sedcommand)

    if (parstage == '1') :
        if not 'parallelstage' in open(inputName).read() :
            runCommand("echo \'\n\nparallelstage "+parstage+"\' >> "+inputName)
        if not 'xgriditeration' in open(inputName).read() :
            runCommand("echo \'xgriditeration "+xgrid+"\' >> "+inputName)

        if not 'manyseeds' in open(inputName).read() :
            runCommand("echo \'manyseeds 1\' >> "+ inputName)

    runCommand('cp -p '+inputName+' '+inputName+'.'+parstage+'_'+str(xgrid))

    for i in range (0, njobs) :
        jobID = jobtag + '_' + str(i)
        jobname = prepareJob(jobID, i, folderName,args.prcName)

        filename = folderName+'/run_' + jobID + '.sh'
        f = open(filename, 'a')
        #f.write('cd '+rootfolder+'/'+folderName+'/ \n')
        if process == 'WWJ' :
          f.write('echo \"Copy TwoLoops grids\"\n')
          f.write('ls\n') 
          f.write('wget https://wwwth.mpp.mpg.de/members/wieseman/download/codes/WW_MiNNLO/VVamp_interpolation_grids/WW_MiNNLO_2loop_grids_reduced1.tar.gz\n')
          f.write('tar xzf WW_MiNNLO_2loop_grids_reduced1.tar.gz\n')
          f.write('ls\n') 
        f.write('cp -p ' + rootfolder + '/' + folderName + '/powheg.input.'+parstage+'_'+str(xgrid) + ' ./powheg.input' + '\n') # copy input file for this stage explicitly, needed by condor dag
        f.write('echo ' + str(i+1) + ' | ./pwhg_main \n')
        f.write('echo "Workdir after run:" \n')
        f.write('ls -ltr \n')
        f.write('cp -p -v -u *.top ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p -v -u *.dat ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p -v -u *.log ' + rootfolder + '/' + folderName + '/. \n')
        f.write('exit 0 \n')

        f.close()

        os.system('chmod 755 '+filename)

    if QUEUE == 'none':
        print 'Direct running... #'+str(i)+' \n'
        os.system('cd '+rootfolder+'/'+folderName)

        for i in range (0, njobs) :
            jobID = jobtag + '_' + str(i)
            os.system('bash run_'+jobID+'.sh &')

    else:
        print 'Submitting to condor queues:  \n'
        condorfile = prepareCondorScript(jobtag, 'multiple', args.folderName, QUEUE, njobs=njobs, runInBatchDir=True, slc6=args.slc6)
        runCommand ('condor_submit ' + condorfile)

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
def runSingleXgrid(parstage, xgrid, folderName, nEvents, powInputName, seed, process, scriptName) :

    print 'Running single job for grid'

    inputName = folderName + "/powheg.input"

    sedcommand = 'sed "s/NEVENTS/' + nEvents + '/ ; s/SEED/' + seed + '/" ' + powInputName + ' > ' + folderName + '/powheg.input'

    runCommand(sedcommand)

    filename = scriptName

    f = open(filename, 'a')
    f.write('cd '+rootfolder+'/'+folderName+'/ \n')

    f.write('export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:`pwd`/obj-gfortran/proclib/:${LD_LIBRARY_PATH} \n\n')

    f.write('sed -i "s/NEVENTS/'+nEvents+'/ ; s/SEED/'+seed+'/" powheg.input\n\n')

    if process == 'gg_H_MSSM' :
        if os.path.exists(powInputName) :
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/powheg-fh.in . \n')
        else :
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/powheg-fh.in \n')

    if process == 'gg_H_2HDM' :
        if os.path.exists(powInputName) :
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/br.a3_2HDM . \n')
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/br.l3_2HDM . \n')
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/br.h3_2HDM . \n')
        else :
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/br.a3_2HDM \n')
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/br.h3_2HDM \n')
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/br.l3_2HDM \n')

    if process == 'VBF_HJJJ' :
        if os.path.exists(powInputName) :
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/vbfnlo.input . \n')
        else :
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/vbfnlo.input \n')

    m_ncall2 = 500000
    if process == 'ttH' :
        for line in open(inputName) :
            if 'ncall2' in line :
                m_ncall2 = line.split(" ")[2]
                print "The original ncall2 is :", m_ncall2

        f.write('sed -i "s/ncall2.*/ncall2 0/g" powheg.input \n')
        f.write('sed -i "s/fakevirt.*/fakevirt 1  ! number of calls for computing the integral and finding upper bound/g" powheg.input \n')

    f.write('./pwhg_main \n')

    if process == 'ttH' :
        f.write('sed -i "s/ncall2.*/ncall2 '+m_ncall2+'  ! number of calls for computing the integral and finding upper bound/g" powheg.input \n')
        f.write('sed -i "s/fakevirt.*/fakevirt 0/g" powheg.input \n')
        f.write('./pwhg_main \n')

    f.write('echo -e "\\nEnd of job on " `date` "\\n" \n\n')
    f.close()

    os.system('chmod 755 '+filename)


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
def runGetSource(parstage, xgrid, folderName, powInputName, process, noPdfCheck, tagName, svnRev) :
    # parstage, xgrid are strings!

    print 'Getting and compiling POWHEG source...'

    #prepareJob(tagName, '', '.',args.prcName)

    filename = './run_%s.sh' % tagName

    template_dict = {
        "folderName" : folderName,
        "powInputName" : powInputName,
        "processtemp" : process,
        "noPdfCheck" : noPdfCheck,
        "rootfolder" : rootfolder,
        "patches_dir" : os.path.dirname(os.path.realpath(__file__)) + "/patches",
        "patch_0" : helpers.runGetSource_patch_0(process),
        "patch_1" : helpers.runGetSource_patch_1(process),
        "patch_2" : helpers.runGetSource_patch_2(process),
        "patch_3" : helpers.runGetSource_patch_3(process),
        "patch_4" : helpers.runGetSource_patch_4(process),
        "patch_5" : helpers.runGetSource_patch_5(process),
        "patch_6" : helpers.runGetSource_patch_6(process),
        "patch_7" : helpers.runGetSource_patch_7(process),
        "patch_8" : helpers.runGetSource_patch_8(process),
    }

    fourFlavorProcesses = ["ST_tch_4f", "bbH", "Wbb_dec", "Wbbj", "WWJ", "ZZJ", "Zgam", "ZgamJ", "VV_dec_ew"]
    template_dict["isFiveFlavor"] = int(process not in fourFlavorProcesses)
    template_dict["defaultPDF"] = 325300 if template_dict["isFiveFlavor"] else 325500

    powhegResProcesses = ["b_bbar_4l", "HWJ_ew", "HW_ew", "HZJ_ew", "HZ_ew", "vbs-ssww-nloew", "WWJ", "ZZJ", "HJJ_ew", "LQ-s-chan", "gg4l", "Zgam", "ZgamJ", "VV_dec_ew"]
    if process in powhegResProcesses:
        template_dict["powhegSrc"] = POWHEGRES_SOURCE
        template_dict["svnRepo"] = "svn://powhegbox.mib.infn.it/trunk/POWHEG-BOX-RES"
        template_dict["svnProc"] = "svn://powhegbox.mib.infn.it/trunk/User-Processes-RES"
        template_dict["svnRev"] = svnRev
    else:
        template_dict["powhegSrc"] = POWHEG_SOURCE
        template_dict["svnRepo"] = "svn://powhegbox.mib.infn.it/trunk/POWHEG-BOX-V2"
        template_dict["svnProc"] = "svn://powhegbox.mib.infn.it/trunk/User-Processes-V2"
        template_dict["svnRev"] = svnRev

    template_file = "%s/Templates/runGetSource_template.sh" % rootfolder
    helpers.fillTemplatedFile(template_file, filename, template_dict)
    os.chmod(filename, 0o755)

    return

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def runEvents(parstage, folderName, EOSfolder, njobs, powInputName, jobtag, process, seed) :
    print 'run : submitting jobs'
    inputName = folderName + "/powheg.input"

    sedcommand = 'sed -i "s/NEVENTS/2000/ ; s/iseed.*/iseed '+str(seed)+'/" '+inputName
    runCommand(sedcommand)

    if (parstage in ['2', '3']) :

        sedcommand = 'sed -i "s/#manyseeds/manyseeds/ ; s/.*parallelstage.*/parallelstage ' + parstage + '/ ; s/.*xgriditeration.*/xgriditeration 1/ ; s/.*manyseeds.*/manyseeds 1/ " '+inputName
        runCommand(sedcommand)

        if not 'parallelstage' in open(inputName).read() :
            runCommand("echo \'\n\nparallelstage "+parstage+"\' >> "+inputName)
        if not 'xgriditeration' in open(inputName).read() :
            runCommand("echo \'xgriditeration 1\' >> "+inputName)

        if not 'manyseeds' in open(inputName).read() :
            runCommand("echo \'manyseeds 1\' >> "+ inputName)

    runCommand('cp -p ' + folderName + '/powheg.input ' + folderName + '/powheg.input.' + parstage)

    with open(os.path.join(folderName, "pwgseeds.dat"), "w") as f:
        for i in xrange(njobs):
            f.write(str(int(seed) + i)+"\n")

    for i in range (0, njobs) :
        tag = jobtag + '_' + str (i)
        # real run
        if parstage == '4' : jobname = prepareJobForEvents(tag, i, folderName, EOSfolder)
        else               : jobname = prepareJob(tag, i, folderName,args.prcName)
        jobID = jobtag + '_' + str (i)

        filename = folderName+'/run_' + tag + '.sh'
        f = open (filename, 'a')
        #f.write('cd '+rootfolder+'/'+folderName+'/ \n')
        if process == 'WWJ' :
          f.write('echo \"Copy TwoLoops grids\"\n')
          f.write('ls\n') 
          f.write('wget https://wwwth.mpp.mpg.de/members/wieseman/download/codes/WW_MiNNLO/VVamp_interpolation_grids/WW_MiNNLO_2loop_grids_reduced1.tar.gz\n')
          f.write('tar xzf WW_MiNNLO_2loop_grids_reduced1.tar.gz\n')
          f.write('ls\n')
        f.write('cp -p ' + rootfolder + '/' + folderName + '/powheg.input.' + parstage + ' ./powheg.input' + '\n') # copy input file for this stage explicitly, needed by condor dag
        f.write('echo ' + str (i+1) + ' | ./pwhg_main \n')
        f.write('echo "Workdir after run:" \n')
        f.write('ls -ltr \n')
        f.write('cp -p -v -u *.top ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p -v -u *.dat ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p -v -u *.log ' + rootfolder + '/' + folderName + '/. \n')
        f.write('exit 0 \n')
        f.close()

        os.system('chmod 755 '+filename)

    if QUEUE == 'none':
        print 'Direct running... #'+str(i)+' \n'
        os.system('cd '+rootfolder+'/'+folderName)

        for i in range (0, njobs) :
            jobID = jobtag + '_' + str(i)
            os.system('bash run_'+jobID+'.sh &')

    else:
        print 'Submitting to condor queues:  \n'
        condorfile = prepareCondorScript(jobtag, 'multiple', args.folderName, QUEUE, njobs=njobs, runInBatchDir=True, slc6=args.slc6) 
        runCommand ('condor_submit ' + condorfile)
     

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

def createTarBall(parstage, folderName, prcName, keepTop, seed, scriptName) :
    print 'Creating tarball distribution for '+args.folderName+'_'+prcName+'.tgz'
    print

    #inputName = folderName + "/powheg.input"

    filename = scriptName

    #if filename = "" :
    #    f = subprocess.Popen(['/bin/sh', '-c'])

    template_dict = {
        "folderName" : folderName,
        "process" : prcName,
        "keepTop" : keepTop,
        "rootfolder" : rootfolder,
        "seed" : seed,
        "exclude_extra" : "exclude_extra",
    }

    template_file = "%s/Templates/createTarBall_template.sh" % rootfolder
    helpers.fillTemplatedFile(template_file, filename, template_dict)

    os.chmod(filename, 0o755)

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def runhnnlo(folderName, njobs, QUEUE):

  template_dict = {
      "rootfolder" : rootfolder,
      "folderName" : folderName,
  }

  scales = ["11", "22", "0505"]
  for scale in scales:
    os.system('rm -rf '+ folderName+"/"+scale)
    os.system('mkdir -p '+ folderName+"/"+scale)
    filename = folderName+"/"+scale+"/launch_NNLO.sh"

    template_dict["scale"] = scale

    template_file = "%s/Templates/runhnnlo_template.sh" % rootfolder
    helpers.fillTemplatedFile(template_file, filename, template_dict, "w")
    os.chmod(filename, 0o755)

    print 'Submitting to condor queues \n'
    tagName = 'hnnlo_%s' % scale
    condorfile = prepareCondorScript(tagName, 'hnnlo', folderName, QUEUE, njobs=njobs, runInBatchDir=scale, slc6=args.slc6) 
    runCommand ('condor_submit ' + condorfile)
   



# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


if __name__ == "__main__":

    eoscmd = '/afs/cern.ch/project/eos/installation/cms/bin/eos.select' ;

    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--parstage'      , dest="parstage",      default= '0',            help='stage of the production process [0]')
    parser.add_argument('-x', '--xgrid'         , dest="xgrid",         default= '1',            help='loop number for the grids production [1]')
    parser.add_argument('-f', '--folderName'    , dest="folderName",    default='testProd',      help='local folder and last eos folder name[testProd]')
    parser.add_argument('-e', '--eosFolder'     , dest="eosFolder",     default='NONE' ,         help='folder before the last one, on EOS')
    parser.add_argument('-j', '--numJobs'       , dest="numJobs",       default= '10',           help='number of jobs to be used for multicore grid step 1,2,3')
    parser.add_argument('-t', '--totEvents'     , dest="totEvents",     default= '10000',        help='total number of events to be generated [10000]')
    parser.add_argument('-n', '--numEvents'     , dest="numEvents",     default= '2000',         help='number of events for a single job [2000]')
    parser.add_argument('-i', '--inputTemplate' , dest="inputTemplate", default= 'powheg.input', help='input cfg file (fixed) [=powheg.input]')
    parser.add_argument('-g', '--inputJHUGen' , dest="inputJHUGen", default= '', help='input JHUGen cfg file []')
    parser.add_argument('-q', '--doQueue'      , dest="doQueue",      default= 'none',          help='Jobflavour: if none, running interactively [none]')
    parser.add_argument('-s', '--rndSeed'       , dest="rndSeed",       default= '42',           help='Starting random number seed [42]')
    parser.add_argument('-m', '--prcName'       , dest="prcName",       default= 'DMGG',           help='POWHEG process name [DMGG]')
    parser.add_argument('-k', '--keepTop'       , dest="keepTop",       default= '0',           help='Keep the validation top draw plots [0]')
    parser.add_argument('-d', '--noPdfCheck'    , dest="noPdfCheck",    default= '0',           help='If 1, deactivate automatic PDF check [0]')
    parser.add_argument('--fordag'    , dest="fordag",    default= 0,           help='If 1, deactivate submission, expect condor DAG file to be created [0]')
    parser.add_argument('--slc6'    , dest="slc6",    default= 0,           help='If 1, use slc6 singularity [0]')
    parser.add_argument('--svn'    , dest="svnRev",    default= 0,           help='SVN revision. If 0, use tarball [0]')

    args = parser.parse_args ()

    message2 = "After step 0, you must input the process name _without_ the slash (e.g. HJ/MiNNLOPS must be just HJ)"

    if args.parstage != '0' and '/' in args.prcName:
        raise RuntimeError(message2)

    QUEUE = args.doQueue
    EOSfolder = args.folderName

    print
    print 'RUNNING PARAMS: parstage = ' + args.parstage + ' , xgrid = ' + args.xgrid  + ' , folderName = ' + args.folderName
    print '                Total Events = ' + args.totEvents
    print '                Number of Events = ' + args.numEvents
    print '                powheg input cfg file : ' + args.inputTemplate
    print '                powheg process name : ' + args.prcName
    print '                working folder : ' + args.folderName
    print '                EOS folder (stages 4,7,8) : ' + args.eosFolder + '/' + EOSfolder
    print '                base folder : ' + rootfolder
    print '                forDAG : ' + str(args.fordag)
    print '                SLC6 : ' + str(args.slc6)
    print '                SVN : ' + str(args.svnRev)
    print

    if (TESTING == 1) :
        print '  --- TESTING, NO submissions will happen ---  '
        print

    if (args.fordag) :
        print '  --- Submissions will be done by DAG ---  '
        print

    res = os.path.exists(rootfolder+'/'+args.folderName)

    ### agrohsje still need an old version for ST_tch_4f; informed ER, PN end 2021 but no fix provided yet, pinged again (today: 27.6.2022)
    if args.prcName == "ST_tch_4f":
        POWHEG_SOURCE="powhegboxV2_rev3624_date20190117.tar.gz" 
        
    if args.parstage == '1' and args.xgrid == '1' and (not res) :
        print 'Creating working folder ' + args.folderName + '...'
        # Assuming the generator binaries are in the current folder.
        os.system('mkdir '+rootfolder+'/'+args.folderName)
        if os.path.exists(rootfolder+'/pwhg_main') :
            print 'Copy pwhg_main'
            os.system('cp -p pwhg_main '+args.folderName+'/.')

        if os.path.exists(rootfolder+'JHUGen') :
            print 'Copy JHUGen'
            os.system('cp -p JHUGen '+args.folderName+'/.')

    if args.parstage == '1' and args.xgrid == '1' :
        if not os.path.exists(args.folderName) :
            print 'Creating working folder ' + args.folderName + '...'
            # Assuming the generator binaries are in the current folder.
            os.system('mkdir '+args.folderName)
            if os.path.exists('pwhg_main') :
                os.system('cp -p pwhg_main '+args.folderName+'/.')

            if os.path.exists('JHUGen') :
                os.system('cp -p JHUGen '+args.folderName+'/.')

        if not os.path.exists(args.inputTemplate) :
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate+' -O '+args.folderName+'/powheg.input')
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate)

            os.system('sed -i "s/^numevts.*/numevts '+args.numEvents+'/" '+
                      args.folderName+'/powheg.input')

        if not os.path.exists(args.folderName+'/powheg.input') :
            os.system('cp -p '+args.inputTemplate+' '+
                      args.folderName+'/powheg.input')
            os.system('sed -i "s/^numevts.*/numevts '+args.numEvents+'/" '+
                      args.folderName+'/powheg.input')

        if not os.path.exists(args.folderName + '/pwgseeds.dat') :
            fseed = open(args.folderName + '/pwgseeds.dat', 'w')
            for ii in range(1, 10000) :
                fseed.write(str(ii)+'\n')
            fseed.close()

    if args.parstage == '4' :
        runCommand (eoscmd + ' mkdir /eos/cms/store/user/${user}/LHE/powheg/' + args.eosFolder, 1, 1)
        runCommand (eoscmd + ' mkdir /eos/cms/store/user/${user}/LHE/powheg/' + args.eosFolder + '/' + EOSfolder, 1, 1)

    njobs = int (args.numJobs)

    powInputName = args.inputTemplate
    jobtag = args.parstage + '_' + args.xgrid
    # different tag for stage3 pilot run
    if args.parstage == '3' and njobs == 1:
        jobtag = jobtag+'_pilot'

    if args.parstage == '0' or \
       args.parstage == '0123' or args.parstage == 'a' or \
       args.parstage == '01239' or args.parstage == 'f' : # full single grid in oneshot

        tagName = 'src_'+args.folderName
        filename = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.',args.prcName)

        if not os.path.exists(args.inputTemplate) :
            m_ret = os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate+' -O '+args.folderName+'/powheg.input')

            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate)

        os.system('mkdir -p '+rootfolder+'/'+args.folderName)

        if not os.path.exists(args.inputTemplate) :
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate+' -O '+args.folderName+'/powheg.input')
        else :
            os.system('cp -p '+args.inputTemplate+' '+args.folderName+'/powheg.input')

        os.system('rm -rf JHUGen.input')
        inputJHUGen = args.inputJHUGen
        if args.inputJHUGen == "":
            inputJHUGen = '/'.join(powInputName.split('/')[0:-1])+'/JHUGen.input'

        if not os.path.exists(inputJHUGen) :
            m_ret = os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+inputJHUGen+' -O '+args.folderName+'/JHUGen.input')
            if ((m_ret>>8) & 255) != 0 :
                os.system('rm -rf '+args.folderName+'/JHUGen.input')

        else :
            os.system('cp -p '+inputJHUGen+' '+args.folderName+'/JHUGen.input')

        if os.path.exists(args.folderName+'/powheg.input') :
            test_pdf1 = 0
            test_pdf2 = 0

            default_pdf = "325300"  # for 5 flavours

            if args.prcName=="ST_tch_4f" or args.prcName=="bbH" or args.prcName=="Wbb_dec" or args.prcName=="Wbbj" or args.prcName=="WWJ" or args.prcName=="ZZJ" or args.prcName=="Zgam" or args.prcName=="ZgamJ" or args.prcName=="VV_dec_ew":
                default_pdf = "325500"  # for 4 flavours

            for line in open(args.folderName+'/powheg.input') :
                n_column = line.split()
                if 'lhans1' in line and len(n_column) >= 2:
                    test_pdf1 = n_column[1].strip()
                if 'lhans2' in line and len(n_column) >= 2:
                    test_pdf2 = n_column[1].strip()

            if not (test_pdf1 == test_pdf2) :
                raise RuntimeError("ERROR: PDF settings not equal for the 2 protons: {0} vs {1}... Please check your datacard".format(test_pdf1, test_pdf2))

            if test_pdf1 != default_pdf :
#                print "PDF in card: ", test_pdf1, "PDF default: ", default_pdf, test_pdf1==default_pdf
                message = "The input card does not have the standard Ultralegacy PDF (NNPDF31 NNLO, 325300 for 5F, 325500 for 4F): {0}. Either change the card or run again with -d 1 to ignore this message.\n".format(test_pdf1)

                if args.noPdfCheck == '0' :
                    raise RuntimeError(message)
                else:
                    print "WARNING:", message
                    print "FORCING A DIFFERENT PDF SET FOR CENTRAL VALUE\n"

    if args.parstage == '0' :

        tagName = 'src_'+args.folderName
        filename = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.',args.prcName)

        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, args.noPdfCheck, tagName, args.svnRev)

        if QUEUE == 'none':
            print 'Direct compiling... \n'
            os.system('bash '+filename+' 2>&1 | tee '+filename.split('.sh')[0]+'.log')

        else:
            print 'Submitting to condor queues \n'
            condorfile = prepareCondorScript(tagName, '', '.', QUEUE, njobs=1, slc6=args.slc6) 
            runCommand ('condor_submit ' + condorfile)

    elif args.parstage == '1' :
        runParallelXgrid(args.parstage, args.xgrid, args.folderName,
                         args.numEvents, njobs, powInputName, jobtag,
                         args.rndSeed, args.prcName)

    elif args.parstage == '123' or args.parstage == 's' : # single grid proc
        tagName = 'grid_'+args.folderName
        scriptName = args.folderName + '/run_'+tagName+'.sh'


        os.system('cp -p '+args.inputTemplate+' '+args.folderName+'/powheg.input')
        os.system('sed -i "s/^numevts.*/numevts '+args.totEvents+'/" '+
                  args.folderName+'/powheg.input')

        prepareJob(tagName, '', args.folderName,args.prcName)
        runSingleXgrid(args.parstage, args.xgrid, args.folderName,
                       args.numEvents, powInputName, args.rndSeed,
                       args.prcName, scriptName)

        if QUEUE == 'none':
            print 'Direct running single grid... \n'
            os.system('bash '+scriptName+' >& '+scriptName.split('.sh')[0]+'.log &')

        else:
            print 'Submitting to condor queues  \n'
            condorfile = prepareCondorScript(tagName, '', args.folderName, QUEUE, njobs=1, runInBatchDir=True, slc6=args.slc6) 
            runCommand ('condor_submit ' + condorfile)

    elif args.parstage == '0123' or args.parstage == 'a' : # compile & run
        tagName = 'all_'+args.folderName
        scriptName = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.',args.prcName)
        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, args.noPdfCheck, tagName, args.svnRev)

        os.system('sed -i "s/^numevts.*/numevts '+args.numEvents+'/" '+
                  args.folderName+'/powheg.input')
        runSingleXgrid(args.parstage, args.xgrid, args.folderName,
                       args.numEvents, powInputName, args.rndSeed,
                       args.prcName, scriptName)

        if QUEUE == 'none':
            print 'Direct compiling and running... \n'
            os.system('bash '+scriptName+' >& '+
                      scriptName.split('.sh')[0]+'.log &')
        else:
            print 'Submitting to condor queues  \n'
            condorfile = prepareCondorScript(tagName, '', '.', QUEUE, njobs=1, runInBatchDir=True, slc6=args.slc6) 
            runCommand ('condor_submit ' + condorfile)

    elif args.parstage == '01239' or args.parstage == 'f' : # full single grid in oneshot
        tagName = 'full_'+args.folderName
        scriptName = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.',args.prcName)
        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, args.noPdfCheck, tagName, args.svnRev)

        runSingleXgrid(args.parstage, args.xgrid, args.folderName,
                       args.numEvents, powInputName, args.rndSeed,
                       args.prcName, scriptName)

        createTarBall(args.parstage, args.folderName, args.prcName,
                      args.keepTop, args.rndSeed, scriptName)

        if QUEUE == 'none':
            print 'Direct running in one shot... \n'
            os.system('bash '+scriptName+' | tee '+
                      scriptName.split('.sh')[0]+'.log')
        else:
            print 'Submitting to condor queues  \n'
            condorfile = prepareCondorScript(tagName, '', '.', QUEUE, njobs=1, runInBatchDir=True, slc6=args.slc6) 
            runCommand ('condor_submit ' + condorfile)

    elif args.parstage == '7' :
        print "preparing for NNLO reweighting"
        if args.prcName == "HJ":
            runhnnlo(args.folderName, njobs, QUEUE)

    elif args.parstage == '9' :
        # overwriting with original
        scriptName = './run_tar_'+args.folderName+'.sh'

        os.system('rm -rf '+scriptName)

        createTarBall(args.parstage, args.folderName, args.prcName,
                      args.keepTop, args.rndSeed, scriptName)

        os.system('cd '+rootfolder+';bash '+scriptName)

    else                    :
        os.system('cp -p '+args.inputTemplate+' '+args.folderName+'/powheg.input')
        runEvents(args.parstage, args.folderName,
                  args.eosFolder + '/' + EOSfolder, njobs, powInputName,
                  jobtag, args.prcName, args.rndSeed)
