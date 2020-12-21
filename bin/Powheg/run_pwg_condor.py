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

POWHEG_SOURCE = "powhegboxV2_rev3728_date20200429.tar.gz"
POWHEGRES_SOURCE = "powhegboxRES_rev3748_date20200615.tar.gz"

rootfolder = os.getcwd()


def runCommand(command, printIt = False, doIt = 1, TESTING = 0) :
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

def prepareCondorScript( tag, i, folderName, queue, SCALE = '0', runInBatchDir = False):
   '''prepare the Condor submission script'''

   filename = 'run_' + folderName + '_' + tag + '.condorConf'
   execname = folderName + '/run_' + tag
   logname =  'run_' + tag
   if runInBatchDir:
       folderName = rootfolder + '/' + folderName
       execname = rootfolder + '/' + execname
       logname =  folderName + '/run_' + tag
   f = open(filename, 'w')
  
   if (i == 'multiple') :
       f.write('executable              = ' + execname + '_$(ProcId).sh \n')
   elif (i == 'hnnlo') :
       f.write('executable              = ' + folderName + '/' + SCALE + '/' + 'launch_NNLO.sh \n')
       f.write('arguments               = HNNLO-LHC13-R04-APX2-' + SCALE + '.input $(ClusterId)$(ProcId) \n')
   elif (i == 'dynnlo') :
       f.write('executable              = ' + folderName + '/' +  SCALE + '/' + 'launch_NNLO.sh \n')
       f.write('arguments               = ' + SCALE + '.input $(ClusterId)$(ProcId) \n')
   elif (i == 'minlo') :
       f.write('executable              = ' + folderName + '/minlo-run/launch_minlo.sh \n')
       f.write('arguments               = ' + 'powheg.input $(ClusterId)$(ProcId) \n')
   else :
       f.write('executable              = ' + execname + '.sh \n')
   f.write('output                  = ' + logname + '_$(ProcId).out \n')
   f.write('error                   = ' + logname + '_$(ProcId).err \n')
   f.write('log                     = ' + logname + '.log \n')
   if not runInBatchDir:
       f.write('initialdir              = ' + rootfolder + '/' + folderName + '\n')

   f.write('+JobFlavour             = "'+ queue +'" \n') 

   f.write('periodic_remove         = JobStatus == 5  \n')
   f.write('WhenToTransferOutput    = ON_EXIT_OR_EVICT \n')
   f.write('transfer_output_files   = "" \n')
 
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
       runCommand('source mergeCondorConf.sh')

   return filename


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

def prepareJob(tag, i, folderName) :
    filename = folderName+'/run_%s.sh' % tag

    template_dict = {
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

    prepareJob(tag, i, folderName)

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

        if not 'fakevirt' in open(inputName).read() :
            if process != 'b_bbar_4l':
                runCommand("echo \'fakevirt 1\' >> "+inputName)

    runCommand('cp -p '+inputName+' '+inputName+'.'+parstage+'_'+str(xgrid))

    for i in range (0, njobs) :
        jobID = jobtag + '_' + str(i)
        jobname = prepareJob(jobID, i, folderName)

        filename = folderName+'/run_' + jobID + '.sh'
        f = open(filename, 'a')
        #f.write('cd '+rootfolder+'/'+folderName+'/ \n')
        f.write('echo ' + str(i+1) + ' | ./pwhg_main &> run_' + jobID + '.log ' + '\n')
        f.write('cp -p *.top ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p *.dat ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p *.log ' + rootfolder + '/' + folderName + '/. \n')
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
        condorfile = prepareCondorScript(jobtag, 'multiple', args.folderName, QUEUE, runInBatchDir=True) 
        runCommand ('condor_submit ' + condorfile + ' -queue '+ str(njobs), TESTING == 0)


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
def runGetSource(parstage, xgrid, folderName, powInputName, process, noPdfCheck, tagName) :
    # parstage, xgrid are strings!

    print 'Getting and compiling POWHEG source...'

    #prepareJob(tagName, '', '.')

    filename = './run_%s.sh' % tagName

    template_dict = {
        "folderName" : folderName,
        "powInputName" : powInputName,
        "process" : process,
        "noPdfCheck" : noPdfCheck,
        "rootfolder" : rootfolder,
        "patches_dir" : os.path.dirname(os.path.realpath(__file__)) + "/patches",
        "patch_1" : helpers.runGetSource_patch_1(process),
        "patch_2" : helpers.runGetSource_patch_2(process),
        "patch_3" : helpers.runGetSource_patch_3(process),
        "patch_4" : helpers.runGetSource_patch_4(process),
        "patch_5" : helpers.runGetSource_patch_5(process),
        "patch_6" : helpers.runGetSource_patch_6(process),
        "patch_7" : helpers.runGetSource_patch_7(process),
        "patch_8" : helpers.runGetSource_patch_8(process),
    }

    fourFlavorProcesses = ["ST_tch_4f", "bbH", "Wbb_dec", "Wbbj", "WWJ"]
    template_dict["isFiveFlavor"] = int(process not in fourFlavorProcesses)
    template_dict["defaultPDF"] = 325300 if template_dict["isFiveFlavor"] else 325500

    DYNNLOPS = ["Zj", "Wj"]
    if process in DYNNLOPS:
        template_dict["forDYNNLOPS"] = 1
    else:
        template_dict["forDYNNLOPS"] = 0

    powhegResProcesses = ["b_bbar_4l", "HWJ_ew", "HW_ew", "HZJ_ew", "HZ_ew", "vbs-ssww-nloew"]
    if process in powhegResProcesses:
        template_dict["powhegSrc"] = POWHEGRES_SOURCE
    else:
        template_dict["powhegSrc"] = POWHEG_SOURCE

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
        else               : jobname = prepareJob(tag, i, folderName)
        jobID = jobtag + '_' + str (i)
    
        filename = folderName+'/run_' + tag + '.sh'
        f = open (filename, 'a')
        #f.write('cd '+rootfolder+'/'+folderName+'/ \n')
        f.write('echo ' + str (i) + ' | ./pwhg_main &> run_' + tag + '.log ' + '\n')
        f.write('cp -p *.top ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p *.dat ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p *.log ' + rootfolder + '/' + folderName + '/. \n')
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
        condorfile = prepareCondorScript(jobtag, 'multiple', args.folderName, QUEUE, runInBatchDir=True) 
        runCommand ('condor_submit ' + condorfile + ' -queue '+ str(njobs), TESTING == 0)
     

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
    }

    template_file = "%s/Templates/createTarBall_template.sh" % rootfolder
    helpers.fillTemplatedFile(template_file, filename, template_dict)
    
    if prcName in ['Zj', 'Wj']:
        if os.path.isfile(folderName + '/DYNNLO_mur1_muf1_3D.top'):
            make_nnlo_rwl(folderName)

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
    condorfile = prepareCondorScript(tagName, 'hnnlo', folderName, QUEUE, scale) 
    runCommand ('condor_submit ' + condorfile + ' -queue '+ str(njobs), TESTING == 0)
   


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def makedynnloconfig(folderName, baseconfig, config, murfac, muffac):
    with open(folderName+'/'+baseconfig, 'r') as infile:
        with open(folderName+'/'+config, 'w') as outfile:
            for line in infile:
                if 'mur' in line:
                    mass = float(line.split()[0])
                    mur = mass*murfac
                    muf = mass*muffac
                    newline = '%s %s  ! mur, muf\n' % (str(mur), str(muf))
                    outfile.write(newline)
                else:
                    outfile.write(line)


def rundynnlo(folderName, njobs, QUEUE):

    template_dict = {
        "rootfolder" : rootfolder,
        "folderName" : folderName,
    }

    scales = ["1", "2", "0.5"]
    baseconfig = "DYNNLO.input"
    for mur in scales:
        for muf in scales:
            config = "DYNNLO_mur%s_muf%s.input" % (mur, muf)
            makedynnloconfig(folderName, baseconfig, config, float(mur), float(muf))
            subfolderName = "dynnlo_mur%s_muf%s" % (mur, muf)
            template_dict["subfolderName"] = subfolderName

            os.system('mkdir -p ' + folderName + "/" + subfolderName)
            filename = folderName+"/"+subfolderName+"/launch_NNLO.sh"

            template_file = "%s/Templates/rundynnlo_template.sh" % rootfolder
            helpers.fillTemplatedFile(template_file, filename, template_dict, "w")
            os.chmod(filename, 0o755)

            print 'Submitting to condor queues \n'
            condorfile = prepareCondorScript(subfolderName, 'dynnlo', folderName, QUEUE, subfolderName, runInBatchDir=True) 
            runCommand ('condor_submit ' + condorfile + ' -queue '+ str(njobs), TESTING == 0)


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def mergedynnlo(folderName, process):
    folderName = folderName + '/'
    from numpy import median
    # Just recompile this in the default environment
    runCommand ('cd %s; gfortran -o merge3ddata POWHEG-BOX/%s/DYNNLOPS/aux/merge3ddata.f' % (folderName, process), printIt = True)
    # Do the work
    scales = ["1", "2", "0.5"]
    for mur in scales:
        for muf in scales:
            path = 'dynnlo_mur%s_muf%s/' % (mur, muf)
            files = [path + f for f in os.listdir(folderName+path) if '3D.top' in f]
            sizes = []
            for f in files:
                sizes.append(os.path.getsize(folderName+'/'+f))
            medianSize = median(sizes)
            selfiles = [path + f for f in os.listdir(folderName+path) if '3D.top' in f and os.path.getsize(folderName + path + f) == medianSize]
            print('Using %i files with size = %i, %i files discarded' % (len(selfiles), medianSize, len(files)-len(selfiles)))
            runCommand ('cd %s; ./merge3ddata 1 ' % folderName + ' '.join(selfiles), printIt = True)
            runCommand ('cd %s; mv fort.12 ' % folderName + 'DYNNLO_mur%s_muf%s_3D.top' % (mur, muf), printIt = True)
            
    #runCommand ('cd %s; for dir in dynnlo_*/; do echo "Removing empty files"; find ${dir} -name "*3D.top" -size  0 -print -delete; echo "Merging files..."; ./merge3ddata 1 ${dir\%?}/*3D.top; mv fort.12 ${dir\%?}_3D.top; done' % (folderName), printIt = True)

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def dynnlops_runminlo(folderName, njobs, QUEUE, eosdir):
    if 'NONE' in eosdir:
        print('WARNING: using workdir for output files, you may run out of disk space')
        eosdir = os.getcwd()+"/"+folderName
    
    os.system('rm -rf ' + folderName + "/minlo-run")
    os.system('mkdir -p ' + folderName + "/minlo-run")
    os.system('rm -rf ' + eosdir + "/minlo-run")
    os.system('mkdir -p ' + eosdir + "/minlo-run")
    
    m_outfile = folderName + '/pwg-rwl-scalesonly.dat'
    m_factor = ['1d0', '2d0', '0.5d0']
    m_idx = 1001
    fout = open(m_outfile, 'w')
    fout.write("<initrwgt>\n")
    fout.write("<weightgroup name='scale_variation' combine='envelope' >\n")
    for m_rensc in m_factor :
      for m_facsc in m_factor :
        fout.write("<weight id='"+str(m_idx)+"'> renscfact=" + m_rensc + " facscfact=" + m_facsc + " </weight>\n")
        m_idx = m_idx + 1
    fout.write("</weightgroup>\n")
    fout.write("</initrwgt>\n")
    fout.close()

    filename = folderName+"/minlo-run/launch_minlo.sh"

    template_dict = {
        "rootfolder" : rootfolder,
        "folderName" : folderName,
        "eosdir" : eosdir,
    }

    template_file = "%s/Templates/dynnlops_runminlo_template.sh" % rootfolder
    helpers.fillTemplatedFile(template_file, filename, template_dict, "w")
    os.chmod(filename, 0o755)
    
    print 'Submitting to condor queues \n'
    condorfile = prepareCondorScript(folderName + '_minlo', 'minlo', folderName, QUEUE, runInBatchDir=True) 
    runCommand ('condor_submit ' + condorfile + ' -queue '+ str(njobs), TESTING == 0)


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def dynnlops_mergeminlo(folderName, process, eosdir):
    from numpy import median
    # Just recompile this in the default environment
    runCommand ('cd %s; gfortran -o merge3ddata POWHEG-BOX/%s/DYNNLOPS/aux/merge3ddata.f' % (folderName, process), printIt = True)
    # Do the work
    import getpass
    user = getpass.getuser()
    runCommand ('mkdir -p /tmp/%s/%s' % (user, folderName), printIt = True)
    jobdirs = os.listdir(eosdir + '/minlo-run/')
    # loop over 9 mur+muf combinations
    for w in range(1, 10):
        files = []
        for jd in jobdirs:
            filename = eosdir + '/minlo-run/' + jd + '/MINLO-W%i-denom.top' % w
            if os.path.isfile(filename):
                files.append(filename)
        sizes = []
        for f in files:
            sizes.append(os.path.getsize(f))
        medianSize = median(sizes)
        selfiles = []
        for f in files:
            if os.path.getsize(f) == medianSize:
                selfiles.append(f)
        print('Using %i files with size = %i, %i files discarded' % (len(selfiles), medianSize, len(files)-len(selfiles)))
        # split into chunks of 10 files
        n = 10
        chunks = [selfiles[i:i + n] for i in xrange(0, len(selfiles), n)]
        # merge
        temps = []
        for c in range(len(chunks)):
            runCommand ('cd %s; ./merge3ddata 1 ' % folderName + ' '.join(chunks[c]), printIt = True)
            if os.path.isfile(folderName + '/fort.12'):
                temps.append('/tmp/%s/%s/MINLO-W%i-%i-denom.top' % (user, folderName, w, c))
                runCommand ('cd %s; mv fort.12 ' % folderName + temps[-1], printIt = True)
        runCommand ('cd %s; ./merge3ddata 1 ' % folderName + ' '.join(temps), printIt = True)
        runCommand ('cd %s; mv fort.12 ' % folderName + 'MINLO-W%i-denom.top' % w, printIt = True)


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def make_nnlo_rwl(folderName):
    # check pwg-rwl.dat
    rwlfile = folderName + '/pwg-rwl.dat'
    if 'scale_variation2' in open(rwlfile).read():
        print('Creating 9x9 NNLOxMINLO scale variations')
    else:
        exit('Please implement simplified NNLO=MINLO scale variations if you need that mode')
    nweights = open(rwlfile).read().count('weight id')
    
    # write nnlo lists
    nnlo_outfile  = folderName + '/list_nnlo.txt'
    minlo_outfile = folderName + '/list_minlo.txt'
    nnlo_fout  = open(nnlo_outfile, 'w')
    minlo_fout = open(minlo_outfile, 'w')
    
    # crossed scale variations
    scales = ["1", "2", "0.5"]
    for mur in scales:
        for muf in scales:
            nnlo = "DYNNLO_mur%s_muf%s_3D.top" % (mur, muf)
            if not os.path.isfile(folderName + '/' + nnlo):
                exit('%s is missing, exiting!' % nnlo)
            for i in range(1,10):
                minlo = 'MINLO-W%i-denom.top' % i
                if not os.path.isfile(folderName + '/' + minlo):
                    print('%s is missing, exiting!' % minlo)
                nnlo_fout.write(nnlo+'\n')
                minlo_fout.write(minlo+'\n')
                nweights -= 1
    
    # weight pdf variations with nominal ratio
    while nweights > 0:
        nnlo_fout.write("DYNNLO_mur1_muf1_3D.top\n")
        minlo_fout.write("MINLO-W1-denom.top\n")
        nweights -= 1
    
    nnlo_fout.close()
    minlo_fout.close()

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

    args = parser.parse_args ()
    
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
    print
 
    if (TESTING == 1) :     
        print '  --- TESTING, NO submissions will happen ---  '
        print

    res = os.path.exists(rootfolder+'/'+args.folderName)

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

    if args.parstage == '0' or \
       args.parstage == '0123' or args.parstage == 'a' or \
       args.parstage == '01239' or args.parstage == 'f' : # full single grid in oneshot 

        tagName = 'src_'+args.folderName
        filename = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.')

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

            if args.prcName=="ST_tch_4f" or args.prcName=="bbH" or args.prcName=="Wbb_dec" or args.prcName=="Wbbj" or args.prcName=="WWJ" :
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

        prepareJob(tagName, '', '.') 

        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, args.noPdfCheck, tagName) 

        if QUEUE == 'none':
            print 'Direct compiling... \n'
            os.system('bash '+filename+' 2>&1 | tee '+filename.split('.sh')[0]+'.log')
        
        else:
            print 'Submitting to condor queues \n'
            condorfile = prepareCondorScript(tagName, '', '.', QUEUE) 
            runCommand ('condor_submit ' + condorfile + ' -queue 1', TESTING == 0)

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

        prepareJob(tagName, '', args.folderName)
        runSingleXgrid(args.parstage, args.xgrid, args.folderName,
                       args.numEvents, powInputName, args.rndSeed,
                       args.prcName, scriptName)

        if QUEUE == 'none':
            print 'Direct running single grid... \n'
            os.system('bash '+scriptName+' >& '+scriptName.split('.sh')[0]+'.log &')
                    
        else:
            print 'Submitting to condor queues  \n'
            condorfile = prepareCondorScript(tagName, '', args.folderName, QUEUE, runInBatchDir=True) 
            runCommand ('condor_submit ' + condorfile + ' -queue 1', TESTING == 0)

    elif args.parstage == '0123' or args.parstage == 'a' : # compile & run
        tagName = 'all_'+args.folderName
        scriptName = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.')
        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, args.noPdfCheck, tagName)

        os.system('sed -i "s/^numevts.*/numevts '+args.numEvents+'/" '+
                  args.folderName+'/powheg.input')
        runSingleXgrid(args.parstage, args.xgrid, args.folderName,
                       args.numEvents, powInputName, args.rndSeed,
                       args.prcName, scriptName)

        if QUEUE == 'none':
            print 'Direct compiling and running... \n'
            #runCommand ('bash run_source.sh ', TESTING == 1)
            os.system('bash '+scriptName+' >& '+
                      scriptName.split('.sh')[0]+'.log &')
        else:
            print 'Submitting to condor queues  \n'
            condorfile = prepareCondorScript(tagName, '', '.', QUEUE, runInBatchDir=True) 
            runCommand ('condor_submit ' + condorfile + ' -queue 1', TESTING == 0)

    elif args.parstage == '01239' or args.parstage == 'f' : # full single grid in oneshot 
        tagName = 'full_'+args.folderName
        scriptName = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.')
        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, args.noPdfCheck, tagName)

        runSingleXgrid(args.parstage, args.xgrid, args.folderName,
                       args.numEvents, powInputName, args.rndSeed,
                       args.prcName, scriptName)

        createTarBall(args.parstage, args.folderName, args.prcName,
                      args.keepTop, args.rndSeed, scriptName)

        if QUEUE == 'none':
            print 'Direct running in one shot... \n'
            os.system('bash '+scriptName+' >& '+
                      scriptName.split('.sh')[0]+'.log &')
        else:
            print 'Submitting to condor queues  \n'
            condorfile = prepareCondorScript(tagName, '', '.', QUEUE, runInBatchDir=True) 
            runCommand ('condor_submit ' + condorfile + ' -queue 1', TESTING == 0)

    elif args.parstage == '7' :
        print "preparing for NNLO reweighting"
        if args.prcName == "HJ":
            runhnnlo(args.folderName, njobs, QUEUE)
        if args.prcName in ["Zj", "Wj"]:
            rundynnlo(args.folderName, njobs, QUEUE)

    elif args.parstage == '77' :
        print "merging DYNNLO files for NNLOPS"
        if args.prcName in ["Zj", "Wj"]:
            mergedynnlo(args.folderName, args.prcName)
    
    elif args.parstage == '8' :
        print "preparing MINLO files for NNLOPS"
        os.system('cp -p '+args.inputTemplate+' '+args.folderName+'/powheg.input')
        if args.prcName in ["Zj", "Wj"]:
            dynnlops_runminlo(args.folderName, njobs, QUEUE, args.eosFolder + '/' + EOSfolder)
    
    elif args.parstage == '88' :
        print "merging MINLO files for NNLOPS"
        if args.prcName in ["Zj", "Wj"]:
            dynnlops_mergeminlo(args.folderName, args.prcName, args.eosFolder + '/' + EOSfolder)
    
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
