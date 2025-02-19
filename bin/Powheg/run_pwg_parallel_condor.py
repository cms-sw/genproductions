#!/usr/bin/env python3

# EXAMPLE ON HOW TO RUN
# python ./run_pwg_parallel.py -i powheg_Zj.input -m Zj -f my_Zj -q 1nd -j 10

import argparse
import os
import sys
import subprocess
import re
import datetime
from time import sleep

class Logger(object):
    def __init__(self):
        self.terminal = sys.stdout
        self.log = open("logfile.log", "a")

    def write(self, message):
        self.terminal.write(message)
        self.log.write(message)  

    def flush(self):
        #this flush method is needed for python 3 compatibility.
        #this handles the flush command by doing nothing.
        #you might want to specify some extra behavior here.
        pass    

sys.stdout = Logger()

if __name__ == "__main__":

    eoscmd = '/afs/cern.ch/project/eos/installation/cms/bin/eos.select' ;

    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--parstage'      , dest="parstage",      default= '123',          help='stage of the production process [01239]')
    parser.add_argument('-f', '--folderName'    , dest="folderName",    default='testProd',      help='local folder and last eos folder name[testProd]')
    parser.add_argument('-e', '--eosFolder'     , dest="eosFolder",     default='NONE' ,         help='folder before the last one, on EOS')
    parser.add_argument('-j', '--numJobs'       , dest="numJobs",       default= '10',           help='number of jobs to be used for multicore grid step 1,2,3')
    parser.add_argument('-x', '--numX'          , dest="numX",          default= '5',            help='number of xgrid iterations for multicore grid step 1')
    parser.add_argument('-i', '--inputTemplate' , dest="inputTemplate", default= 'powheg.input', help='input cfg file (fixed) [=powheg.input]')
    parser.add_argument('-q', '--doQueue'       , dest="doQueue",       default= 'longlunch',          help='Condor job flavor [longlunch]')
    parser.add_argument('-m', '--prcName'       , dest="prcName",       default= 'DMGG',         help='POWHEG process name [DMGG]')
    parser.add_argument(      '--step3pilot'    , dest="step3pilot",    default= False,          help='do a pilot job to combine the grids, calculate upper bounds afterwards (otherwise afs jobs might fail)', action='store_true')
    parser.add_argument(      '--dry-run'       , dest="dryrun",        default= False,          help='show commands only, do not submit', action='store_true')
    parser.add_argument(      '--svn'           , dest="svnRev",        default= 0,              help='SVN revision. If 0, use tarball [0]')

    args = parser.parse_args ()
    
    QUEUE = args.doQueue
    EOSfolder = args.folderName

    print()
    print('RUNNING PARAMS: ')
    print('                parstage              = ' + args.parstage)
    print('                folderName            = ' + args.folderName) 
    print('                eosFolder             = ' + args.eosFolder) 
    print('                Number of jobs        = ' + args.numJobs) 
    print('                Number of xgrid iter  = ' + args.numX) 
    print('                Condor Job flavor     = ' + args.doQueue) 
    print('                powheg input cfg file = ' + args.inputTemplate) 
    print('                powheg process name   = ' + args.prcName)
    print('                do step 3 pilot run   = ' + str(args.step3pilot))
    print('                dry run               = ' + str(args.dryrun))
    print('                SVN                   = ' + str(args.svnRev))
    print()


    # parse differe
    queues = {}
    if ',' in args.doQueue:
        for pair in args.doQueue.split(','):
            istep,queue = pair.split(':')
            queues[int(istep)] = queue
    
    steps = []
    if '0' in args.parstage:
        #steps.append((0, 'compile',                   '-p 0','null'))
        print ("Compilation on condor is not supported anymore, please run step 0 interactively")
        sys.exit()
    if '1' in args.parstage:
        for ix in range(1, int(args.numX)+1):
            steps.append(
            (1, 'grid production 1-'+str(ix),'-p 1 -x '+str(ix), 'run_'+args.folderName+'_1_'+str(ix)+'.condorConf'))
    if '2' in args.parstage:
        steps.append(
            (2, 'grid production 2',         '-p 2', 'run_'+args.folderName+'_2_1.condorConf'))
    if '3' in args.parstage:
        if args.step3pilot:
            steps.append(
            (3, 'grid production 3 pilot',   '-p 3', 'run_'+args.folderName+'_3_1_pilot.condorConf'))
        steps.append(
            (3, 'grid production 3',         '-p 3', 'run_'+args.folderName+'_3_1.condorConf'))
    if '9' in args.parstage:
        steps.append(
            (9, 'grid production 9',         '-p 9 -k 1','null'))

    for istep,step,extraOpt,condorFile in steps:
        print('*'*50,step,'*'*5,extraOpt,'*'*50)
        njobs = args.numJobs
        if 'pilot' in step:
            njobs = '1'
        
        commonOpts='-i '+args.inputTemplate+' -m '+args.prcName+' -f '+args.folderName+' -j '+njobs+' --fordag 1'
        commonOpts+=' --svn %i' % args.svnRev
        if args.eosFolder != 'NONE':
            commonOpts+=' -e '+args.eosFolder
        if extraOpt!='-p 0' and extraOpt!='-p 9 -k 1':
            if len(queues) > 0:
                commonOpts = commonOpts+' -q '+queues[istep]
            else:
                commonOpts = commonOpts+' -q '+args.doQueue
        command = 'python3 ./run_pwg_condor.py %s %s'%(extraOpt,commonOpts)
        print(command)
        if args.dryrun:
            continue
        command_out = subprocess.getstatusoutput(command)[1]
        print(command_out)
    
    dagfilename = 'run_' + args.folderName + '.dag'
    dagfile = open(dagfilename, 'w')
    for istep,step,extraOpt,condorFile in steps:
        if 'condorConf' in condorFile:
            cleanstep = ''.join(e for e in step if e.isalnum())
            dagfile.write('JOB '+cleanstep+' '+condorFile)
            dagfile.write('\n')
    cwd = os.getcwd()
    dagfile.write('SCRIPT POST ALL_NODES %s/check_dag_success.py %s/%s.nodes.log $JOB\n' % (cwd, cwd, dagfilename))
    for i in range(len(steps)-1):
        condorFile_current = steps[i  ][3]
        condorFile_next    = steps[i+1][3]
        step_current = ''.join(e for e in steps[i  ][1] if e.isalnum())
        step_next    = ''.join(e for e in steps[i+1][1] if e.isalnum())
        if 'condorConf' in condorFile_current and 'condorConf' in condorFile_next:
            dagfile.write('PARENT '+step_current+' CHILD '+step_next)
            dagfile.write('\n')
    dagfile.close()
    
    command = 'sh dag_wrapper.sh %s'%(dagfilename)
    print(command)
    if not args.dryrun:
        command_out = subprocess.getstatusoutput(command)[1]
        print(command_out)
