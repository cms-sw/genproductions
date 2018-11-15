
# EXAMPLE ON HOW TO RUN
# python ./run_pwg_parallel.py -i powheg_Zj.input -m Zj -f my_Zj -q 1nd -j 10

from optparse import OptionParser
import os
import sys
import commands
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

    parser = OptionParser()
    parser.add_option('-f', '--folderName'    , dest="folderName",    default='testProd',      help='local folder and last eos folder name[testProd]')
    parser.add_option('-j', '--numJobs'       , dest="numJobs",       default= '10',           help='number of jobs to be used for multicore grid step 1,2,3')
    parser.add_option('-x', '--numX'          , dest="numX",          default= '5',            help='number of xgrid iterations for multicore grid step 1')
    parser.add_option('-i', '--inputTemplate' , dest="inputTemplate", default= 'powheg.input', help='input cfg file (fixed) [=powheg.input]')
    parser.add_option('-q', '--doQueue'       , dest="doQueue",       default= 'longlunch',          help='Condor job flavor [longlunch]')
    parser.add_option('-m', '--prcName'       , dest="prcName",       default= 'DMGG',         help='POWHEG process name [DMGG]')
    parser.add_option(      '--step3pilot'    , dest="step3pilot",    default= False,          help='do a pilot job to combine the grids, calculate upper bounds afterwards (otherwise afs jobs might fail)', action='store_true')
    parser.add_option(      '--dry-run'       , dest="dryrun",        default= False,          help='show commands only, do not submit', action='store_true')

    (args, opts) = parser.parse_args(sys.argv)
    
    QUEUE = args.doQueue
    EOSfolder = args.folderName

    print
    print 'RUNNING PARAMS: '
    print '                folderName            = ' + args.folderName 
    print '                Number of jobs        = ' + args.numJobs 
    print '                Number of xgrid iter  = ' + args.numX 
    print '                Condor Job flavor     = ' + args.doQueue 
    print '                powheg input cfg file = ' + args.inputTemplate 
    print '                powheg process name   = ' + args.prcName
    print '                do step 3 pilot run   = ' + str(args.step3pilot)
    print '                dry run               = ' + str(args.dryrun)
    print


    steps=[
           ('compile',                   '-p 0','null')
          ]
    for ix in range(1, int(args.numX)+1):
        steps.append(
           ('grid production 1-'+str(ix),'-p 1 -x '+str(ix),args.folderName+'/run_1_'+str(ix)+'.log'))
    steps.append(
           ('grid production 2',         '-p 2',args.folderName+'/run_2_1.log'))
    if args.step3pilot:
        steps.append(
           ('grid production 3 pilot',   '-p 3',args.folderName+'/run_3_1.log'))
    steps.extend(
          [('grid production 3',         '-p 3',args.folderName+'/run_3_1.log'),
           ('grid production 9',         '-p 9 -k 1','null')])

    for step,extraOpt,logf in steps:
        print '*'*50,step,'*'*5,extraOpt,'*'*50
        njobs = '1' if 'pilot' in step else args.numJobs
        commonOpts='-i '+args.inputTemplate+' -m '+args.prcName+' -f '+args.folderName+' -j '+njobs
        if extraOpt!='-p 0' and extraOpt!='-p 9 -k 1': commonOpts = commonOpts+' -q '+args.doQueue
        command = 'python ./run_pwg_condor.py %s %s'%(extraOpt,commonOpts)
        print command
        if args.dryrun: continue
        command_out = commands.getstatusoutput(command)[1]
        print command_out
        if extraOpt!='-p 0' and extraOpt!='-p 9 -k 1': 
            print command
            command = 'condor_wait -status %s'%(logf)  
            command_out = commands.getstatusoutput(command)[1]
            print command_out
        sleep(5)   

        
