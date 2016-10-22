from optparse import OptionParser
import os
import sys
import commands
import re
from time import sleep

if __name__ == "__main__":

    eoscmd = '/afs/cern.ch/project/eos/installation/cms/bin/eos.select' ;

    parser = OptionParser()
    parser.add_option('-f', '--folderName'    , dest="folderName",    default='testProd',      help='local folder and last eos folder name[testProd]')
    parser.add_option('-j', '--numJobs'       , dest="numJobs",       default= '10',           help='number of jobs to be used for multicore grid step 1,2,3')
    parser.add_option('-i', '--inputTemplate' , dest="inputTemplate", default= 'powheg.input', help='input cfg file (fixed) [=powheg.input]')
    parser.add_option('-q', '--lsfQueue'      , dest="lsfQueue",      default= '',             help='LSF queue [2nd]')
    parser.add_option('-m', '--prcName'       , dest="prcName",       default= 'DMGG',         help='POWHEG process name [DMGG]')

    (args, opts) = parser.parse_args(sys.argv)
    
    QUEUE = args.lsfQueue
    EOSfolder = args.folderName

    print
    print 'RUNNING PARAMS: '
    print '                folderName = ' + args.folderName 
    print '                Number of jobs = ' + args.numJobs 
    print '                LSF Queue = ' + args.lsfQueue 
    print '                powheg input cfg file : ' + args.inputTemplate 
    print '                powheg process name : ' + args.prcName
    print

    commonOpts='-i '+args.inputTemplate+' -m '+args.prcName+' -f '+args.folderName+' -q '+args.lsfQueue+' -j '+args.numJobs

    steps=[('compile','-p 0'),
           ('grid production 1-1','-p 1 -x 1'),
           ('grid production 1-2','-p 1 -x 2'),
           ('grid production 1-3','-p 1 -x 3'),
           ('grid production 1-4','-p 1 -x 4'),
           ('grid production 1-5','-p 1 -x 5'),
           ('grid production 2',   '-p 2'),
           ('grid production 3',   '-p 3'),
           ('grid production 9',   '-p 9 -k 1')]

    for step,extraOpt in steps:
        print '*'*50,extraOpt,'*'*50
        command_out = commands.getstatusoutput('python ./run_pwg.py %s %s'%(extraOpt,commonOpts))[1]
        print command_out
        # jobNumbers=[int(s) for s in re.findall(r'\<(.*?)\>', command_out) if s.isdigit()]
        # while True:
            # command_out = commands.getstatusoutput("bjobs | awk '{print $1}'")[1]
            # runningJobs=[int(s) for s in command_out.split() if s.isdigit()]
            # jobPending=False
            # for j in jobNumbers:
                # if j in runningJobs:
                    # jobPending=True
                    # break
            # if not jobPending:
                # print 'All done for this step'
          # break
            # else:
                # print 'Querying batch in 10s'
                # sleep(10)

        
