# EXAMPLE ON HOW TO RUN
# python ./run_pwg.py -p 0 -i powheg_Zj.input -m Zj -f my_Zj -q 8nh -j 10

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
    parser.add_option('-i', '--inputTemplate' , dest="inputTemplate", default= 'powheg.input', help='input cfg file (fixed) [=powheg.input]')
    parser.add_option('-q', '--lsfQueue'      , dest="lsfQueue",      default= '1nd',          help='LSF queue [2nd]')
    parser.add_option('-m', '--prcName'       , dest="prcName",       default= 'DMGG',         help='POWHEG process name [DMGG]')

    (args, opts) = parser.parse_args(sys.argv)
    
    QUEUE = args.lsfQueue
    EOSfolder = args.folderName

    print
    print 'RUNNING PARAMS: '
    print '                folderName            = ' + args.folderName 
    print '                Number of jobs        = ' + args.numJobs 
    print '                LSF Queue             = ' + args.lsfQueue 
    print '                powheg input cfg file = ' + args.inputTemplate 
    print '                powheg process name   = ' + args.prcName
    print


    steps=[
           ('compile','-p 0'),
           ('grid production 1-1','-p 1 -x 1'),
           ('grid production 1-2','-p 1 -x 2'),
           ('grid production 1-3','-p 1 -x 3'),
           ('grid production 1-4','-p 1 -x 4'),
           ('grid production 1-5','-p 1 -x 5'),
           ('grid production 2',   '-p 2'),
           ('grid production 3',   '-p 3'),
           ('grid production 9',   '-p 9 -k 1')
          ]

    for step,extraOpt in steps:
        print '*'*50,extraOpt,'*'*50
        commonOpts='-i '+args.inputTemplate+' -m '+args.prcName+' -f '+args.folderName+' -j '+args.numJobs
        if extraOpt!='-p 0' and extraOpt!='-p 9 -k 1': commonOpts = commonOpts+' -q '+args.lsfQueue
        command = 'python ./run_pwg.py %s %s'%(extraOpt,commonOpts)
        print command
        command_out = commands.getstatusoutput(command)[1]
        print command_out
        jobNumbers=[int(s) for s in re.findall(r'\<(.*?)\>', command_out) if s.isdigit()]
        while True:
            command_out = commands.getstatusoutput("bjobs | awk '{print $1}'")[1]
            runningJobs=[int(s) for s in command_out.split() if s.isdigit()]
            jobPending=False
            for j in jobNumbers:
                if j in runningJobs:
                    jobPending=True
                    break
            if not jobPending:
                print 'All done for this step'
                break
            else:
                command_out = commands.getstatusoutput("bjobs | wc -l")[1]
                print datetime.datetime.now(),'jobs running:',(int(command_out)-1),'Querying batch in 120s, check logfile.log for more info'
                sleep(120)

        
