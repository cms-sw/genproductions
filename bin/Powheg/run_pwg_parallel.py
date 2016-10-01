#!/usr/bin/python

'''
Script for POWHEG generator production with parallel processing
By Luca Perrozzi Sept. 30th 2016
'''

import commands
import fileinput
# import argparse
import sys
import os
from optparse import OptionParser

TESTING = 0
QUEUE = ''

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


if __name__ == "__main__":

    eoscmd = '/afs/cern.ch/project/eos/installation/cms/bin/eos.select' ;

    parser = OptionParser()
    parser.add_option('-p', '--parstage'      , dest="parstage",      default= '0',            help='starting stage of the production process [0]')
    parser.add_option('-x', '--xgrid'         , dest="xgrid",         default= '5',            help='number of iterations for the parstage (grids production) step [5]')
    parser.add_option('-f', '--folderName'    , dest="folderName",    default='testProd',      help='local folder and last eos folder name[testProd]')
    parser.add_option('-j', '--numJobs'       , dest="numJobs",       default= '10',           help='number of jobs to be used for multicore grid step 1,2,3')
    parser.add_option('-t', '--totEvents'     , dest="totEvents",     default= '10000',        help='total number of events to be generated [10000]')
    parser.add_option('-n', '--numEvents'     , dest="numEvents",     default= '2000',         help='number of events for a single job [2000]')
    parser.add_option('-i', '--inputTemplate' , dest="inputTemplate", default= 'powheg.input', help='input cfg file (fixed) [=powheg.input]')
    parser.add_option('-q', '--lsfQueue'      , dest="lsfQueue",      default= '',             help='LSF queue [2nd]')
    parser.add_option('-s', '--rndSeed'       , dest="rndSeed",       default= '42',           help='Starting random number seed [42]')
    parser.add_option('-m', '--prcName'       , dest="prcName",       default= 'DMGG',         help='POWHEG process name [DMGG]')
    parser.add_option('-k', '--keepTop'       , dest="keepTop",       default= '1',            help='Keep the validation top draw plots [0]')

    # args = parser.parse_args ()
    (args, opts) = parser.parse_args(sys.argv)
    
    QUEUE = args.lsfQueue
    EOSfolder = args.folderName

    print
    print 'RUNNING PARAMS: '
    print '                parstage = ' + args.parstage
    print '                xgrid = ' + args.xgrid
    print '                folderName = ' + args.folderName 
    print '                Number of jobs = ' + args.numJobs 
    print '                LSF Queue = ' + args.lsfQueue 
    print '                powheg input cfg file : ' + args.inputTemplate 
    print '                powheg process name : ' + args.prcName
    print '                working folder : ' + args.folderName
    print '                base folder : ' + rootfolder
    print

    
    print 'DOWNLOAD AND COMPILE'
    runCommand('python ./run_pwg.py -p 0 -i '+args.inputTemplate+' -m '+args.prcName+' -f '+args.folderName+' -q '+args.lsfQueue+'; wait',True,0)
    
    print 'STEP 1'
    for i_xgrid in range(1, int(args.xgrid)+1):
      print 'xgrid',i_xgrid
      runCommand('python ./run_pwg.py -p 1 -x '+str(i_xgrid)+' -i '+args.inputTemplate+' -m '+args.prcName+' -f '+args.folderName+' -q '+args.lsfQueue+' -j '+args.numJobs+'; wait',True,0)
    
    print 'STEP 2'
    runCommand('python ./run_pwg.py -p 2 -i '+args.inputTemplate+' -m '+args.prcName+' -f '+args.folderName+' -q '+args.lsfQueue+' -j '+args.numJobs+'; wait',True,0)

    print 'STEP 3'
    runCommand('python ./run_pwg.py -p 3 -i '+args.inputTemplate+' -m '+args.prcName+' -f '+args.folderName+' -q '+args.lsfQueue+' -j '+args.numJobs+'; wait',True,0)

    print 'CREATE GRIDPACK'
    runCommand('python ./run_pwg.py -p 9 -i '+args.inputTemplate+' -m '+args.prcName+' -f '+args.folderName+' -k 1',True,0)
    
    
    
    