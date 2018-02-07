#! /usr/bin/env python

#
# Description:
#

import commands,sys,os,subprocess,stat
import datetime
import time 
from os.path import isfile, join
from optparse import OptionParser
import argparse
import random
import platform

#Colors:
class fcol:
    header = '\033[95m'
    blue   = '\033[94m'
    green  = '\033[92m'
    yellow = '\033[93m'
    red    = '\033[91m'
    end    = '\033[0m'
    bold   = '\033[1m'

#Parsing arguments:
aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-carddir'   ,'--carddir'   ,action='store',dest='carddir'   ,default='cards/'           ,help='Dir for FPMC cards')
aparser.add_argument('-name'      ,'--name'      ,action='store',dest='name'      ,default='dummy'            ,help='Label for working dir')
aparser.add_argument('-q'         ,'--queue'     ,action='store',dest='queue'     ,default='2nd'              ,help='LXBATCH queue')
aparser.add_argument('-scram_arch','--scram_arch',action='store',dest='scram_arch',default='slc6_amd64_gcc481',help='SCRAM_ARCH sys var, default slc6_amd64_gcc481')
aparser.add_argument('-cmssw'     ,'--cmssw'     ,action='store',dest='cmssw'     ,default='CMSSW_7_1_30'     ,help='CMSSW_VERSION sys var, default CMSSW_7_1_30')
aparser.add_argument('-run'       ,'--run'       ,type=bool     ,dest='run'       ,default=False              ,help='Run input cards at cards/')
args1 = aparser.parse_args()

#Screen output:
print 'Set options: \n',
print 'Cards dir =      ',args1.carddir,'\n',
print 'Label work dir = ',args1.name,'\n',
print 'LXBATCH queue =  ',args1.queue,'\n',
print 'SCRAM_ARCH =     ',args1.scram_arch,'\n',
print 'CMSSW release =  ',args1.cmssw,'\n',
print 'Run jobs? =      ',args1.run

#Start downloding FPMC source code and compiling it:
basedir=os.getcwd()
print ''
print fcol.yellow+platform.python_compiler()+fcol.end
print 'BE SURE THAT GCC VERSION IS >6.0, otherwise use the following command line: \n'
print fcol.red+'source /cvmfs/sft.cern.ch/lcg/external/gcc/6.1.0/x86_64-slc6/setup.sh \n'+fcol.end
if os.path.exists('fpmc/'):
    ts = time.time()
    os.system('mv fpmc/ fpmc_%s/' % datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d_%H:%M:%S'))
print 'Cloning git repo of FPMC event generator: \n',
os.system('git clone https://github.com/fpmc-hep/fpmc.git')
print 'Wait ... \n',
os.chdir('fpmc/')
os.makedirs('build/')
os.chdir('build/')
os.system('cmake ..')
print 'Compiling FPMC... \n',
os.system('make')
print fcol.blue+'FPMC is ready \n'+fcol.end,

#Get cards in cards/:
parameterfiles = [ f for f in listdir(basedir+'/'+args1.carddir) if isfile(join(basedir+'/'+args1.carddir,f)) ]
print parameterfiles,' -',basedir+'/'+args1.carddir

#Run jobs:
if args1.run:
    print 'You chose to run an event production. \n',
    os.makedirs('job_%2/' % (args1.name))
    random.seed(1)
    print 'Preperaing scripts to be submitted to LXBATCH... \n',
    for f in parameterfiles:
       job_file = open('job_%s/job_%s.sh' % (args1.name,f), "wt")
       job_file.write('#!/bin/bash\n')
       job_file.write('cd %s/job_%s/ \n' % (basedir,args1.name))
       job_file.write('eval `scramv1 runtime -sh` \n')
       job_file.write('export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/lib/ \n')
       job_file.write('export LHAPATH=/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/share/LHAPDF/ \n')
       job_file.write('./fpmc < cards/%s \n' % (f))
       job_file.write('rm *.lhe \n')
       job_file.close()
       os.chmod('job_%s/job_%s.sh' % (args1.name,f),0777)
       print 'Submitting job... \n',
       os.system('bsub -q %s %s/job_%s/job_%s.sh -o %s/job_%s/job_%s.out -e %s/job_%s/job_%s.err' % (args1.queue,basedir,args1.name,f))
