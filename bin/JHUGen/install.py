#! /usr/bin/env python
import commands,sys,os,subprocess,stat
import datetime
import time 
from os import listdir
from os.path import isfile, join
from optparse import OptionParser
import argparse
import random
import ROOT

aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-card'    ,'--card'      ,action='store' ,dest='card',default='JHUGen.input',help='card')
aparser.add_argument('-name'    ,'--name'      ,action='store' ,dest='name'   ,default='ScalarVH'       ,help='name')
aparser.add_argument('-s'       ,'--seed'      ,action='store' ,dest='seed'   ,default='123456'         ,help='random seed for grid generation')
aparser.add_argument('-n'       ,'--nevents'   ,action='store' ,dest='nevents',default='100'            ,help='number of events for the test run after grid generation')
args1 = aparser.parse_args()

basedir=os.getcwd()
#Start with the basics download MCFM and add the options we care  :
os.system('cp patches/install.sh .')
os.system('./install.sh')
os.system('mv JHUGenerator %s_JHUGen' % args1.name)

##Get the base files
os.system('cp '+basedir+('/%s' % args1.card)+(' %s_JHUGen' % args1.name))

os.chdir('%s_JHUGen' % (args1.name))
os.system('make')

command='./JHUGen '
with open(basedir+'/'+args1.card,"rt")         as flabel: 
    for line in flabel:
        command=command+line.rstrip('\n')
        break
command=command+' DataFile=Out' 
print command

job_file = open('integrate.sh', "wt")
job_file.write('#!/bin/bash\n')
job_file.write('cd %s/%s_JHUGen/ \n'% (basedir,args1.name))
job_file.write('eval `scramv1 runtime -sh` \n')
job_file.write('cp %s/runcmsgrid_template.sh .                                    \n' % (basedir))
job_file.write('sed "s@GENCOMMAND@%s@g"    runcmsgrid_template.sh > runcmsgrid.sh \n' % (command))
job_file.write('mv runcmsgrid.sh runcmsgrid_template.sh                        \n')
job_file.write('sed "s@VegasNc2=NEVT@VegasNc2=\\$\\{nevt\\}@g"   runcmsgrid_template.sh > runcmsgrid.sh \n')
job_file.write('mv runcmsgrid.sh runcmsgrid_template.sh                        \n')
job_file.write('sed "s@Seed=SEED@Seed=\\$\\{rnum\\}@g"   runcmsgrid_template.sh > runcmsgrid.sh \n')
job_file.write('mv runcmsgrid.sh runcmsgrid_template.sh                        \n')
job_file.write('sed "s@BASEDIR@%s_JHUGen@g"   runcmsgrid_template.sh > runcmsgrid.sh \n'  % (args1.name))
job_file.write('chmod +x runcmsgrid.sh \n')
if "ReadCSmax" in command:
    #set up the grid now so it can be read
    runcommand = command.replace("ReadCSmax", "").replace("NEVT", args1.nevents).replace("SEED", args1.seed)
    job_file.write("%s \n" % (runcommand))
job_file.write('rm *.lhe \n')
job_file.write('rm -r data/ \n')
job_file.close()
os.chmod('integrate.sh',0777)
os.system('%s/%s_JHUGen/integrate.sh' % (basedir,args1.name))

os.chmod('runcmsgrid.sh',0777)
os.chdir('%s' % (basedir))
os.system('pwd')
os.system('rm -rf %s_JHUGen/LSFJOB*' % (args1.name))
os.system('mv %s_JHUGen/runcmsgrid.sh .' % (args1.name)) 
os.system('tar czvf JHUGen_%s.tgz %s_JHUGen runcmsgrid.sh' % (args1.name,args1.name))
