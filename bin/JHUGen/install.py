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

def completed(name,basedir,card):
    completed = True
    fileExists=os.path.isfile('runcmsgrid.sh') 
    if fileExists:
        os.chmod('runcmsgrid.sh',0777)
        os.chdir('%s' % (basedir))
        os.system('pwd')
        os.system('rm -rf %s_JHUGen/LSFJOB*' % (name))
        os.system('mv %s_JHUGen/runcmsgrid.sh .' % (name)) 
        os.system('tar czvf JHUGen_%s.tgz %s_JHUGen runcmsgrid.sh' % (name,name))
    else:
        if not os.path.isfile('JHUGen_%s.tgz' % (name)):
            completed = False
    return completed

aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-card'    ,'--card'      ,action='store' ,dest='card',default='JHUGen.input',help='card')
aparser.add_argument('-name'    ,'--name'      ,action='store' ,dest='name'   ,default='ScalarVH'       ,help='name')
aparser.add_argument('-q'       ,'--queue'     ,action='store' ,dest='queue'  ,default='1nh'            ,help='queue')
args1 = aparser.parse_args()

print args1.card,args1.name,args1.queue

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
job_file.write('sed "s@BASEDIR@%s_JHUGen@g"   runcmsgrid_template.sh > runcmsgrid.sh \n'  % (args1.name))
job_file.write('chmod +x runcmsgrid.sh \n')
job_file.write('rm *.lhe \n')
job_file.close()
os.chmod('integrate.sh',0777)
os.system('bsub -q %s %s/%s_JHUGen/integrate.sh' % (args1.queue,basedir,args1.name))
           
while not completed(args1.name,basedir,args1.card):
    print "Waiting ",datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    time.sleep(60)
