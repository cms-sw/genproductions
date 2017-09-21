#! /usr/bin/env python
import commands,sys,os,subprocess,stat
import datetime
import time 
from os import listdir
from os.path import isfile, join
from optparse import OptionParser
import argparse
import random

def completed(name,medrange,dmrange,basedir,carddir,scram_arch,cmssw):
    completed = True
    labelname=''
    with open(basedir+'/'+carddir+'/outputname',"rt")         as flabel: 
        for line in flabel:
            labelname=line
    print "Label!",labelname
    for med in medrange:
        for dm in dmrange:
            if med < dm: 
                continue
            fileExists=os.path.isfile('MCFM_%s_%s_%s/%s_gridfile' % (name,med,dm,labelname)) 
            if fileExists:
                os.system('rm MCFM_%s_%s_%s/input.DAT' % (name,med,dm))
                with open('runcmsgrid.sh', "wt")                          as fout: 
                    with open(basedir+'/runcmsgrid_template.sh',"rt")         as fin: 
                        for line in fin:
                            tmpline =    line   .replace('BASENAME', 'MCFM_%s_%s_%s' % (name,med,dm))
                            tmpline =    tmpline.replace('FILENAME', '%s.lhe'        % (labelname))
                            tmpline = tmpline.replace('SCRAM_ARCH_VERSION_REPLACE','%s' % (scram_arch))
                            tmpline = tmpline.replace('CMSSW_VERSION_REPLACE'     ,'%s' % (cmssw))
                            fout.write(tmpline)
                os.chmod('runcmsgrid.sh',0777)
                os.system('tar czvf  MCFM_%s_%s_%s_%s_%s.tgz  MCFM_%s_%s_%s runcmsgrid.sh' % (name,med,dm,scram_arch,cmssw,name,med,dm))
                os.system('rm -rf MCFM_%s_%s_%s' % (name,med,dm))
            else:
                if not os.path.isfile('MCFM_%s_%s_%s_%s_%s.tgz' % (name,med,dm,scram_arch,cmssw)):
                    completed = False
    return completed

aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-carddir','--carddir'   ,action='store',dest='carddir',default='cards/ScalarDM/',help='carddir')
aparser.add_argument('-name'   ,'--name'      ,action='store',dest='name'   ,default='ScalarDM'       ,help='name')
aparser.add_argument('-q'      ,'--queue'     ,action='store',dest='queue'  ,default='8nm'            ,help='queue')
aparser.add_argument('-dm'     ,'--dmrange'   ,dest='dmrange' ,nargs='+',type=int,     default=[1,10,50,100,500],    help='mass range')
aparser.add_argument('-med'    ,'--medrange'  ,dest='medrange',nargs='+',type=int,     default=[10,50,100,150,200,300,500,1000,1500],help='mediator range')
aparser.add_argument('-retar'   ,'--retar'     ,type=bool      ,dest='retar'   ,default=False,help='tar up')
aparser.add_argument('-scram_arch' ,'--scram_arch',action='store' ,dest='scram_arch',default='slc6_amd64_gcc481',help='SCRAM_ARCH system variable, default slc6_amd64_gcc481')
aparser.add_argument('-cmssw'      ,'--cmssw'     ,action='store' ,dest='cmssw'     ,default='CMSSW_7_1_30'     ,help='CMSSW_VERSION system variable, default CMSSW_7_1_30')
args1 = aparser.parse_args()

print args1.carddir,args1.name,args1.queue,args1.dmrange,args1.medrange

basedir=os.getcwd()
##Start with the basics download MCFM and add the options we care  :
if not args1.retar:
    os.system('wget http://mcfm.fnal.gov/MCFM-7.0.tar.gz')
    os.system('tar xzvf MCFM-7.0.tar.gz')
    os.system('cp -r patches MCFM-7.0')
    os.system('mv MCFM-7.0 %s_MCFM-7.0' % (args1.name))

os.chdir ('%s_MCFM-7.0' %(args1.name))

if not args1.retar:
    os.system('cp  patches/Install  . ')
    os.system('cp  patches/makefile . ')
    os.system ('./patches/setup.sh')

##Get the base files
if not args1.retar:
    parameterfiles = [ f for f in listdir(basedir+'/'+args1.carddir) if isfile(join(basedir+'/'+args1.carddir,f)) ]
    print parameterfiles,' -',basedir+'/'+args1.carddir

if not args1.retar:
##Now build the directories iterating over options
random.seed(1)
    for med in args1.medrange:
        for dm in args1.dmrange:
            if med < dm:
                continue
            os.system('cp -r Bin MCFM_%s_%s_%s' % (args1.name,med,dm))
            for f in parameterfiles:
                with open('MCFM_%s_%s_%s/%s' % (args1.name,med,dm,f), "wt") as fout: 
                    with open(basedir+'/'+args1.carddir+'/'+f,"rt")         as fin: 
                        for line in fin:
                            tmpline =    line.replace('MED' ,str(med))
                            tmpline = tmpline.replace('MASS',str(dm))
                            tmpline = tmpline.replace('RAND',str(random.randrange(1000,9999,1)))
                            fout.write(tmpline)
            job_file = open('MCFM_%s_%s_%s/integrate.sh' % (args1.name,med,dm), "wt")
            job_file.write('#!/bin/bash\n')
            job_file.write('cd %s/%s_MCFM-7.0/MCFM_%s_%s_%s/ \n'% (basedir,args1.name,args1.name,med,dm))
            job_file.write('eval `scramv1 runtime -sh` \n')
            job_file.write('export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/lib/ \n')
            job_file.write('export LHAPATH=/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/share/LHAPDF/ \n')
            job_file.write('./mcfm \n')
            job_file.write('rm *.lhe \n')
            job_file.close()
            os.chmod('MCFM_%s_%s_%s/integrate.sh' % (args1.name,med,dm),0777)
            os.system('bsub -q %s %s/%s_MCFM-7.0/MCFM_%s_%s_%s/integrate.sh' % (args1.queue,basedir,args1.name,args1.name,med,dm))
           
while not completed(args1.name,args1.medrange,args1.dmrange,basedir,args1.carddir,args1.scram_arch,args1.cmssw):
    print "Waiting ",datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    time.sleep(60)
