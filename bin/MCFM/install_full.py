#! /usr/bin/env python
import commands,sys,os,subprocess,stat
import datetime
import time 
from os import listdir
from os.path import isfile, join
from optparse import OptionParser
import argparse
import random

def completed(name,medrange,dmrange,widthrange,gdmrange,basedir,carddir):
    completed = True
    labelname=''
    with open(basedir+'/'+carddir+'/outputname',"rt")         as flabel: 
        for line in flabel:
            labelname=line
    print "Label!",labelname
    for width in widthrange:
        for gdm in gdmrange:
            for med in medrange:
                for dm in dmrange:
                    if med < dm: 
                        continue
                    fileExists=os.path.isfile('MCFM_%s_%s_%s_%s_%s/%s_gridfile' % (name,med,dm,gdm,width,labelname)) 
                    if fileExists:
                        os.system('rm MCFM_%s_%s_%s_%s_%s/input.DAT' % (name,med,dm,gdm,width))
                        with open('runcmsgrid.sh', "wt")                          as fout: 
                            with open(basedir+'/runcmsgrid_template.sh',"rt")     as fin: 
                                for line in fin:
                                    tmpline =    line   .replace('BASENAME', 'MCFM_%s_%s_%s_%s_%s' % (name,med,dm,gdm,width))
                                    tmpline =    tmpline.replace('FILENAME', '%s.lhe'              % (labelname))
                                    fout.write(tmpline)
                        os.chmod('runcmsgrid.sh',0777)
                        os.system('tar czvf  MCFM_%s_%s_%s_%s_%s.tgz  MCFM_%s_%s_%s_%s_%s runcmsgrid.sh' % (name,med,dm,gdm,width,name,med,dm,gdm,width))
                        os.system('rm -rf MCFM_%s_%s_%s_%s_%s' % (name,med,dm,gdm,width))
                    else:
                        if not os.path.isfile('MCFM_%s_%s_%s_%s_%s.tgz' % (name,med,dm,gdm,width)):
                            completed = False
    return completed

aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-carddir','--carddir'   ,action='store',dest='carddir',default='cards/PseudoScalarDM_Full/',help='carddir')
aparser.add_argument('-name'   ,'--name'      ,action='store',dest='name'   ,default='PseudoScalarDM_Full'       ,help='name')
aparser.add_argument('-q'      ,'--queue'     ,action='store',dest='queue'  ,default='8nm'                 ,help='queue')
aparser.add_argument('-dm'     ,'--dmrange'   ,dest='dmrange' ,nargs='+',type=int,     default=[1,10,50,100,500],    help='mass range')
aparser.add_argument('-med'    ,'--medrange'  ,dest='medrange',nargs='+',type=int,     default=[10,50,100,150,200,300,500,1000,1500],help='mediator range')
aparser.add_argument('-gdm'    ,'--gdmrange'  ,dest='gdmrange',nargs='+',type=int,     default=[0.1,1.0,2.0,3.3],help='coupling range')
#aparser.add_argument('-width'  ,'--widthrange',dest='widthrange',nargs='+',type=int,   default=[1.0,2.0],help='width range')
aparser.add_argument('-width'  ,'--widthrange',dest='widthrange',nargs='+',type=int,   default=[1.0],help='width range')
aparser.add_argument('-retar'   ,'--retar'     ,type=bool      ,dest='retar'   ,default=False,help='tar up')
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
parameterfiles = [ f for f in listdir(basedir+'/'+args1.carddir) if isfile(join(basedir+'/'+args1.carddir,f)) ]
#print parameterfiles,' -',basedir+'/'+args1.carddir
##Now build the directories iterating over options
if not args1.retar:
    random.seed(1)
    for width in args1.widthrange:
        for gdm in args1.gdmrange:
            for med in args1.medrange:
                for dm in args1.dmrange:
                    if med < dm:
                        continue
                    os.system('cp -r Bin MCFM_%s_%s_%s_%s_%s' % (args1.name,med,dm,gdm,width))
                    for f in parameterfiles:
                        with open('MCFM_%s_%s_%s_%s_%s/%s' % (args1.name,med,dm,gdm,width,f), "wt") as fout: 
                            with open(basedir+'/'+args1.carddir+'/'+f,"rt")         as fin: 
                                for line in fin:
                                    tmpline =    line.replace('WIDTH',str(width))
                                    tmpline = tmpline.replace('GDM'  ,str(gdm))
                                    tmpline = tmpline.replace('MED'  ,str(med))
                                    tmpline = tmpline.replace('MASS' ,str(dm))
                                    tmpline = tmpline.replace('RAND' ,str(random.randrange(1000,9999,1)))
                                    fout.write(tmpline)
                    job_file = open('MCFM_%s_%s_%s_%s_%s/integrate.sh' % (args1.name,med,dm,gdm,width), "wt")
                    job_file.write('#!/bin/bash\n')
                    job_file.write('cd %s/%s_MCFM-7.0/MCFM_%s_%s_%s_%s_%s/ \n'% (basedir,args1.name,args1.name,med,dm,gdm,width))
                    job_file.write('eval `scramv1 runtime -sh` \n')
                    job_file.write('export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/lib/ \n')
                    job_file.write('export LHAPATH=/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/share/LHAPDF/ \n')
                    job_file.write('./mcfm \n')
                    job_file.write('rm *.lhe \n')
                    job_file.close()
                    os.chmod('MCFM_%s_%s_%s_%s_%s/integrate.sh' % (args1.name,med,dm,gdm,width),0777)
                    os.system('bsub -q %s %s/%s_MCFM-7.0/MCFM_%s_%s_%s_%s_%s/integrate.sh' % (args1.queue,basedir,args1.name,args1.name,med,dm,gdm,width))
                   
while not completed(args1.name,args1.medrange,args1.dmrange,args1.widthrange,args1.gdmrange,basedir,args1.carddir):
    print "Waiting ",datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    time.sleep(60)
