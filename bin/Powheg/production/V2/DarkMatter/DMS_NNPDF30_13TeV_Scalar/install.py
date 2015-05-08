#! /usr/bin/env python
import commands,sys,os,subprocess,stat
import datetime
import time 
from os import listdir
from os.path import isfile, join
from optparse import OptionParser
import argparse
import random

aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-sourceepo','--sourcerepo',action='store',dest='sourcerepo' ,default='slc6_amd64_gcc481/powheg/V2.0/src',help='sourcerepo')
aparser.add_argument('-release'  ,'--release'   ,action='store',dest='release' ,default='powhegboxv2.0_Apr2014_DarkMatter',help='Powheg release')
aparser.add_argument('-carddir','--carddir'   ,action='store',dest='carddir' ,default='examples/V2/DMV_NNPDF30_13TeV_v2/',help='carddir')
aparser.add_argument('-name'   ,'--name'      ,action='store',dest='name'    ,default='DMV_NNPDF30_13TeV_v2'          ,help='name')
aparser.add_argument('-procname','--procname' ,action='store',dest='procname',default='DMV'           ,help='Powheg process')
aparser.add_argument('-q'      ,'--queue'     ,action='store',dest='queue'  ,default='8nh'            ,help='queue')
aparser.add_argument('-dm'      ,'--dmrange'   ,dest='dmrange' ,nargs='+',type=int,     default=[1,10,50,100,500],    help='mass range')
aparser.add_argument('-med'     ,'--medrange'  ,dest='medrange',nargs='+',type=int,     default=[10,50,100,150,200,300,500,1000,1500],help='mediator range')
aparser.add_argument('-retar'   ,'--retar'     ,type=bool      ,dest='retar'   ,default=False,help='tar up')
args1 = aparser.parse_args()

print args1.carddir,args1.name,args1.queue,args1.dmrange,args1.medrange

basedir=os.getcwd()
##Start with the basics download MCFM and add the options we care  :
if not args1.retar:
    os.system('./create_powheg_tarball.sh %s %s %s C none none 500 1 Y' % (args1.sourcerepo,args1.release,args1.procname) )
    os.system('mv %s/work/POWHEG-BOX %s_POWHEG' % (args1.release,args1.name))
    os.system('rm -rf %s' % args1.release)

os.chdir ('%s_POWHEG' %(args1.name))

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
            os.system('cp -r %s %s_%s_%s_%s' % (args1.procname,args1.procname,args1.name,med,dm))
            for f in parameterfiles:
                with open('%s_%s_%s_%s/%s' % (args1.procname,args1.name,med,dm,f), "wt") as fout: 
                    with open(basedir+'/'+args1.carddir+'/'+f,"rt")         as fin: 
                        for line in fin:
                            tmpline =    line.replace('MED'  ,str(med))
                            tmpline = tmpline.replace('MASS' ,str(dm))
                            tmpline = tmpline.replace('RAND' ,str(random.randrange(1000,9999,1)))
                            fout.write(tmpline)
            os.chdir('%s_%s_%s_%s' % (args1.procname,args1.name,med,dm))
            os.chmod('run.sh',0777)
            os.system('./run.sh')
            os.chdir('..')
            powhegdir=basedir+'/'+args1.name+'_POWHEG/'+args1.procname+'_'+args1.name+'_'+str(med)+'_'+str(dm)
            job_file = open('%s_%s_%s_%s/integrate.sh' % (args1.procname,args1.name,med,dm), "wt")
            job_file.write('#!/bin/bash\n')
            job_file.write('cd %s \n'% (powhegdir))
            job_file.write('eval `scramv1 runtime -sh` \n')
            job_file.write('export LHAPATH=/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/share/LHAPDF/ \n')
            job_file.write('cd - \n')
            job_file.write('cp %s/powheg.input  . \n'                                % (powhegdir))
            job_file.write('cp %s/pwhg_main  . \n'                                   % (powhegdir))
            job_file.write('./pwhg_main &> log.txt \n')
            job_file.write('cat pwgevents.lhe | grep -v "Random number generator exit values" > events_final.lhe \n')
            job_file.write('ls -l events_final.lhe \n')
            job_file.write("sed -i 's/Input file powheg.input contained:/Process: %s\\nInput file powheg.input contained:/g' events_final.lhe \n" % args1.procname)
            job_file.write('pwd \n')
            job_file.write('cp events_final.lhe  %s \n' % (powhegdir))
            job_file.write('cp -p pwg*.dat       %s \n' % (powhegdir))
            job_file.write('cp -r %s . \n'              % (powhegdir))
            job_file.write('cp %s/powheg.input_final  powheg.input \n'                                % (powhegdir))
            job_file.write(" sed -e 's/PROCESS/%s/g' %s/runcmsgrid_powheg.sh > runcmsgrid.sh \n" % (args1.procname,basedir) )
            job_file.write('chmod 755 runcmsgrid.sh \n')
            job_file.write('tar cpzsf POWHEG_%s_%s_%s_tarball.tar.gz * \n' % (args1.name,med,dm))
            job_file.write('mv POWHEG_%s_%s_%s_tarball.tar.gz %s       \n' % (args1.name,med,dm,basedir))
            job_file.close()
            os.chmod('%s_%s_%s_%s/integrate.sh' % (args1.procname,args1.name,med,dm),0777)
            os.system('bsub -q %s %s/%s_POWHEG/%s_%s_%s_%s/integrate.sh' % (args1.queue,basedir,args1.name,args1.procname,args1.name,med,dm))

