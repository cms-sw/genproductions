#! /usr/bin/env python
import commands,sys,os,subprocess,stat
import datetime
import time 
from os import listdir
from os.path import isfile, join
from optparse import OptionParser
import argparse
import random

def completed(name,medrange,dmrange,basedir,carddir):
    completed = True
    for med in medrange:
        for dm in dmrange:
            if med < dm: 
                continue
            fileExists=os.path.isfile('%s/%s_MG5_aMC_v2_2_2/MG_%s_%s_%s/process/run.sh' % (basedir,name,name,med,dm)) 
            if fileExists:
                #os.chmod('runcmsgrid.sh',0777)
                madgraph='%s/%s_MG5_aMC_v2_2_2/MG_%s_%s_%s/mgbasedir' % (basedir,name,name,med,dm)
                process ='%s/%s_MG5_aMC_v2_2_2/MG_%s_%s_%s/process'   % (basedir,name,name,med,dm)
                runcms  ='%s/runcmsgrid.sh'                           % (basedir)
                output  ='%s_%s_%s_tarball.tar.xz'                    % (name,med,dm)
                os.system('XZ_OPT="--lzma2=preset=9,dict=512MiB" tar -cJpsf '+output+' '+madgraph+' '+process+' '+runcms)
                #os.system('rm -rf %s/%s_MG5_aMC_v2_2_2/MG_%s_%s_%s' % (basedir,name,name,med,dm))
            else:
                if not os.path.isfile('MG_%s_%s_%s.tgz' % (name,med,dm)):
                    completed = False
    return completed

def replace(name,med,dm,rand,directory):
    parameterfiles = [ f for f in listdir(directory) if isfile(join(directory,f)) ]    
    for f in parameterfiles:
        with open('%s/%s_tmp' % (directory,f),"wt") as fout: 
            with open(directory+'/'+f        ,"rt") as fin: 
                for line in fin:
                    tmpline =    line.replace('MED'  ,str(med))
                    tmpline = tmpline.replace('XMASS',str(dm))
                    tmpline = tmpline.replace('RAND' ,str(rand))
                    fout.write(tmpline)
        os.system('mv %s/%s_tmp %s/%s'%(directory,f,directory,f))
 
aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-carddir','--carddir'   ,action='store',dest='carddir',default='cards/MonoW/'   ,help='carddir')
aparser.add_argument('-name'   ,'--name'      ,action='store',dest='name'   ,default='MonoW'          ,help='name')
aparser.add_argument('-q'      ,'--queue'     ,action='store',dest='queue'  ,default='1nd'            ,help='queue')
#aparser.add_argument('-dm'     ,'--dmrange'   ,dest='dmrange' ,nargs='+',type=int,     default=[1,10],    help='mass range')
#aparser.add_argument('-med'    ,'--medrange'  ,dest='medrange',nargs='+',type=int,     default=[100,150], help='mediator range')
aparser.add_argument('-dm'     ,'--dmrange'   ,dest='dmrange' ,nargs='+',type=int,     default=[1,10,50,100,500],    help='mass range')
aparser.add_argument('-med'    ,'--medrange'  ,dest='medrange',nargs='+',type=int,     default=[10,50,100,150,200,300,500,1000,1500],help='mediator range')
args1 = aparser.parse_args()

print args1.carddir,args1.name,args1.queue,args1.dmrange,args1.medrange

basedir=os.getcwd()
os.system('rm %s/%s/*~' % (basedir,args1.carddir))
##Start with the basics download Madgraph and add the options we care  :
os.system('cp  patches/install.sh .')
os.system('./install.sh')
os.system('mv MG5_aMC_v2_2_2 %s_MG5_aMC_v2_2_2' % args1.name)
os.chdir ('%s_MG5_aMC_v2_2_2' %(args1.name))

##Get the base files
parameterdir   = [ f for f in listdir(basedir+'/'+args1.carddir) if not isfile(join(basedir+'/'+args1.carddir,f)) ]
parameterfiles = [ f for f in listdir(basedir+'/'+args1.carddir) if isfile(join(basedir+'/'+args1.carddir,f)) ]
print parameterfiles,' -',basedir+'/'+args1.carddir,parameterdir

proc = [f for f in parameterfiles if f.find('proc')    > -1]
cust = [f for f in parameterfiles if f.find('custom')  > -1]
spin = [f for f in parameterfiles if f.find('madspin') > -1]

##Now build the directories iterating over options
random.seed(1)
for med    in args1.medrange:
    for dm in args1.dmrange:
        if med < dm:
            continue
        rand=random.randrange(1000,9999,1)
        for f in parameterdir:
            if f.find('model') == -1:
                continue
            os.system('cp -r %s/%s/%s models/%s_%s_%s' % (basedir,args1.carddir,f,f,med,dm))
            replace(args1.name,med,dm,rand,'models/%s_%s_%s' % (f,med,dm))
            os.chdir('models/%s_%s_%s' % (f,med,dm))
            os.system('python write_param_card.py')
            os.chdir('%s/%s_MG5_aMC_v2_2_2' % (basedir,args1.name))
        os.system('mkdir MG_%s_%s_%s' % (args1.name,med,dm))
        for f in parameterfiles:
            with open('MG_%s_%s_%s/%s' % (args1.name,med,dm,f), "wt") as fout: 
                with open(basedir+'/'+args1.carddir+'/'+f        ,"rt") as fin: 
                    for line in fin:
                        tmpline =    line.replace('MED'  ,str(med))
                        tmpline = tmpline.replace('XMASS',str(dm))
                        tmpline = tmpline.replace('RAND' ,str(random.randrange(1000,9999,1)))
                        fout.write(tmpline)
        job_file = open('%s/%s_MG5_aMC_v2_2_2/MG_%s_%s_%s/integrate.sh' % (basedir,args1.name,args1.name,med,dm), "wt")
        job_file.write('#!/bin/bash\n')
        job_file.write('cp -r %s/%s_MG5_aMC_v2_2_2/MG_%s_%s_%s/  .     \n' % (basedir,args1.name,args1.name,med,dm))
        job_file.write('cd    %s/%s_MG5_aMC_v2_2_2/MG_%s_%s_%s/        \n' % (basedir,args1.name,args1.name,med,dm))
        job_file.write('eval `scramv1 runtime -sh` \n')
        job_file.write('LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml \n')
        job_file.write('if [ -e $LHAPDF6TOOLFILE ]; then \n')
        job_file.write('  LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\\"LHAPDF6_BASE\\"" | cut -d \\" -f 4`/bin/lhapdf-config \n')
        job_file.write('else \n')
        job_file.write('  LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"` \n')
        job_file.write('fi \n')
        job_file.write('export LHAPDF_DATA_PATH=`$LHAPDFCONFIG --datadir` \n')
        job_file.write('LHAPDFINCLUDES=`$LHAPDFCONFIG --incdir` \n')
        job_file.write('LHAPDFLIBS=`$LHAPDFCONFIG --libdir` \n')
        #job_file.write('export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/lib/ \n')
        #job_file.write('export LHAPATH=/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/share/LHAPDF/ \n')
        job_file.write('cd -  \n')
        job_file.write('cd MG_%s_%s_%s/                       \n' % (args1.name,med,dm) )
        job_file.write('%s/%s_MG5_aMC_v2_2_2/bin/mg5_aMC  %s  \n' % (basedir,args1.name,proc[0]) )
        job_file.write('cd  %s_%s_%s                          \n' % (args1.name,med,dm) )
        if len(cust) > 0:
            job_file.write('cat %s >> makegrid.dat \n' % cust[0])
        for f in parameterfiles:
            if f.find('dat') > -1:
                job_file.write('mv ../%s Cards \n' % f)
            if f.find('.f')  > -1:
                job_file.write('mv ../%s SubProcesses \n' % f)
        job_file.write('echo "done" > makegrid.dat  \n')
        job_file.write('echo "set gridpack true" >> makegrid.dat \n')
        job_file.write('echo "" >> makegrid.dat \n')
        job_file.write('cat makegrid.dat | ./bin/generate_events pilotrun \n')
        job_file.write('cd ..      \n')
        job_file.write('mkdir process \n')
        job_file.write('mv %s_%s_%s/pilotrun_gridpack.tar.gz                 process  \n' % (args1.name,med,dm))
        job_file.write('mv %s_%s_%s/Events/pilotrun/unweighted_events.lhe.gz process  \n' % (args1.name,med,dm))
        job_file.write('cd process  \n')
        job_file.write('tar xzf pilotrun_gridpack.tar.gz  \n')
        job_file.write('rm pilotrun_gridpack.tar.gz  \n')
        job_file.write('echo "mg5_path = ../../mgbasedir" >> ./madevent/Cards/me5_configuration.txt \n')
        job_file.write('echo "run_mode = 0" >> ./madevent/Cards/me5_configuration.txt \n')  
        if len(spin) > 0: 
            job_file.write('echo "import unweighted_events.lhe.gz"          >  madspinrun.dat \n')
            job_file.write('cat %s                                          >> madspinrun.dat \n' % spin[0])
            job_file.write('cat madspinrun.dat | MadSpin/madspin \n')
            job_file.write('rm madspinrun.dat \n')
            job_file.write('rm unweighted_events.lhe.gz \n')
            job_file.write('rm -rf tmp* \n')
            job_file.write('cp %s/%s/%s process/madspin_card.dat \n' % (basedir,args1.carddir,spin[0]))
        job_file.write('cd .. \n')
        job_file.write('cp    %s/cleangridmore.sh .      \n'  % (basedir))
        job_file.write('cp    %s/runcmsgrid_LO.sh runcmsgrid.sh   \n'  % (basedir))
        job_file.write('./cleangridmore.sh               \n')
        job_file.write('mkdir  mgbasedir     \n')
        job_file.write('cp -r %s/%s_MG5_aMC_v2_2_2/SysCalc  mgbasedir \n' % (basedir,args1.name))
        job_file.write('cp -r %s/%s_MG5_aMC_v2_2_2/input    mgbasedir \n' % (basedir,args1.name))
        output  ='%s_%s_%s_tarball.tar.xz'                    % (args1.name,med,dm)
        job_file.write('XZ_OPT="--lzma2=preset=9,dict=512MiB" tar -cJpsf '+output+' mgbasedir process runcmsgrid.sh \n')
        job_file.write('cp -r %s  %s/%s_MG5_aMC_v2_2_2/      \n' % (output,basedir,args1.name))
        job_file.close()
        os.chmod('%s/%s_MG5_aMC_v2_2_2/MG_%s_%s_%s/integrate.sh' % (basedir,args1.name,args1.name,med,dm),0777)
        os.system('bsub -q  %s -R "rusage[mem=20000000]" %s/%s_MG5_aMC_v2_2_2/MG_%s_%s_%s/integrate.sh' % (args1.queue,basedir,args1.name,args1.name,med,dm))
           
#while not completed(args1.name,args1.medrange,args1.dmrange,basedir,args1.carddir):
#    print "Waiting ",datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
#    time.sleep(60)
