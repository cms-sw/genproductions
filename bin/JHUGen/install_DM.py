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

def completed(name,medrange,dmrange,basedir,carddir,resubmit):
    completed = True
    for med in medrange:
        for dm in dmrange:
            #no generation offshell
            if med < 2*dm: 
                continue
            fileExists=os.path.isfile('JHUGen_%s_%s_%s/runcmsgrid.sh' % (name,med,dm)) 
            if fileExists:
                os.system('cp JHUGen_%s_%s_%s/runcmsgrid.sh .'  % (name,med,dm))
                os.chmod('runcmsgrid.sh',0777)
                os.system('tar czvf  JHUGen_%s_%s_%s.tgz  JHUGen_%s_%s_%s runcmsgrid.sh' % (name,med,dm,name,med,dm))
                os.system('rm -r JHUGen_%s_%s_%s ' % (name,med,dm))
                os.system('mv JHUGen_%s_%s_%s.tgz ../' % (name,med,dm))
            else:
                if not os.path.isfile('JHUGen_%s_%s_%s.tgz' % (name,med,dm)):
                    completed = False
                    if resubmit:
                        os.system('bsub -q %s %s/%s_JHUGen/JHUGen_%s_%s_%s/integrate.sh' % (args1.queue,basedir,args1.name,args1.name,med,dm))
    return completed

def getXS(iMed,command,basedir):
    label="ZH"
    if command.find("11") > -1:
        label="WH"
    lFile  = ROOT.TFile(basedir+'/patches/WZXS.root')
    lG     = lFile.Get(label)
    lScale = lFile.Get("scaleUp")
    lBR    = lFile.Get("BRbb")
    #Correct for the BR to fermions assuming Scalar decays to bosons
    BRCorr = lBR.Eval(iMed)*246.*246./4.8/4.8
    scale=int(iMed)+91+15 # 15 is an approximation of the extra energy based on matching xsections at 125
    if label.find("WH") > -1:
        scale=int(iMed)+80+15
    return lG.Eval(iMed)*lScale.Eval(scale)*BRCorr


aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-resubmit','--resubmit'  ,type=bool      ,dest='resubmit',default=False,help='resubmit')
aparser.add_argument('-retar'   ,'--retar'     ,type=bool      ,dest='retar'   ,default=False,help='tar up')
aparser.add_argument('-carddir' ,'--carddir'   ,action='store' ,dest='carddir',default='cards/ScalarVH/',help='carddir')
aparser.add_argument('-name'    ,'--name'      ,action='store' ,dest='name'   ,default='ScalarVH'       ,help='name')
aparser.add_argument('-q'       ,'--queue'     ,action='store' ,dest='queue'  ,default='1nh'            ,help='queue')
aparser.add_argument('-dm'      ,'--dmrange'   ,dest='dmrange' ,nargs='+',type=int,     default=[1,10,50,100,500],    help='mass range')
aparser.add_argument('-med'     ,'--medrange'  ,dest='medrange',nargs='+',type=int,     default=[10,50,100,150,200,300,500,1000,1500],help='mediator range')
args1 = aparser.parse_args()

print args1.carddir,args1.name,args1.queue,args1.medrange

basedir=os.getcwd()
#Start with the basics download MCFM and add the options we care  :
if not args1.retar and not args1.resubmit:
    os.system('cp patches/install_DM.sh .')
    os.system('./install_DM.sh')
    os.system('mv JHUGenerator %s_JHUGen' % args1.name)

##Get the base files
if not args1.retar and not args1.resubmit:
    parameterfiles = [ f for f in listdir(basedir+'/'+args1.carddir) if isfile(join(basedir+'/'+args1.carddir,f)) ]
    for f in parameterfiles:
        if f.find('F90') > -1:
            os.system('cp '+basedir+'/'+args1.carddir+'/'+f+(' %s_JHUGen' % args1.name))

os.chdir('%s_JHUGen' % (args1.name))
if not args1.retar and not args1.resubmit:
    os.system('make')
    os.system('cp JHUGen Bin')

command=''
if not args1.retar and not args1.resubmit:
    with open(basedir+'/'+args1.carddir+'/generate.sh',"rt")         as flabel: 
        for line in flabel:
            command=line
            break
#Now build the directories iterating over options
random.seed(1)
if not args1.retar and not args1.resubmit:
    for dm  in args1.dmrange:
        for med in args1.medrange:
        #Generation offshell does not yet work
            if med < dm*2:
                continue
            os.system('cp -r Bin JHUGen_%s_%s_%s' % (args1.name,med,dm))
            for f in parameterfiles:
                with open('JHUGen_%s_%s_%s/%s' % (args1.name,med,dm,f), "wt") as fout: 
                    with open(basedir+'/'+args1.carddir+'/'+f,"rt")         as fin: 
                        for line in fin:
                            tmpline =    line.replace('MED' ,str(med))
                            tmpline = tmpline.replace('MASS',str(dm))
                            tmpline = tmpline.replace('RAND',str(random.randrange(1000,9999,1)))
                            fout.write(tmpline)
            xs=getXS(med,command,basedir)
            job_file = open('JHUGen_%s_%s_%s/integrate.sh' % (args1.name,med,dm), "wt")
            job_file.write('#!/bin/bash\n')
            job_file.write('cd %s/%s_JHUGen/JHUGen_%s_%s_%s/ \n'% (basedir,args1.name,args1.name,med,dm))
            job_file.write('eval `scramv1 runtime -sh` \n')
            job_file.write('chmod +x runxs.sh \n')
            job_file.write('./runxs.sh \n')
            job_file.write("xs=`cat xsfile   | awk '{print $1*%s}' ` \n" % xs)
            job_file.write("xsun=`cat xsfile | awk '{print $2*%s}' ` \n" % xs)
            job_file.write('cp %s/runcmsgrid_template_DM.sh .                                    \n' % (basedir))
            job_file.write('sed "s@GENCOMMAND@%s@g"    runcmsgrid_template_DM.sh > runcmsgrid.sh \n' % (command.replace('MED',str(med))))
            job_file.write('mv runcmsgrid.sh runcmsgrid_template_DM.sh                        \n')
            job_file.write('sed "s@XSECTION@$xs@g"     runcmsgrid_template_DM.sh > runcmsgrid.sh \n')
            job_file.write('mv runcmsgrid.sh runcmsgrid_template_DM.sh                        \n')
            job_file.write('sed "s@XSECUNC@$xsun@g"   runcmsgrid_template_DM.sh > runcmsgrid.sh \n')
            job_file.write('mv runcmsgrid.sh runcmsgrid_template_DM.sh                        \n')
            job_file.write('sed "s@VegasNc2=50000@VegasNc2=\\$\\{nevt\\}@g"   runcmsgrid_template_DM.sh > runcmsgrid.sh \n')
            job_file.write('mv runcmsgrid.sh runcmsgrid_template_DM.sh                        \n')
            job_file.write('sed "s@BASEDIR@JHUGen_%s_%s_%s@g"   runcmsgrid_template_DM.sh > runcmsgrid.sh \n'  % (args1.name,med,dm))
            job_file.write('chmod +x runcmsgrid.sh \n')
            job_file.write('rm *.lhe \n')
            job_file.close()
            os.chmod('JHUGen_%s_%s_%s/integrate.sh' % (args1.name,med,dm),0777)
            os.system('bsub -q %s %s/%s_JHUGen/JHUGen_%s_%s_%s/integrate.sh' % (args1.queue,basedir,args1.name,args1.name,med,dm))
           
while not completed(args1.name,args1.medrange,args1.dmrange,basedir,args1.carddir,args1.resubmit):
    print "Waiting ",datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    time.sleep(60)
