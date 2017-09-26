#! /usr/bin/env python
import argparse
import os

aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-card'       ,'--card'      ,action='store' ,dest='card',default='JHUGen.input',help='card')
aparser.add_argument('-decay-card' ,'--decay-card',action='store' ,dest='card2'  ,default=None             ,help='second input card to run for decay')
aparser.add_argument('-name'       ,'--name'      ,action='store' ,dest='name'   ,default='ScalarVH'       ,help='name')
aparser.add_argument('-s'          ,'--seed'      ,action='store' ,dest='seed'   ,default='123456'         ,help='random seed for grid generation')
aparser.add_argument('-n'          ,'--nevents'   ,action='store' ,dest='nevents',default='100'            ,help='number of events for the test run after grid generation')
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
if args1.card2 is not None:
    command += " DataFile=undecayed && ./JHUGen "
    with open(basedir+'/'+args1.card2,"rt") as flabel:
        for line in flabel:
            command += line.rstrip("\n").replace("ReadCSmax", "")  #remove CSmax so the same card can be used for generating ggH or decaying
    command += " ReadLHE=undecayed.lhe"
command=command+' DataFile=Out'
print command

job_file = open('integrate.sh', "wt")
job_file.write('#!/bin/bash\n')
job_file.write('cd %s/%s_JHUGen/ \n'% (basedir,args1.name))
job_file.write('eval `scramv1 runtime -sh` \n')
job_file.write('cp %s/runcmsgrid_template.sh .                                    \n' % (basedir))
job_file.write('sed "s@GENCOMMAND@%s@g"    runcmsgrid_template.sh > runcmsgrid.sh \n' % (command.replace(r"&", r"\&")))
job_file.write('mv runcmsgrid.sh runcmsgrid_template.sh                        \n')
job_file.write('sed "s@VegasNc2=NEVT@VegasNc2=\\$\\{nevt\\}@g"   runcmsgrid_template.sh > runcmsgrid.sh \n')
job_file.write('mv runcmsgrid.sh runcmsgrid_template.sh                        \n')
job_file.write('sed "s@Seed=SEED@Seed=\\$\\{rnum\\}@g"   runcmsgrid_template.sh > runcmsgrid.sh \n')
job_file.write('mv runcmsgrid.sh runcmsgrid_template.sh                        \n')
job_file.write('sed "s@BASEDIR@%s_JHUGen@g"   runcmsgrid_template.sh > runcmsgrid.sh \n'  % (args1.name))

job_file.write('sed -i s/SCRAM_ARCH_VERSION_REPLACE/%s/g runcmsgrid.sh \n'  % (os.environ["SCRAM_ARCH"]))
job_file.write('sed -i s/CMSSW_VERSION_REPLACE/%s/g runcmsgrid.sh \n'  % (os.environ["CMSSW_VERSION"]))

job_file.write('chmod +x runcmsgrid.sh \n')
if "ReadCSmax" in command:
    #set up the grid now so it can be read
    #but not the decay part (that is quick anyway)
    runcommand = command.replace("ReadCSmax", "").replace("NEVT", args1.nevents).replace("SEED", args1.seed).split("&&")[0]
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
os.system('tar czvf JHUGen_%s_%s_%s.tgz %s_JHUGen runcmsgrid.sh' % (args1.name,args1.name,os.environ["SCRAM_ARCH"],os.environ["CMSSW_VERSION"]))
