#! /usr/bin/env python
import argparse
import glob
import os
import shutil

aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('-card'       ,'--card'      ,action='store' ,dest='card',default='JHUGen.input'      ,help='card')
aparser.add_argument('-decay-card' ,'--decay-card',action='store' ,dest='card2'  ,default=None             ,help='second input card to run for decay')
aparser.add_argument('-name'       ,'--name'      ,action='store' ,dest='name'   ,required=True            ,help='name')
aparser.add_argument('-s'          ,'--seed'      ,action='store' ,dest='seed'   ,default='123456'         ,help='random seed for grid generation')
aparser.add_argument('-n'          ,'--nevents'   ,action='store' ,dest='nevents',default='100'            ,help='number of events for the test run after grid generation')
args1 = aparser.parse_args()

@contextlib.contextmanager
def cd(newdir):
    """http://stackoverflow.com/a/24176022/5228524"""
    prevdir = os.getcwd()
    os.chdir(os.path.expanduser(newdir))
    try:
        yield
    finally:
        os.chdir(prevdir)

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
        command += line.rstrip('\n')
    command += " VegasNc2=${nevt} Seed=${rnum}"
if args1.card2 is not None:
    command += " DataFile=undecayed && ./JHUGen "
    with open(basedir+'/'+args1.card2,"rt") as flabel:
        for line in flabel:
            command += line.rstrip("\n").replace("ReadCSmax", "")  #remove CSmax so the same card can be used for generating ggH or decaying
    command += " ReadLHE=undecayed.lhe Seed=${rnum}"
command=command+' DataFile=Out'
print command

runcmsgrid = os.path.join(basedir, args1.name+"_JHUGen", "runcmsgrid.sh")
with open(os.path.join(basedir, "runcmsgrid_template.sh")) as f, open(runcmsgrid, "w") as newf:
    contents = (f.read()
                        .replace("GENCOMMAND", command)
                        .replace("BASEDIR", args1.name+"_JHUGen")
                        .replace("SCRAM_ARCH_VERSION_REPLACE", os.environ["SCRAM_ARCH"])
                        .replace("CMSSW_VERSION_REPLACE", os.environ["CMSSW_VERSION"])
               )
    newf.write(contents)

with cd(os.path.join(basedir, args1.name+"_JHUGen")):
    if "ReadCSmax" in command:
        #set up the grid now so it can be read
        #but not the decay part (that is quick anyway)
        runcommand = command.split("&&")[0].replace("ReadCSmax", "").replace("${nevt}", args1.nevents).replace("${rnum}", args1.seed)
        os.system(runcommand)
        for _ in glob.glob("*.lhe"): os.remove(_)
        shutil.rmtree("data/")
    for _ in glob.glob("LSFJOB*"): shutil.rmtree(_)
    shutil.move("runcmsgrid.sh", "..")

with cd(basedir):
    os.system('tar czvf JHUGen_%s_%s_%s.tgz %s_JHUGen runcmsgrid.sh' % (args1.name,args1.name,os.environ["SCRAM_ARCH"],os.environ["CMSSW_VERSION"]))

