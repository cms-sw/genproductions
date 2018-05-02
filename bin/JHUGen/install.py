#! /usr/bin/env python
import argparse
import contextlib
import glob
import os
import shutil
import subprocess

aparser = argparse.ArgumentParser(description='Process benchmarks.')
aparser.add_argument('--card', '-c', action='store', required=True, help='card')
aparser.add_argument('--decay-card', '-d', action='store', help='second input card to run for decay')
aparser.add_argument('--name', action='store', required=True, help='name')
aparser.add_argument('--seed', '-s', action='store', default='123456', help='random seed for grid generation')
aparser.add_argument('--nevents', '-n', action='store', default='100', help='number of events for the test run after grid generation')
aparser.add_argument('--force', '-f', action='store_true', help='overwrite existing folder')
aparser.add_argument('--link-mela', action='store_true', help='indicate that the process requires MCFM libraries from MELA')
args = aparser.parse_args()

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
JHUbasedir = os.path.join(basedir, args.name+"_JHUGen")
if os.path.exists(JHUbasedir):
    if args.force:
        shutil.rmtree(JHUbasedir)
    else:
        raise OSError("{} already exists.  To overwrite, use the --force option".format(JHUbasedir))
os.mkdir(JHUbasedir)
#Start with the basics download MCFM and add the options we care  :
with cd(JHUbasedir):
    subprocess.check_call([os.path.join(basedir, "patches", "install.sh"), "true" if args.link_mela else "false"])

##Get the base files
shutil.copy(os.path.join(basedir, args.card), os.path.join(JHUbasedir, "JHUGen.input"))

command="./JHUGen $(cat ../JHUGen.input) VegasNc2=${nevt} Seed=${rnum}"
if args.decay_card is not None:
    with open(os.path.join(basedir, args.decay_card)) as f, open(os.path.join(JHUbasedir, "JHUGen_decay.input"), "w") as newf:
        newf.write(f.read().replace("ReadCSmax", ""))
    command += " DataFile=undecayed &&\n./JHUGen $(cat ../JHUGen_decay.input) Seed=${rnum} ReadLHE=undecayed.lhe Seed=${rnum}"
command += " DataFile=Out"
########################
#backwards compatibility
command = command.replace("Seed=SEED", "")
########################
print command
#Note the same seed is used twice.  This sounds bad but the JHUGen processes are completely independent and use the seed in different ways.

runcmsgrid = os.path.join(JHUbasedir, "runcmsgrid.sh")
#runcmsgrid = os.path.join(basedir, "runcmsgrid.sh")
with open(os.path.join(basedir, "runcmsgrid_template.sh")) as f, open(runcmsgrid, "w") as newf:
    contents = (f.read()
                        .replace("GENCOMMAND", command)
                        .replace("BASEDIR", "JHUGenerator")
                        #.replace("BASEDIR", args.name+"_JHUGen/JHUGenerator")
                        .replace("SCRAM_ARCH_VERSION_REPLACE", os.environ["SCRAM_ARCH"])
                        .replace("CMSSW_VERSION_REPLACE", os.environ["CMSSW_VERSION"])
               )
    newf.write(contents)
os.chmod(runcmsgrid, 0777)

with cd(os.path.join(JHUbasedir, "JHUGenerator")), open("../JHUGen.input") as f:
    if "ReadCSmax" in f.read():
        #set up the grid now so it can be read
        #but not the decay part (that is quick anyway)
        runcommand = command.split("&&")[0].replace("${nevt}", args.nevents).replace("${rnum}", args.seed) + " NoReadCSmax"
        os.system(runcommand)
        for _ in glob.glob("*.lhe"): os.remove(_)
        shutil.rmtree("data/")

os.chdir(JHUbasedir)
cmd="tar czf "+ "../JHUGen_%s_%s_%s.tgz" % (args.name, os.environ["SCRAM_ARCH"], os.environ["CMSSW_VERSION"])+ " *"
print cmd
os.system(cmd)
os.chdir(basedir)
#with cd(basedir):
#with cd(JHUbasedir):
#    subprocess.check_call(["tar", "czvf", "../JHUGen_%s_%s_%s.tgz" % (args.name, os.environ["SCRAM_ARCH"], os.environ["CMSSW_VERSION"]), "*"])
#    #subprocess.check_call(["tar", "czvf", "JHUGen_%s_%s_%s.tgz" % (args.name, os.environ["SCRAM_ARCH"], os.environ["CMSSW_VERSION"]), args.name+"_JHUGen", "runcmsgrid.sh"])
