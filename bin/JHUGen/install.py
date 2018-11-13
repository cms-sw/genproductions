#! /usr/bin/env python
import argparse
import contextlib
import glob
import os
import pipes
import re
import shutil
import subprocess

JHUGenversion = "v7.2.3"

if __name__ == "__main__":
  aparser = argparse.ArgumentParser(description='Make JHUGen gridpack')
  aparser.add_argument('--card', '-c', action='store', required=True, help='card')
  aparser.add_argument('--decay-card', '-d', action='store', help='second input card to run for decay')
  aparser.add_argument('--name', action='store', required=True, help='name')
  aparser.add_argument('--seed', '-s', action='store', default='123456', help='random seed for grid generation')
  aparser.add_argument('--nevents', '-n', action='store', default='100', help='number of events for the test run after grid generation')
  aparser.add_argument('--link-mela', action='store_true', help='indicate that the process requires MCFM or Collier libraries from MELA')
  aparser.add_argument('--vbf-offshell', action='store_true', help='indicate that the process is offshell VBF')
  group = aparser.add_mutually_exclusive_group()
  group.add_argument('--force', '-f', action='store_true', help='overwrite existing folder')
  group.add_argument('--check-jobs', action='store_true', help='check if the jobs for VBF offshell are done, and if they are finish the tarball')
  args = aparser.parse_args()

  if args.vbf_offshell and not args.link_mela:
    aparser.error("--vbf-offshell also requires --link-mela")
  if args.check_jobs and not args.vbf_offshell:
    aparser.error("--check-jobs only makes sense for --vbf-offshell")

def jobended(*bjobsargs):
  try:
    bjobsout = subprocess.check_output(["bjobs"]+list(bjobsargs), stderr=subprocess.STDOUT)
  except subprocess.CalledProcessError:
    return True
  if re.match("Job <.*> is not found", bjobsout.strip()):
    return True
  lines = bjobsout.strip().split("\n")
  if len(lines) == 2 and lines[1].split()[2] in ("EXIT", "DONE"):
    return True

  return False

@contextlib.contextmanager
def cd(newdir):
  """http://stackoverflow.com/a/24176022/5228524"""
  prevdir = os.getcwd()
  os.chdir(os.path.expanduser(newdir))
  try:
    yield
  finally:
    os.chdir(prevdir)

def main(args):
  basedir=os.getcwd()
  JHUbasedir = os.path.join(basedir, args.name+"_JHUGen")
  if os.path.exists(JHUbasedir):
    if args.force:
      shutil.rmtree(JHUbasedir)
    elif args.check_jobs:
      pass
    else:
      errortext = "{} already exists.  To overwrite, use the --force option.".format(JHUbasedir)
      if args.vbf_offshell:
        errortext += "  To check the status of the jobs, use the --check-jobs option."
      raise OSError(errortext)

  if args.link_mela:
    os.environ["LD_LIBRARY_PATH"] = (os.path.join(JHUbasedir, "JHUGenMELA", "MELA", "data", "slc6_amd64_gcc530") + ":" + os.environ["LD_LIBRARY_PATH"]).rstrip(":")

  command="./JHUGen $(cat ../JHUGen.input) VegasNc2=${nevt} Seed=${rnum}"
  if args.decay_card is not None:
    command += " DataFile=undecayed &&\n./JHUGen $(cat ../JHUGen_decay.input) Seed=${rnum} ReadLHE=undecayed.lhe Seed=${rnum}"
  command += " DataFile=Out"
  ########################
  #backwards compatibility
  command = command.replace("Seed=SEED", "")
  ########################
  print command
  #Note the same seed is used twice.  This sounds bad but the JHUGen processes are completely independent and use the seed in different ways.

  if not args.check_jobs:
  #download and compile
    os.mkdir(JHUbasedir)
    with cd(JHUbasedir):
      subprocess.check_call(["wget", "--no-check-certificate", "http://spin.pha.jhu.edu/Generator/JHUGenerator."+JHUGenversion+".tar.gz", "-O", "JHUGenerator.tar.gz"])
      subprocess.check_call(["tar", "xvzf", "JHUGenerator.tar.gz"])
      os.remove("JHUGenerator.tar.gz")
      os.remove("manJHUGenerator."+JHUGenversion+".pdf")
      shutil.rmtree("AnalyticMELA")
      if args.link_mela:
        with cd("JHUGenMELA/MELA"):
          subprocess.check_call(["./setup.sh"])
      else:
        shutil.rmtree("JHUGenMELA")
      with cd("JHUGenerator"):
        with open("makefile") as f:
          makefile = f.read()

        find = "(^linkMELA *= *)(Yes|No)"
        assert re.search(find, makefile, flags=re.MULTILINE)
        makefile = re.sub(find, r"\1"+("Yes" if args.link_mela else "No"), makefile, flags=re.MULTILINE)

        find = "(^UseLHAPDF *= *)(Yes|No)"
        assert re.search(find, makefile, flags=re.MULTILINE)
        makefile = re.sub(find, r"\1Yes", makefile, flags=re.MULTILINE)

        with open("makefile", "w") as f:
          f.write(makefile)
        os.system("make")

    ##Get the base files
    shutil.copy(os.path.join(basedir, args.card), os.path.join(JHUbasedir, "JHUGen.input"))

    if args.decay_card is not None:
      with open(os.path.join(basedir, args.decay_card)) as f, open(os.path.join(JHUbasedir, "JHUGen_decay.input"), "w") as newf:
        newf.write(f.read().replace("ReadCSmax", ""))

    runcmsgrid = os.path.join(JHUbasedir, "runcmsgrid.sh")
    if args.vbf_offshell:
      template = "runcmsgrid_template_VBFoffshell.sh"
    else:
      template = "runcmsgrid_template.sh"

    with open(os.path.join(basedir, template)) as f, open(runcmsgrid, "w") as newf:
      contents = (f.read()
                   .replace("GENCOMMAND", command)
                   .replace("SCRAM_ARCH_VERSION_REPLACE", os.environ["SCRAM_ARCH"])
                   .replace("CMSSW_VERSION_REPLACE", os.environ["CMSSW_VERSION"])
      )
      newf.write(contents)
    os.chmod(runcmsgrid, 0777)

  with cd(os.path.join(JHUbasedir, "JHUGenerator")), open("../JHUGen.input") as f:
    if args.vbf_offshell:
      #set up the VBF offshell grids
      grids = []
      jobids = []
      for i in range(1, 6):
        gridfile = "Out_{}_step2.grid".format(i)
        try:
          with open(gridfile+".tmp") as f:
            jobid = int(f.read())
            if jobended(str(jobid)):
              if not os.path.exists(gridfile):
                raise RuntimeError("Job " + str(jobid) + " died without creating the grid file.  Check the log.  If you change the input card, you have to delete the whole directory and start over.  If you think it will work this time with no change, remove " + os.path.relpath(gridfile+".tmp", basedir) + " and run install.py again.")
              os.remove(gridfile+".tmp")
            else:
              grids.append(i)
              jobids.append(jobid)
              continue
        except IOError:
          pass

        if os.path.exists(gridfile):
          continue

        job = " && ".join((
          "cd " + os.environ["CMSSW_BASE"],
          "eval $(scram ru -sh)",
          "cd " + JHUbasedir + "/JHUGenerator",
          command.replace("${nevt}", args.nevents).replace("${rnum}", args.seed) + " NoReadCSmax VBFoffsh_run="+str(i),
        ))
        jobname = args.name+"_JHUGen_"+str(i)

        output = subprocess.check_output("echo "+pipes.quote(job)+" | bsub -q 1nw -J " + jobname, shell=True)
        print output
        grids.append(i)
        jobids.append(int(output.split("<")[1].split(">")[0]))
        with open(gridfile+".tmp", "w") as f:
          f.write(str(jobids[-1]))

      if grids:
        print "jobs", ", ".join(str(_) for _ in jobids), "are running to create grids", ", ".join(str(_) for _ in grids)
        print "run install.py again with the --check-jobs option to finish up once they're done"
        return

      os.remove("Out_1.lhe")
      os.remove("Out_2.lhe")
      os.remove("Out_3.lhe")
      os.remove("Out_4.lhe")
      os.remove("Out_5.lhe")

    elif "ReadCSmax" in f.read():
      #set up the grid now so it can be read
      #but not the decay part (that is quick anyway)
      runcommand = command.split("&&")[0].replace("${nevt}", args.nevents).replace("${rnum}", args.seed) + " NoReadCSmax"
      os.system(runcommand)
      for _ in glob.glob("*.lhe"): os.remove(_)
      shutil.rmtree("data/")

  with cd(JHUbasedir):
    subprocess.check_call(["tar", "czvf", "../JHUGen_%s_%s_%s.tgz" % (args.name, os.environ["SCRAM_ARCH"], os.environ["CMSSW_VERSION"])] + glob.glob("*"))

if __name__ == "__main__":
  main(args)
