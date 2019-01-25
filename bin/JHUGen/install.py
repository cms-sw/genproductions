#! /usr/bin/env python
import argparse
import contextlib
import glob
import os
import pipes
import re
import shutil
import subprocess
import textwrap

JHUGenversion = "v7.2.6"

if __name__ == "__main__":
  aparser = argparse.ArgumentParser(description='Make JHUGen gridpack')
  aparser.add_argument('--card', '-c', action='store', help='card')
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
  if not args.card and not args.check_jobs:
    aparser.error("have to provide a card (unless checking jobs for VBF offshell)")

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

        find = "(^MELADir *= *).*"
        assert re.search(find, makefile, flags=re.MULTILINE)
        makefile = re.sub(find, r"\1../JHUGenMELA/MELA", makefile, flags=re.MULTILINE) #use the RELATIVE path.  make creates symlinks that have to be valid wherever the gridpack is opened.

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
      tosubmit = []
      for i in range(1, 6):
        gridfile = "Out_{}_step2.grid".format(i)
        try:
          with open(gridfile+".log") as f:
            log = f.read()
            jobid = re.search("^000 [(]([0-9]*[.][0-9]*)[.][0-9]*[)]", log)
            if not jobid:
              raise RuntimeError("Don't know how to interpret "+os.path.relpath(gridfile+".log", basedir)+", can't find the jobid in the file.")
            jobid = jobid.group(1)

            error = None
            if "Job terminated" in log:
              if "return value 0" in log:
                if os.path.exists(gridfile):
                  continue
                else:
                  error = "finished with exit code 0, but without creating the grid file"
              else:
                if os.path.exists(gridfile):
                  error = "finished with a nonzero exit code.  It seems to have created a grid file anyway, but the grid may be buggy"
                else:
                  error = "finished with a nonzero exit code"
            elif "Job was aborted" in log:
              if os.path.exists(gridfile):
                error = "was aborted.  It seems to have created a grid file anyway, but the grid may be buggy"
              else:
                error = "was aborted"

            if error:
              raise RuntimeError("Job " + str(jobid) + " " + error + ".  Check the log, output, and error.  If you change the input card, you have to delete the whole directory and start over.  If you think it will work this time with no change, or if you edit the install.py script, remove " + os.path.relpath(gridfile+".log", basedir) + " and run install.py again.")
            else:
              grids.append(i)
              jobids.append(jobid)
              continue
        except IOError:
          pass

        if os.path.exists(gridfile):
          continue

        grids.append(i)
        tosubmit.append(str(i))

      if tosubmit:
        job = textwrap.dedent("""\
          #!/bin/bash
          set -euo pipefail
          cd {CMSSW_BASE}
          eval $(scram ru -sh)
          cd {JHUbasedir}/JHUGenerator
          eval $(../JHUGenMELA/MELA/setup.sh env)
          {command}
        """.format(
          CMSSW_BASE=os.environ["CMSSW_BASE"],
          JHUbasedir=JHUbasedir,
          command=command.replace("${nevt}", args.nevents).replace("${rnum}", args.seed) + " NoReadCSmax VBFoffsh_run=$1"
        ).rstrip())
        with open("condorgridscript.sh", "w") as f:
          f.write(job)

        submit = textwrap.dedent("""\
          executable              = condorgridscript.sh
          arguments               = $(i)

          output                  = Out_$(i)_step2.grid.out
          error                   = Out_$(i)_step2.grid.err
          log                     = Out_$(i)_step2.grid.log

          request_memory          = 4000M
          +JobFlavour             = "nextweek"

          #https://www-auth.cs.wisc.edu/lists/htcondor-users/2010-September/msg00009.shtml
          periodic_remove         = JobStatus == 5
          WhenToTransferOutput    = ON_EXIT_OR_EVICT

          queue i in {ilist}
        """.format(ilist=" ".join(tosubmit)).rstrip())

        with open("condor.sub", "w") as f:
          f.write(submit)

        output = subprocess.check_output(["condor_submit", "condor.sub"])
        print output,
        match = re.search("([0-9]) job[(]s[)] submitted to cluster ([0-9]+)", output)
        try:
          njobs = int(match.group(1))
          mainjobid = int(match.group(2))
          jobids += ["{}.{}".format(mainjobid, i) for i in range(njobs)]
        except (TypeError, ValueError, AttributeError):
          #in case something weird happens in the output or regex
          jobids.append("(unknown)")

      if grids:
        print "jobs", ", ".join(str(_) for _ in jobids), "are running to create grids", ", ".join(str(_) for _ in grids)
        print "run install.py again with the --check-jobs option to finish up once they're done"
        return

      for i in range(1, 6):
        try:
          os.remove("Out_{}.lhe".format(i))
        except OSError:
          pass

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
