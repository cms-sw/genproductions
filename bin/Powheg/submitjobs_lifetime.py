#!/usr/bin/env python

import os
import subprocess

parametersbase = {
  "-p": "f",
  "-q": "2nw",
  "-n": "10",
  "-d": "1",
  "-m": "ZZ",
  "-i": "production/2017/13TeV/ZZTo4L/ZZ_4L_NNPDF31_13TeV.input",
}

samples = [
#  ("ZZ4l_lifetime_NNPDF31_13TeV_50um", 50),
#  ("ZZ4l_lifetime_NNPDF31_13TeV_200um", 200), 
  ("ZZ4l_lifetime_NNPDF31_13TeV_800um", 800),
]

for name, time in samples:
  parameters = {
    "-g": os.path.join("../JHUGen", "cards", "2017_Part2", "13TeV", "decay", "lifetime", "ZZ4l_withtaus_CTau"+str(time)+"um.input"),
    "-f": name
  }
  parameters.update(parametersbase)
  print parameters["-g"]
  assert os.path.exists(parameters["-i"]) and os.path.exists(parameters["-g"])

  command = ["./run_pwg.py"]
  for k, v in parameters.iteritems():
    command += [k, v]
  print ' '.join(command)

  subprocess.check_call(command)
