#!/usr/bin/env python

import os
import subprocess

parametersbase = {
  "-p": "f",
  "-g": "../JHUGen/cards/decay/ZZ4l_withtaus.input",
  "-q": "1nw",
  "-n": "10",
  "-d": "1",
}

samples = [
  ("ggH", "gg_H_quark-mass-effects_NNPDF30_14TeV", "gg_H_quark-mass-effects"),
  ("WplusH", "HWplusJ_HanythingJ_NNPDF30_14TeV", "HWJ"),
  ("WminusH", "HWminusJ_HanythingJ_NNPDF30_14TeV", "HWJ"),
  ("minloHJJ", "HJJ_NNPDF30_14TeV", "HJJ"),
]

for name, powhegcard, powhegmodel in samples:
  parameters = {
    "-i": os.path.join("production", "pre2017", "14TeV", powhegcard, powhegcard+"_M125.input"),
    "-f": name,
    "-m": powhegmodel,
  }
  parameters.update(parametersbase)
  assert os.path.exists(parameters["-i"]) and os.path.exists(parameters["-g"])

  command = ["./run_pwg.py"]
  for k, v in parameters.iteritems():
    command += [k, v]

  subprocess.check_call(command)
