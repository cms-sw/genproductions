#!/usr/bin/env python

from collections import namedtuple
import subprocess

Sample = namedtuple("Sample", "name powhegcard powhegmodel")

parametersbase = {
  "-p": "f",
  "-g": "../JHUGen/cards/decay/ZZ4l_withtaus.input",
  "-q": "1nw",
  "-n": "10",
  "-d": "1",
}

samples = [
  Sample("ggH", "gg_H_quark-mass-effects_NNPDF30_14TeV", "gg_H_quark-mass-effects"),
  Sample("WplusH", "HWplusJ_HanythingJ_NNPDF30_14TeV", "HWJ"),
  Sample("WminusH", "HWminusJ_HanythingJ_NNPDF30_14TeV", "HWJ"),
  Sample("minloHJJ", "HJJ_NNPDF30_14TeV", "HJJ"),
]

for name, powhegcard, powhegmodel in samples:
  parameters = {
    "-i": powhegcard+"/"+powhegcard+"_M125.input",
    "-f": name,
    "-m": powhegmodel,
  }
  parameters.update(parametersbase)

  command = ["./run_pwg.py"]
  for k, v in parameters.iteritems():
    command += [k, v]

  subprocess.check_call(command)
