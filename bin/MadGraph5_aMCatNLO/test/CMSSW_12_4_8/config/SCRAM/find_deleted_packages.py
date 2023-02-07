#!/usr/bin/env python3
from sys import argv, exit, stderr
from os.path import exists
import re
from subprocess import getstatusoutput as run_cmd


try:    tag = argv[1]
except:
  from os import getenv
  tag = getenv("CMSSW_GIT_HASH", None)
  if not tag: exit(0)

try:    sparse_file = argv[2]
except:
  sparse_file = ".git/info/sparse-checkout"
  if not exists(sparse_file): exit(0)

checkout_pkgs = [ re.compile("^"+line.strip("\n").replace("/*/","/.*/").strip("/")+"/.+$") for line in open(sparse_file).readlines() if line.strip("\n")[-1]=="/" ]
e, o = run_cmd ("git diff --name-only %s -- | cut -d/ -f1,2 | sort -u" % tag)
if e:
  print(o,file=stderr)
  exit(1)

for pkg in o.split("\n"):
  if not "/" in pkg: continue
  for regex in checkout_pkgs:
    if regex.match(pkg+"/abc"):
      if not exists (pkg): print(pkg)
      break

