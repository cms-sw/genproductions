#!/usr/bin/env python
import yaml
import json
from os import environ
from sys import argv, exit
from os.path import join
from commands import getstatusoutput as run_cmd

e, o = run_cmd ("find %s -name '*.yaml' -type f" % argv[1])
if e:
  print o
  exit (1)

localtop = environ["CMSSW_BASE"]
files = [ "/src/"+f.split(argv[1],1)[-1][:-5].strip("/") for f in o.split("\n") ]
ignore_files=[]
print "Changed files:",files
for f in o.split("\n"):
  print "Working on",f
  obj = yaml.load(open(f))
  if not obj: obj={"Diagnostics":[]}
  change = 0
  new_dia = []
  if (not "Diagnostics" in obj) or (not obj["Diagnostics"]):
    run_cmd("rm -f %s" % f)
    continue
  for d in obj["Diagnostics"]:
    new_rep = []
    if (not "Replacements" in d) or (not d["Replacements"]): continue
    for r in d["Replacements"]:
      rf = "/"+r["FilePath"].split(localtop,1)[-1].strip("/")
      if rf in files: new_rep.append(r)
      else:
        if not rf in ignore_files:
          ignore_files.append(rf)
          print "  Ignoring file",rf," as it is not part of changed fileset"
        change+=1
    if new_rep: new_dia.append(d)
  if new_dia:
    print "Clang Tidy cleanup: ",f,change
    if change>0:
      obj["Diagnostics"]=new_dia
      ref = open(f,"w")
      ref.write("---\n")
      yaml.dump(obj,ref,default_flow_style=False, width=4096)
      ref.write("...\n")
      ref.close()
  else: run_cmd("rm -f %s" % f)

