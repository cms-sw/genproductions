#!/usr/bin/env python3
import yaml
import json
from os import environ
from sys import argv, exit
from os.path import join
from subprocess import getstatusoutput as run_cmd

e, o = run_cmd ("find %s -name '*.yaml' -type f" % argv[1])
if e:
  print(o)
  exit (1)

localtop = environ["CMSSW_BASE"]
files = [ "/src/"+f.split(argv[1],1)[-1][:-5].strip("/") for f in o.split("\n") ]
ignore_files=[]
track_changes = {}
for f in o.split("\n"):
  print (">> Processing",f)
  obj = yaml.load(open(f), Loader=yaml.SafeLoader)
  if not obj: obj={"Diagnostics":[]}
  if ("Diagnostics" not in obj) or (not obj["Diagnostics"]):
    print("  Deleting: No Diagnostics found")
    run_cmd("rm -f %s" % f)
    continue
  change = 0
  new_dia = []
  atN = False
  for d in obj["Diagnostics"]:
    d1 = d
    if 'DiagnosticMessage' in d: d1 = d['DiagnosticMessage']
    if ("FilePath" not in d1): continue
    rf = "/"+d1["FilePath"].split(localtop,1)[-1].strip("/")
    if rf not in files:
      if rf not in ignore_files:
        ignore_files.append(rf)
        print ("  Ignoring file",rf," as it is not part of changed fileset")
      change+=1
      continue
    if ("FileOffset" not in d1): continue
    dia_key='%s:%s:%s' % (d["DiagnosticName"], d1['FilePath'], d1['FileOffset'])
    if dia_key in track_changes:
      print ("  Dropping %s from %s. found in %s" % (dia_key, f, track_changes[dia_key]))
      change+=1
      continue
    track_changes[dia_key] = f
    ds = [d1]
    if ("Notes" in d): ds = ds + d["Notes"]
    new_rep = False
    for xd in ds:
      if ("Replacements" not in xd) or (not xd["Replacements"]): continue
      new_rep = True
      for r in xd["Replacements"]:
        if d["DiagnosticName"] in ['readability-braces-around-statements']:
          if r['ReplacementText'] == ' }':
            r['ReplacementText']='@N@}'
            atN = True
            change+=1
        elif d["DiagnosticName"] in ['performance-inefficient-vector-operation']:
          r['ReplacementText'] = r['ReplacementText'].replace('\n','@N@ ')
          atN = True
          change+=1
    if new_rep: new_dia.append(d)
  if new_dia:
    print ("  Clang Tidy cleanup: ",f,change)
    if change>0:
      obj["Diagnostics"]=new_dia
      ref = open(f,"w")
      ref.write("---\n")
      yaml.dump(obj,ref,default_flow_style=False, width=4096)
      ref.write("...\n")
      ref.close()
      if atN:
        run_cmd('sed -i -e \'s|@N@|\\n|g\' %s' % f)
  else: run_cmd("rm -f %s" % f)

