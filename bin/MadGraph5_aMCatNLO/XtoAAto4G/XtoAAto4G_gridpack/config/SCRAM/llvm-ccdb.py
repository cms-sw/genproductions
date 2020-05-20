#!/usr/bin/env python
from sys import exit, stdout, stderr, argv
from os import getenv
from os.path import join, exists, basename
try: import json
except:import simplejson as json

try: from commands import getstatusoutput as run_cmd
except:
  try: from subprocess import getstatusoutput  as run_cmd
  except:
    def run_cmd(command2run):
      from subprocess import Popen, PIPE, STDOUT
      cmd = Popen(command2run, shell=True, stdout=PIPE, stderr=STDOUT)
      (output, errout) = cmd.communicate()
      if isinstance(output,bytes): output =  output.decode()
      if output[-1:] == '\n': output = output[:-1]
      return (cmd.returncode, output)

def print_msg(msg,stream=stdout,newline="\n"): stream.write(msg+newline)
try: LLVM_CCDB_NAME = argv[1]
except: LLVM_CCDB_NAME = "compile_commands.json"

llvm_ccdb   = []
local_files = []
localtop    = getenv("LOCALTOP")
arch        = getenv("SCRAM_ARCH")
tmpsrc      = join(localtop,"tmp",arch,"src")

#Read SCRAM Generated
if exists (tmpsrc):
  err, llvm_ccdb_files = run_cmd("find %s -name '*.%s' -type f" % (tmpsrc, LLVM_CCDB_NAME))
  if err:
    print_msg(llvm_ccdb_files)
    exit(err)
  if llvm_ccdb_files:
    for llvm_ccdb_file in llvm_ccdb_files.split("\n"):
      obj = json.load(open(llvm_ccdb_file))
      if obj['file'] in local_files: continue
      local_files.append(obj['file'])
      llvm_ccdb.append(obj)

release_top = getenv("RELEASETOP",None)
if not release_top:
  proj_name = getenv("SCRAM_PROJECTNAME")
  proj_ver = getenv("SCRAM_PROJECTVERSION")
  err, full_release = run_cmd("scram tool info %s | grep '^%s_BASE=' | sed 's|%s_BASE=||'" %  (proj_name.lower(), proj_name, proj_name))
  if full_release:
    rel_llvm_ccdb_file = join(full_release,LLVM_CCDB_NAME)
    if exists(rel_llvm_ccdb_file):
      full_ver = basename(full_release)
      for obj in json.load(open(rel_llvm_ccdb_file)):
        obj_dir  = obj['directory']
        obj_file = obj['file'].replace(obj_dir, localtop)
        if obj_file in local_files: continue
        obj['directory'] = localtop
        obj['command']   = obj['command'].replace(obj_dir, localtop).replace(full_ver,proj_ver)
        llvm_ccdb.append(obj)

print_msg(json.dumps(llvm_ccdb, indent=2, sort_keys=True, separators=(',',': ')),
          open(join(localtop,LLVM_CCDB_NAME),"w"),newline="")

