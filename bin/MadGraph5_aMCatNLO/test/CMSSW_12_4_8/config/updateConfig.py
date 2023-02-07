#!/usr/bin/env python3

import sys
from argparse import ArgumentParser
from os import path
from subprocess import call

base = sys.argv[0]
help_text = "\n\
    %s --project <name> --version <version> --scram <scram version>\n\
    --toolbox <toolbox> [--config <dir>] [--help]\n\n\
    This script will copy all Projects/<name>/<files> files into <files>\n\
    and replace project names, version, scram verion, toolbox path\n\
    and extra keys/values provided via the command line. e.g.\n\n\
    %s -p CMSSW -v CMSSW_4_5_6 -s V1_2_0 -t /path/cmssw-tool-conf/CMS170\
 --keys MYSTRING1=MYVALUE1 --keys MYSTRING2=MYVALUE2\n\n\
    will release:\n\
    @PROJECT_NAME@=CMSSW\n\
    @PROJECT_VERSION@=CMSSW_4_5_6\n\
    @SCRAM_VERSION@=V1_2_0\n\
    @PROJECT_TOOL_CONF@=/path/cmssw-tool-conf/CMS170\n\
    @MYSTRING1@=MYVALUE1\n\
    @MYSTRING2@=MYVALUE2\n\n" % (base, base)
parser = ArgumentParser(usage=help_text)
parser.add_argument('--project', '-p', dest='project', required=True, help='Missing or empty project name.')
parser.add_argument('--version', '-v', dest='version', required=True, help='Missing or empty project version.')
parser.add_argument('--scram', '-s', dest='scram', required=True, help="Missing or empty scram version.")
parser.add_argument('--toolbox', '-t', dest='toolbox', required=True, help="Missing or empty SCRAM tool box path.")
parser.add_argument('--keys', dest='keys', action='append')
args = parser.parse_args()
keys = dict(s.split('=') for s in args.keys)

tooldir = "tools"
if not path.isdir(path.join(args.toolbox, tooldir)):
    raise Exception("Wrong toolbox directory. Missing directory %s." % path.join(args.toolbox, tooldir))

dir = path.dirname(path.realpath(__file__))
pdir = path.join(dir, "Projects", args.project)
if not path.isdir(pdir):
    raise Exception("No project directory found: %s" % (pdir))
cache = {
    "PROJECT_NAME": args.project,
    "PROJECT_VERSION": args.version,
    "PROJECT_TOOL_CONF": args.toolbox,
    "SCRAM_VERSION": args.scram,
    "PROJECT_CONFIG_BASE": path.dirname(dir),
    "SCRAM_COMPILER": "gcc",
    "PROJECT_GIT_HASH": {}
}

for k in keys:
    cache[k] = keys[k]
if not cache["PROJECT_GIT_HASH"]:
    cache["PROJECT_GIT_HASH"] = args.version

regexp = ""
for k in cache.keys():
    v = cache[k]
    regexp += "s|@%s@|%s|g;" % (k, v)

call("find %s -name \"*\" -type f | xargs sed -i.backup -e '%s'" % (pdir, regexp), shell=True)
call("rm -rf {0}/*.backup; mv {0}/* {1}/; rm -rf {1}/Projects".format(pdir, dir), shell=True)
with open("%s/scram_version" % dir, "w") as fh:
    fh.write(args.scram)
