#!/usr/bin/env python3
import sys, re, os, json, gzip, shutil
from argparse import ArgumentParser
from os import path, environ, stat, unlink, utime
from subprocess import  call, check_output

parser = ArgumentParser()
parser.add_argument('olddir', help="Missing old installation path")
parser.add_argument('newtop', help="Missing current installation path")
parser.add_argument('arch', help="Missing SCRAM arch")
parser.add_argument('dir', nargs='?', default=os.getcwd())
args = parser.parse_args()

rel = args.dir
while not path.isdir(path.join(rel, ".SCRAM")) and re.search(r'^[\.\/]$', rel):
    rel = path.dirname(rel)
if not path.isdir(path.join(rel, ".SCRAM")):
    raise Exception("%s is not a SCRAM-based project area." % args.dir)

if args.olddir != args.newtop:
    qod = re.escape(args.olddir)
    runtime = None
    for d in [path.join(rel, ".SCRAM", args.arch), path.join(rel, "config")]:
        for f in check_output("find %s -type f" % d, shell=True).decode().rstrip().splitlines():
            if f.endswith(".pyc"): continue
            ftime = 0
            gzip = 0
            if re.search('.db.gz$', f):
                gzip = 1
                s = stat(f)
                ftime = s[9]
                call("gunzip -S.gz %s" % f, shell=True)
                f = re.sub(r'.gz$', r'', f)
            has_olddir = 0
            try:
                with open("%s.rename" % f, "w") as oref:
                    try:
                        with open(f) as iref:
                            for l in iref.readlines():
                                if re.search(qod,l):
                                    has_olddir += 1
                                    l = re.sub(qod, args.newtop, l)
                                oref.write(l)
                    except Exception as e:
                        print(e, "Error: Unable to read file %s" % f)
            except Exception as e:
                print(e, "Error: Unable to create file %s.rename" % f)
            if has_olddir==0:
                unlink("%s.rename" % f)
            else:
                if ftime == 0:
                    s = stat(f)
                    ftime = s[9]
                shutil.move("%s.rename" % f, f)
            if gzip:
                call("gzip -S.gz %s" % f, shell=True)
                f = "%s.gz" % f
            if ftime > 0:
                if (not runtime) and f.endswith("RuntimeCache.json"):
                    runtime = f
                utime(f,(ftime, ftime))

    if runtime:
        utime(runtime, None)
