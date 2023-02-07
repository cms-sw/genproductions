#!/usr/bin/env python3
import sys
from subprocess import getstatusoutput
from os.path import exists, normpath

allowed_usage = {
  "test": ["test"]
}

err=0
with open(sys.argv[1]) as ref:
  data = ""
  for line in ref.readlines():
    line = line.strip()
    if line.endswith('\\'):
      data += line.strip()[:-1]
      continue
    data += line.strip()
    if (not data) or (":" not in data): continue
    items = data.split(":",1)
    data = ""
    if " " in items[0]: items[0] = [x for x in items[0].split(" ") if x.startswith("tmp/")][-1]
    if not items[0].startswith("tmp/"): continue
    items = [x for x in items[1].split(" ") if x]
    if not items[0].startswith("src/"): continue
    src_parts = items[0].split("/")
    src_pkg = "/".join(src_parts[:3])+"/"
    for inc in items[1:]:
      if not inc.startswith("src/"): continue
      inc = normpath(inc)
      inc_parts = inc.split("/")
      if (len(inc_parts)<4) or (inc_parts[3] == "interface"): continue
      inc_file = "/%s/%s" % (inc_parts[-2], inc_parts[-1])
      if inc.startswith(src_pkg):
        interface_dir = src_pkg+"interface"
        if exists(interface_dir) and int(getstatusoutput("grep -IR '%s\"' %s | wc -l" % (inc_file, interface_dir))[-1])>0:
          print("****ERROR:Private Header: File %s via it's public headers in %s includes %s" % (items[0],interface_dir,inc))
          err=1
        continue
      if (src_parts[3] in allowed_usage) and (inc_parts[3] in allowed_usage[src_parts[3]]):
        continue
      e, o = getstatusoutput("grep -IR '%s\"' %s | wc -l" % (inc_file, src_pkg))
      if int(o)>0:
        print ("****ERROR:Private Header: File %s directly includes %s" % (items[0],inc))
        err=1
sys.exit(err)
