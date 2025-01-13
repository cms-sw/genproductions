#!/usr/bin/env python3

import sys, re, os, json, gzip
from argparse import ArgumentParser
sys.path.append(os.environ["SCRAM_TOOL_HOME"])
from SCRAM.Core.Utils import readProducts
from SCRAM.Configuration.ConfigArea import ConfigArea

parser = ArgumentParser()
parser.add_argument('-rel')
parser.add_argument('-arch')
parser.add_argument('-scramroot')
args = parser.parse_args()

rel = args.rel

global name
uses = {}
usedby = {}
prod2src = []
directory = rel + "/tmp"


def doexec():
  global uses
  getnext = 0
  depname = ""
  reCC = re.compile(r'(\.o|\.cc)\s*:$')
  re1 = re.compile(r'^[^:]+ :\s*$')
  re2 = re.compile(r'\s*\\$')
  with open(name, 'r', encoding="utf-8") as file:
    for l in file:
      l = l.rstrip('\n')
      if re1.search(l): break
      l = re2.sub(r'', l)
      sp1 = l.split()
      if len(sp1) == 0: continue
      if len(sp1[0]) < 4: continue
      sp2 = sp1[0].split('/')
      tsp1 = ""
      foundsrc = 0
      for t in sp2:
        if foundsrc == 1: tsp1 += "%s/" % t
        if t == "src": foundsrc = 1
      tsp1 = tsp1[:-1]
      if tsp1 == "": continue
      if getnext == 1:
        depname = tsp1
        getnext = 0
      else:
        if sp1[0][:4] == "tmp/":
          if reCC.search(sp1[0]):
            getnext = 1
        else:
          if sp1[0][:3] == "src":
            if depname not in uses:
              uses[depname] = "%s " % tsp1
            else:
              uses[depname] += "%s " % tsp1
            if tsp1 not in usedby:
              usedby[tsp1] = "%s " % depname
            else: 
              usedby[tsp1] += "%s " % depname


def write2File(path, data, type_= None, prod2src=None):
  with open(path, 'w') as file:
    if type_:
      if type_ not in data: data[type_] = {}
      for x in sorted(data[type_]):
        file.write("%s %s\n" % (x, " ".join(sorted(data[type_][x]))))
    elif prod2src:
      for dep in data:
        file.write(str(dep))
    else:
      for key, value in sorted(data.items()):
        file.write("%s %s\n" % (key, value))


def createCache(match, cache, file):
  for x in import2CMSSWDir(match.group(1), cache):
    if "usedby" not in cache: cache["usedby"] = {}
    if "uses" not in cache: cache["uses"] = {}
    if x not in cache["usedby"]: cache["usedby"][x] = {}
    if file not in cache["uses"]: cache["uses"][file] = {}
    if file not in cache["usedby"][x]: cache["usedby"][x][file] = {}
    if x not in cache["uses"][file]: cache["uses"][file][x] = {}
    cache["usedby"][x][file] = 1
    cache["uses"][file][x] = 1


def pythonDeps(rel):
  cache = {}
  for root, dirs, files in os.walk("%s/src/" % rel):
    for filename in files:
      fpath = os.path.join(root, filename)
      if re.search(r'.py$', fpath):
        file = fpath
        file = re.sub(r"^%s\/+src\/+" % rel, r'', file)
        if not re.search(r'\/python\/', fpath):
          continue
        with open(fpath, 'r', encoding="utf-8") as f:
          for line in f.readlines():
            if 'import ' in line:
              line = line.rstrip('\n')
              if re.search(r'^\s*#', line):
                continue
              match_from_import = re.search(r'^\s*from\s+([^\s]+)\s+import\s+', line)
              match_import = re.search(r'^\s*import\s+([^\s]+)\s*', line)
              if match_from_import:
                createCache(match_from_import, cache, file)
              elif match_import:
                createCache(match_import, cache, file)
  for type_ in ("uses","usedby"):
    write2File("%s/etc/dependencies/py%s.out" % (rel, type_), cache, type_)

def import2CMSSWDir(str, cache):
  pyfiles = []
  if "pymodule" not in cache: cache["pymodule"] = {}
  if "noncmsmodule" not in cache: cache["noncmsmodule"] = {}
  for s in str.split(","):
    s = re.sub(r'\.', r'/', s)
    if s in cache["pymodule"]:
      pyfiles.append(cache["pymodule"][s])
    elif s not in cache["noncmsmodule"]:
      if os.path.exists("%s/python/%s.py" % (rel, s)):
        match = re.search(r'^([^\/]+\/+[^\/]+)\/+(.+)$', s)
        if match:
          cache["pymodule"][s] = "%s/python/%s.py" % (match.group(1), match.group(2))
          pyfiles.append("%s/python/%s.py" % (match.group(1), match.group(2)))
      else: cache["noncmsmodule"][s] = 1
  return pyfiles


def updateBFDeps(dir, pcache, cache):
  bf = cache["dirs"][dir]
  if bf in cache["uses"]: return
  cache["uses"][bf] = {}
  deps = {}
  if "USE" in pcache[dir]:
    for use in pcache[dir]["USE"]:
      deps[use] = 1
  if "BUILDPRODUCTS" in pcache[dir]:
    for type in pcache[dir]["BUILDPRODUCTS"]:
      for prod in pcache[dir]["BUILDPRODUCTS"][type]:
        if "USE" not in pcache[dir]["BUILDPRODUCTS"][type][prod]:
          continue
        for use in pcache[dir]["BUILDPRODUCTS"][type][prod]["USE"]:
          deps[use] = 1
  for pack in deps:
    if pack in cache["dirs"]:
      updateBFDeps(pack, pcache, cache)
      xbf = cache["dirs"][pack]
      cache["uses"][bf][xbf] = 1
      if xbf not in cache["usedby"]:
          cache["usedby"][xbf] = {}
      cache["usedby"][xbf][bf] = 1
      for xdep in cache["uses"][xbf]:
        cache["uses"][bf][xdep] = 1
        cache["usedby"][xdep][bf] = 1
  return


def buildFileDeps(rel):
  area = ConfigArea()
  area.location(rel)
  pcache = readProducts(area)
  cache = {"dirs": {}, "uses": {}, "usedby": {}}
  for dir in sorted(pcache.keys()):
    bf = os.path.join(dir, "BuildFile.xml")
    if dir not in cache["dirs"]: cache["dirs"][dir] = {}
    cache["dirs"][dir] = bf
  for dir in cache["dirs"]:
    updateBFDeps(dir, pcache, cache)
  del cache["dirs"]
  for type_ in ("uses", "usedby"):
    write2File("%s/etc/dependencies/bf%s.out" % (rel, type_), cache, type_)


for root, dirs, files in os.walk(directory):
  for filename in files:
    name = os.path.join(root, filename)
    if filename[-4:] == ".dep":
        doexec()
    elif filename == "scram-prod2src.txt":
      import fileinput
      for line in fileinput.input(name):
        prod2src.append(line)


write2File(rel + "/etc/dependencies/uses.out", uses)
write2File(rel + "/etc/dependencies/usedby.out", usedby)
write2File(rel + "/etc/dependencies/prod2src.out", prod2src, prod2src=True)
pythonDeps(rel)
buildFileDeps(rel)
