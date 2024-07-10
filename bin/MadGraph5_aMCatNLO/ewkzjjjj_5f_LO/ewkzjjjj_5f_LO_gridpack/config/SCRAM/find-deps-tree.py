#!/usr/bin/env python3
import sys
from os import environ
from os.path import join, exists, isdir
from subprocess import getstatusoutput
sys.path.append(environ["SCRAM_TOOL_HOME"])

from SCRAM.Core.Utils import readProducts
from SCRAM.BuildSystem.ToolManager import ToolManager
from SCRAM.Configuration.ConfigArea import ConfigArea

def addDeps(cache, product):
    if "USE" in product:
        for use in product["USE"]:
            cache[use] = 1
    return

def readDeps(release, cache):
    area = ConfigArea()
    area.location(release)
    pcache = readProducts(area)
    for dir in pcache:
        if dir in cache:
            continue
        cache[dir] = {}
        addDeps(cache[dir], pcache[dir])
        if "BUILDPRODUCTS" in pcache[dir]:
            for type in pcache[dir]["BUILDPRODUCTS"]:
                for prod in pcache[dir]["BUILDPRODUCTS"][type]:
                    addDeps(cache[dir], pcache[dir]["BUILDPRODUCTS"][type][prod])
    return

def findDeps(cache, pack, dep, chain):
    chain.append(pack)
    if pack in cache:
        for use in sorted(cache[pack].keys()):
            if use == dep:
                print ("  %s->%s" % ("->".join(chain), use))
            else:
                findDeps(cache, use, dep, chain)
                use = "%s/src" % use
                if use in cache:
                    findDeps(cache, use, dep, chain)
    del chain[-1]
    return

pack=sys.argv[1]
dep=sys.argv[2]
cache = {}

area = ConfigArea()
area.location(sys.argv[3])
toolmgr = ToolManager(area)
tools = toolmgr.loadtools()

for base in sys.argv[3:]:
    if base and isdir(base):
        readDeps(base, cache)

for tool in tools:
    cache[tool] = {}
    if "USE" in tools[tool]:
        for use in tools[tool]["USE"]:
            cache[tool][use] = 1

for dir in list(cache.keys()):
    if dir.endswith("/src"):
        pdir = dir[:-4]
        if pdir not in cache:
            cache[pdir] = {}
        for use in cache[dir]:
            cache[pdir][use] = 1
        del cache[dir]

if pack not in cache:
    print ("Error: No such package/tool: %s" % pack)
if dep not in cache:
    print ("Error: No such package/tool: %s" % dep)
findDeps(cache, pack, dep, [])
