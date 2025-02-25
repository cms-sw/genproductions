#!/usr/bin/env python3
import sys
import re
import json
from os import environ, rename, getcwd, unlink, listdir, readlink, chdir
sys.path.append(environ["SCRAM_TOOL_HOME"])
from os.path import exists, join, islink, isdir, abspath, isfile, dirname
from argparse import ArgumentParser
import SCRAM
from SCRAM.BuildSystem.ToolManager import ToolManager
from SCRAM.Configuration.ConfigArea import ConfigArea

cache = {}

SCRAM_CMD = join(environ["SCRAM_TOOL_HOME"], "bin", "scram")
ocache = {}
linksDB = ""

def scramReleaseTop(dir):
    return checkWhileSubdirFound(dir,".SCRAM")

def checkWhileSubdirFound(dir, subdir):
    while (not exists(join(dir, subdir))) and (dir != "/"):
        dir=dirname(dir)
    if exists(join(dir, subdir)):
        return dir
    return ""

def readProjectData(dir, arch):
    data = {}
    for efile in [ join(dir, ".SCRAM", "Environment"), join(dir, ".SCRAM", arch, "Environment")]:
        if exists(efile):
            with open(efile) as ref:
                for line in ref.readlines():
                    x = line.strip("\n").split("=",1)
                    data[x[0]] = x[1]
    return data

def readLinkDB ():
    if "PREDBLINK" in cache:
        return
    if not exists(linksDB):
        cache["PREDBLINK"] = {}
        cache["PREDBLINKR"] = {}
        return
    with open(linksDB) as ref:
        for line in ref.readlines():
            line = line .strip()
            if line[0:1] not in ["L:", "B:"]:
                continue
            x = line.split(":",2)
            if x[0] == "L":
                if x[1] not in cache["PREDBLINK"]:
                    cache["PREDBLINK"][x[1]] = {}
                lfile = join(cache["externals"],x[2])
                cache["PREDBLINK"][x[1]][lfile] = 1
                if lfile not in cache["PREDBLINKR"]:
                    cache["PREDBLINKR"][lfile] = {}
                cache["PREDBLINKR"][lfile][x[1]] = 1

def removeLinks (tool, arch, tools):
    if tool not in cache["PREDBLINK"]:
        return
    for file in cache["PREDBLINK"][tool]:
        if islink(file):
            del cache["PREDBLINKR"][file][tool]
            if not cache["PREDBLINKR"][file]:
                unlink(file)
    if tool in tools and "LIB" in tools[tool]:
        for l in tools[tool]["LIB"]:
            lf = join (localtop, "tmp", arch, "cache", "prod", "lib%s" % l)
            if exists(lf):
                with open(lf, "w"):
                    pass
    del cache["PREDBLINK"][tool]

def updateLinks (tool):
    for type in cache["defaultlinks"]:
        dep = 1
        if type in cache["sym_depth"]:
            dep = cache["sym_depth"][type]
        tb = "%s_BASES" % type
        if tool in ocache[tb]:
            for dir in ocache[tb][tool]:
                processBase(tool, dir, type, dep, "")

def isIgnoreLink(type, srcfile, file):
    if type not in cache["ignorefiles"]:
        return False
    for reg in cache["ignorefiles"][type]:
        ftype = cache["ignorefiles"][type][reg]
        if reg.match(file):
            if ftype == "a":
                return True
            if isdir(srcfile):
                return ftype == "d"
            return ftype == "f"
    return False

def createLink (tool, srcfile, type, file):
    if tool not in cache["DBLINK"]:
        cache["DBLINK"][tool] = {}
    lfile = ""
    if (type in cache["links"]) and (srcfile in cache["links"][type]):
        lfile = cache["links"][type][srcfile]
        cache["DBLINK"][tool][lfile] = 1
        cache["DBLINKR"][lfile] = 1
        return
    type = cache["validlinks"][type]
    ldir = ""
    for s in cache["extradir"]:
        ldir = join(cache["externals"], "%s%s" % (type, s))
        lfile = join(ldir, file)
        xdir = dirname(lfile)
        if not isdir(xdir):
            SCRAM.run_command("mkdir -p %s" % xdir)
        if not islink(lfile):
            SCRAM.run_command("cd %s;ln -s %s ." % (xdir, srcfile))
            break
        elif (readlink(lfile) == srcfile):
            break
        elif lfile not in cache["DBLINKR"]:
            SCRAM.run_command("rm -f %s; cd %s;ln -s %s ." % (lfile, xdir, srcfile))
            break
    cache["dirused"][ldir] = 1
    if type not in cache["links"]:
        cache["links"][type] = {}
    cache["links"][type][srcfile] = lfile
    if tool not in  cache["DBLINK"]:
        cache["DBLINK"][tool] = {}
    cache["DBLINK"][tool][lfile] = 1
    cache["DBLINKR"][lfile] = 1
    if lfile in cache["PREDBLINKR"]:
        del cache["PREDBLINKR"][lfile]

def getOrderedTools (tm, tools):
    compilers = []
    tmphash = {}
    data = tm.toolsdata()
    data.reverse()
    for tc in data:
        tn = tc['TOOLNAME']
        if (tn not in tools) or (tn in ["self", ""]): continue
        if 'SCRAM_COMPILER' in tc and tc['SCRAM_COMPILER']:
            compilers.append(tn)
            continue
        if tn not in tmphash:
            tmphash[tn] = 1
            cache["alltools"].append(tn)
    for tn in compilers:
        if tn not in tmphash:
            tmphash[tn] = 1
            cache["alltools"].append(tn)

def processBase(tool, dir, type, dep, rpath):
    if not isdir(dir): return
    dir = abspath(dir)
    for f in listdir(dir):
        ff = join(dir, f)
        if isIgnoreLink(type,ff,f):
            continue
        if (dep > 1) and isdir(ff):
            processBase(tool, ff, type, dep-1, "%s%s/" % (rpath, f))
        else:
            createLink(tool, ff, type, join(rpath, f))

def main():
    parser = ArgumentParser()
    parser.add_argument('--update', default=[], action='append', help="Updates tools")
    parser.add_argument('--pre',    default=[], action='append', help="pre tools")
    parser.add_argument('--post',   default=[], action='append', help="post tools")
    parser.add_argument('--arch',   default=environ["SCRAM_ARCH"], help="SCRAM arch")
    parser.add_argument('--all',    default=False, action='store_true', help="Process all tools")
    args = parser.parse_args()

    for x in ["validlinks", "defaultlinks", "ignorefiles", "runtime_map",
              "sym_depth", "skipTools", "donetools", "links", "dirused",
              "BASES", "DBLINK", "DBLINKR"]:
        cache[x] = {}
    cache["alltools"] = []
    cache["externals"] = ""

    cache["validlinks"]["LIBDIR"]="lib";
    cache["validlinks"]["PATH"]="bin";
    cache["validlinks"]["PYTHONPATH"]="python";
    cache["validlinks"]["CMSSW_SEARCH_PATH"]="data";
    cache["validlinks"]["SHAREDIR"]="share";
    cache["validlinks"]["ROOT_INCLUDE_PATH"]="rootinclude";

    cache["defaultlinks"]["LIBDIR"]=1;

    for x in ["LIBDIR", "PYTHONPATH", "CMSSW_SEARCH_PATH", "ROOT_INCLUDE_PATH"]:
        cache["ignorefiles"][x] = {}

    cache["ignorefiles"]["LIBDIR"][re.compile("^python.+$")]="d";
    cache["ignorefiles"]["LIBDIR"][re.compile("^modules$")]="d";
    cache["ignorefiles"]["LIBDIR"][re.compile("^pkgconfig$")]="d";
    cache["ignorefiles"]["LIBDIR"][re.compile("^archive$")]="d";
    cache["ignorefiles"]["LIBDIR"][re.compile("^cmake$")]="d";
    cache["ignorefiles"]["LIBDIR"][re.compile('^__.SYMDEF\s+SORTED$')]="f";
    cache["ignorefiles"]["PYTHONPATH"][re.compile("^CVS$")]="d";
    cache["ignorefiles"]["CMSSW_SEARCH_PATH"][re.compile("^etc$")]="d";
    cache["ignorefiles"]["CMSSW_SEARCH_PATH"][re.compile("^.package-checksum$")]="f";
    cache["ignorefiles"]["ROOT_INCLUDE_PATH"][re.compile("^CVS$")]="d";

    cache["runtime_map"]["LIBDIR"]=["LD_LIBRARY_PATH", "DYLD_FALLBACK_LIBRARY_PATH"];
    environ["SCRAM_ARCH"] = args.arch

    localtop = scramReleaseTop(getcwd())
    chdir(localtop)
    projdata = readProjectData(localtop, args.arch)
    all = args.all
    if (not all) and ("RELEASETOP" not in projdata):
        all = True

    area = ConfigArea()
    area.location(localtop)
    tm = ToolManager(area)
    tools = tm.loadtools()

    cache["skipTools"]["self"] = 1
    cache["skipTools"][projdata["SCRAM_PROJECTNAME"].lower()] = 1
    if "FLAGS" in tools["self"]:
        flags = tools["self"]["FLAGS"]
        if "EXTERNAL_SYMLINK" in flags:
            cache["defaultlinks"]={}
            for x in flags["EXTERNAL_SYMLINK"]:
                if x.upper() in cache["validlinks"]:
                    cache["defaultlinks"][x.upper()] = 1
        if "SCRAM_TARGETS" in flags:
            for x in flags["SCRAM_TARGETS"]:
                cf = "LIBDIR"
                if cf in cache["defaultlinks"]:
                    ux = "%s_%s" %  (x.upper().replace("-","_"), cf)
                    cache["validlinks"][ux] = join(cache["validlinks"][cf], x)
                    cache["defaultlinks"][ux] = 1
                    if cf in cache["ignorefiles"]:
                        cache["ignorefiles"][ux] = cache["ignorefiles"][cf]
        if "SKIP_TOOLS_SYMLINK" in flags:
            for t in flags["SKIP_TOOLS_SYMLINK"]:
                cache["skipTools"][t.lower()] = 1
        for f in flags:
            if f.startswith("SYMLINK_DEPTH_"):
                fx = f.replace("SYMLINK_DEPTH_", "")
                if fx in cache["validlinks"]:
                    dep = int(flags[f][0])
                    if dep<1:
                        dep = 1
                    cache["sym_depth"][fx] = dep

    if not cache["defaultlinks"]:
        sys.exit(0)

    #### Ordered list of removed tools
    tmphash = {}
    cache["updatetools"]=[]
    for t in args.update:
        t = t.lower()
        if t not in tmphash:
            cache["updatetools"].append()

    #### Ordered list of tools to be set first
    tmphash=();
    cache["pretools"]=[];
    for t in args.pre:
        t = t.lower()
        if t not in tmphash:
            cache["pretools"].append()

    #### Ordered list of tools to be set last
    cache["posttools"]=[];
    cache["posttools_uniq"]={};
    for t in args.post:
        t = t.lower()
        if t not in cache["posttools_uniq"]:
            cache["posttools_uniq"][t] = 1
            cache["posttools"].append()

    cache["extradir"] = [""]
    for i in range(20):
        if i<10:
            cache["extradir"].append("0%s" % i)
        else:
            cache["extradir"].append(i)

    #### Read previous link info
    cache["externals"] = "external/%s" % args.arch
    externals = cache["externals"]
    linksDB = join (externals, "links.DB")
    readLinkDB ()
 
    tmphash = {}
    for tool in tools:
        tc = tools[tool]
        if (tool in cache["skipTools"]) or (("FLAGS" in tc) and ("SKIP_TOOL_SYMLINKS" in tc["FLAGS"])):
            removeLinks(tool, args.arch, tools)
            if tool in cache["BASES"]:
                del cache["BASES"][tool]
            continue
        nbases = {}
        for x in cache["defaultlinks"]:
            xb = "%s_BASES" % x
            if xb not in ocache:
                ocache[xb] = {}
            if tool not in ocache[xb]:
                ocache[xb][tool] = []
            if x in tc:
                dirs = []
                if isinstance(tc[x], list):
                    dirs = tc[x]
                elif isinstance(tc[x], str):
                    dirs.append(tc[x])
                for dir in dirs:
                    nbases[dir] = 1
                    if xb not in cache:
                        cache[xb] = {}
                    if tool not in cache[xb]:
                        cache[xb][tool] = {}
                    if dir not in cache[xb][tool]:
                        cache[xb][tool][dir] = 1
                        ocache[xb][tool].append(dir)
            if "RUNTIME" in tc:
                xp = [x]
                if x in cache["runtime_map"]:
                    xp = cache["runtime_map"][x]
                for y in xp:
                    yp = "PATH:%s" % y
                    if yp in tc["RUNTIME"]:
                        for dir in tc["RUNTIME"][yp]:
                            nbases[dir] = 1
                            if xb not in cache:
                                cache[xb] = {}
                            if tool not in cache[xb]:
                                cache[xb][tool] = {}
                            if dir not in cache[xb][tool]:
                                cache[xb][tool][dir] = 1
                                ocache[xb][tool].append(dir)
        for dir in nbases:
            if tool not in cache["BASES"]:
                tmphash[tool] = 1
                cache["BASES"][tool] = {}
            cache["BASES"][tool][dir] = 2
        if tool not in cache["BASES"]:
            cache["BASES"][tool] = {}
        for dir in cache["BASES"][tool]:
            if cache["BASES"][tool][dir] == 2:
                cache["BASES"][tool][dir] = 1
            else:
                tmphash[tool] = 1
                del cache["BASES"][tool][dir]
    if tmphash:
        for tool in cache["updatetools"]:
            tmphash[tool]=1
        cache["updatetools"] = list(tmphash.keys())

    #### Remove all the links for tools passed via command-line arguments
    for tool in cache["updatetools"]:
        removeLinks(tool, args.arch, tools)

    ##### Ordered list of all tools
    getOrderedTools (tm, tools)

    for tooltype in ["pretools", "alltools" , "posttools"]:
        if tooltype in cache:
            for tool in cache[tooltype]:
                if tool in cache["skipTools"]:
                    continue
                if (tooltype == "alltools") and (tool in cache["posttools_uniq"]):
                    continue
                if all or exists(join(localtop,".SCRAM", args.arch, "InstalledTools", tool)):
                    if tool not in cache["donetools"]:
                        cache["donetools"][tool] = 1
                        updateLinks(tool)
    if isdir(externals):
        with open(linksDB, "w") as ref:
            if "DBLINK" in cache:
                for x1 in cache["DBLINK"]:
                    for x2 in cache["DBLINK"][x1]:
                        ref.write("L:%s:%s\n" % (x1, x2.split(externals,1)[-1].strip("/")))
            if "BASES" in cache:
                for x1 in cache["BASES"]:
                    for x2 in cache["BASES"][x1]:
                        ref.write("B:%s:%s\n" % (x1, x2))
        for type in sorted(cache["validlinks"].keys()):
            type = cache["validlinks"][type]
            for s in cache["extradir"]:
                ldir = join(externals, "%s%s" % (type, s))
                if isdir(ldir):
                    if ldir not in cache["dirused"]:
                        SCRAM.run_command("rm -rf %s" % ldir)
                else:
                    break
        if "PREDBLINKR" in cache:
            for lfile in cache["PREDBLINKR"]:
                if islink(lfile):
                    SCRAM.run_command("rm -f %s" % lfile)
    return

main()
