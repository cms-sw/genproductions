#!/usr/bin/env python3
import sys
import re
from os import environ, makedirs, rename
sys.path.append(environ["SCRAM_TOOL_HOME"])

from os.path import exists, join
from SCRAM.BuildSystem.ToolManager import ToolManager
from SCRAM.Configuration.ConfigArea import ConfigArea
import SCRAM

def isSymlinkSkipped(tools, tool):
    tc = tools[tool]
    if ("FLAGS" in tc) and ("SKIP_TOOL_SYMLINKS" in tc["FLAGS"]):
        return True
    if ("FLAGS" in tools["self"]) and ("SKIP_TOOLS_SYMLINK" in tools["self"]["FLAGS"]):
        return tool in tools["self"]["FLAGS"]["SKIP_TOOLS_SYMLINK"]
    return False

def getScramProjectOrder(tool_cache, tool, cache=None):
    if cache == None:
        cache = {}
    if tool in cache:
        return cache[tool]
    bvar = "%s_BASE" % tool.upper()
    order = 1
    if bvar in tool_cache:
        tcfile = join(tool_cache[bvar], ".SCRAM", arch, "tools")
        if not exists (tcfile):
            SCRAM.die("No such directory: %s" % tcfile)
        area = ConfigArea()
        area.location(tool_cache[bvar])
        tm = ToolManager(area)
        ptools = tm.loadtools()
        for t in ptools:
            if "SCRAM_PROJECT" in ptools[t]:
                o = getScramProjectOrder (ptools[t], t, cache)
                if o>=order:
                    order = o + 1
    cache[tool] = order
    return order

def mkprocessfile (infile, outfile, data):
    tool = data["tool"]
    base = data["base"]
    proj_name = data["project"]
    basevar = "%s_BASE" % tool.upper()
    if tool == "self":
        basevar = "RELEASETOP"
    elif tool == proj_name:
        basevar = "%s_FULL_RELEASE" % basevar
    iref = open(infile)
    oref = open(outfile, "w")
    line = ""
    scount = data["skipcount"]
    ecount = data["editcount"]
    bigprod = False
    plugin = False
    reBigProd = re.compile("^ALL_BIGPRODS\s*\+=\s*([^\s]+)\s*$")
    reReg1 = re.compile("^\s*[^\s]+_(LOC_|relbigobj\+=.*)")
    for line in iref.readlines():
        line = line.strip("\n")
        if reBigProd.match(line):
            bigprod = True
        elif line == "PLUGINS:=yes":
            plugin = True
            continue
        elif line.strip() == "endif":
            bigprod = False
            plugin = False
        if bigprod:
            oref.write(line + "\n")
            continue
        if plugin:
            line = line.replace("_forbigobj+=", "_relbigobj+=")
            if reReg1.match(line):
                oref.write(line + "\n")
            elif line.startswith("ALL_PRODS "):
                oref.write(line.replace("ALL_PRODS ", "ALL_EXTERNAL_PLUGIN_PRODS ") + "\n")
            continue
        skip = False
        for reSkip in data["skiplines"]:
            if reSkip.match(line):
                skip = True
                break
        if skip:
            continue
        for edit in data["editlines"]:
            reg = edit["reg"]
            m = reg.match(line)
            if m:
                v = edit["value"]
                exec (v)
                line = locals()['eline']
                if "cont" not in edit:
                    break
        oref.write(line+"\n")
    oref.close()
    iref.close()

arch = sys.argv[1]
localtop = environ["LOCALTOP"]
reltop = environ["RELEASETOP"] if "RELEASETOP" in environ else ""
proj_name = environ["SCRAM_PROJECTNAME"].lower()
tools= {}
for tool in sys.argv[2:]:
   tools[tool] = 1

locarea = ConfigArea()
locarea.location(localtop)
toolmgr = ToolManager(locarea)
all_tools = toolmgr.loadtools()
if not tools:
    for tool in all_tools:
        tools[tool] = 1

toolvar = ["INCLUDE","LIB"]

skline=0
mkprocess = {"skiplines": [], "editlines": []}
mkprocess["skiplines"].append(re.compile("^.+_(files|XDEPS|SKIP_FILES|libcheck|parent)\s+[:+]=.*"))
mkprocess["skiplines"].append(re.compile("^.+_LOC_((?!(USE|FLAGS)).+)\s+[:+]=.*"))
mkprocess["skiplines"].append(re.compile("^.+_EX_((?!LIB).+)\s+[:+]=.*"))
mkprocess["skiplines"].append(re.compile("^(ALL_COMMONRULES|NON_XML_BUILDFILE)\s+\+=.*"))
mkprocess["skiplines"].append(re.compile("^.+_PACKAGE\s+:=\s+self\/.*"))
mkprocess["skiplines"].append(re.compile("^.+\$\(call\s+.*"))

mkprocess["editlines"].append({"reg": re.compile("^(.+)_LOC_USE\s*:=\s*(.+)$")})
mkprocess["editlines"][-1]["cont"] = 1
mkprocess["editlines"][-1]["value"] = 'eline="%s_EX_USE := $(foreach d, %s,$(if $($(d)_EX_FLAGS_NO_RECURSIVE_EXPORT),,$d))" % (m.group(1), m.group(2))';

mkprocess["editlines"].append({"reg": re.compile("^\s*ALL_PRODS(\s+\+=.+)$")})
mkprocess["editlines"][-1]["value"] = 'eline="ALL_EXTERNAL_PRODS%s" % m.group(1)'

mkprocess["editlines"].append({"reg": re.compile("^(.+)\s+self(\s*.*)$")})
mkprocess["editlines"][-1]["value"] = 'eline="%s %s%s" % (m.group(1), tool, m.group(2))'

mkprocess["editlines"].append({"reg": re.compile("^(.+)\s+self\/(.+)$")})
mkprocess["editlines"][-1]["value"] = 'eline="%s %s/%s" % (m.group(1), tool, m.group(2))'

mkprocess["editlines"].append({"reg": re.compile("^(.+_BuildFile\s+:=\s+)(.+\/cache\/bf\/([^\s]+))\s*$")})
mkprocess["editlines"][-1]["value"] = 'eline="%s$(%s)/.SCRAM/$(SCRAM_ARCH)/MakeData/DirCache.mk" % (m.group(1), basevar)'

mkprocess["editlines"].append({"reg": re.compile("^(.+)_forbigobj\s*\+=(.+)$")})
mkprocess["editlines"][-1]["value"] = 'eline="%s_relbigobj+=%s" % (m.group(1), m.group(2))'

mkprocess["skipcount"] = len(mkprocess["skiplines"])
mkprocess["editcount"] = len(mkprocess["editlines"])
mkprocess["project"] = proj_name

tooldir = ".SCRAM/%s/MakeData/Tools" % arch
stooldir = join(tooldir, "SCRAMBased")

if not exists(stooldir):
    SCRAM.run_command("mkdir -p {0}; touch {0}/order".format(stooldir))

for tool in tools:
    if not tool in all_tools:
        SCRAM.run_command("rm -rf {0}/{2}.mk {1}/{2}".format(tooldir, stooldir, tool))
        continue
    tc = all_tools[tool]
    sproj = "SCRAM_PROJECT" in tc
    tvars = toolvar[:]
    if tool == "self":
        tvars.append("LIBDIR")
    elif tool == proj_name:
        if not reltop:
            tvars.append("LIBDIR")
        else:
            tvars[0] = "LIBDIR"
    elif isSymlinkSkipped(all_tools, tool):
        tvars.append("LIBDIR")
    libtypes = ""
    if "LIBTYPES" in tc:
        libtypes = " ".join(tc["LIBTYPES"])
        tvars.extend(tc["LIBTYPES"])
    fh =open("%s/%s.mk" % (tooldir, tool), "w")
    fh.write("ALL_TOOLS      += %s\n" % tool)
    if libtypes:
        fh.write("ALL_LIB_TYPES += %s\n" % libtypes)
    if sproj:
        fh.write("ALL_SCRAM_PROJECTS += %s\n" % tool)
    for var in tvars:
        if var in tc:
            val = " ".join(tc[var])
            if val:
                fh.write("%s_EX_%s := %s\n" % (tool, var, val))
                if (tool == "self") and (var == "LIBDIR") and proj_name in tools:
                    fh.write("self_EX_%s += \$(%s_EX_%s)\n" % (var, proj_name, var))
    if "USE" in tc:
        x = " ".join([t.lower() for t in tc["USE"]])
        if x:
            fh.write("%s_EX_USE := %s\n" % (tool, x))
    if "FLAGS" in tc:
        for k in sorted(tc["FLAGS"].keys()):
            pre = " "
            if k == "CPPDEFINES":
                pre = " -D"
            x = pre.join(tc["FLAGS"][k])
            if (k == "PCMS") and (x == "1"):
                x = "$(%s_EX_LIB)" % tool
            if x:
                if k == "OVERRIDABLE_FLAGS":
                    fh.write("TOOLS_%s  +=%s\n" % (k, x))
                else:
                    fh.write("%s_EX_FLAGS_%s  :=%s%s\n" % (tool, k, pre, x))
    if sproj:
        sproj = (100000-(2000*getScramProjectOrder(tc,tool)))
    if tool == "self":
        fh.write("self_ORDER := 20000\n")
        if proj_name in all_tools:
            fh.write("IS_PATCH:=yes\n")
        else:
            fh.write("IS_PATCH:=\n")
    elif sproj:
       fh.write("%s_ORDER := %s\n" % (tool, sproj))
    fh.write("\n")
    fh.close()

    if sproj or (tool == "self" and reltop):
        base = ""
        if tool =="self":
            base = reltop
            sproj = 20000
        else:
            base = tc["%s_BASE" % tool.upper().replace("-","_")]
        SCRAM.run_command("grep -v '.*:{0}\$' {1}/order > {1}/order.new; "
                          "mv {1}/order.new {1}/order".format(tool, stooldir))
        if base and exists(base):
            infile = join(base, ".SCRAM", arch, "MakeData", "DirCache.mk")
            outfile = join(stooldir, tool+".mk")
            if exists(infile):
                mkprocess["base"] = base
                mkprocess["tool"] = tool
                mkprocessfile(infile, outfile+".tmp", mkprocess)
                rename(outfile+".tmp", outfile)
                SCRAM.run_command("echo %s:%s >> %s/order" % (sproj, tool, stooldir))

ref = open("%s/all.mk" % stooldir, "w")
with open("%s/order" % stooldir) as ref1:
    for line in [l.strip("\n").split(":",1)[-1] for l in ref1.readlines() if ":" in l]:
        ref.write("include %s/%s.mk\n" % (stooldir, line))
ref.close()

if reltop and "self" in tools:
    bfdir = join(reltop, ".SCRAM", arch, "BuildFiles", environ["SCRAM_SOURCEDIR"])
    if exists(bfdir):
        SCRAM.run_command("find %s -type f | grep -v '/BuildFile.xml$' | xargs cat | "
                          "sort > %s/self_libs_def.mk" % (bfdir, stooldir))
