#!/usr/bin/python
'''
 - get some indications from Phantom authors on the parameters for the gridpacks generation
 - transform the script into a daemon
 - upload it to genproduction and find a way to test it
    - how do I get the list of architectures and most importantly the configurations?
      slc6_amd64_gcc493 CMSSW_7_6_6_patch1
      slc6_amd64_gcc630 CMSSW_9_3_0_pre4
      slc5_amd64_gcc462 CMSSW_5_3_17
      slc5_amd64_gcc472 CMSSW_6_2_0_SLHC15
      slc6_amd64_gcc530 CMSSW_9_2_10
      slc5_amd64_gcc434 CMSSW_4_2_10_patch2
      slc6_amd64_gcc491 CMSSW_7_5_8_patch7
      slc6_amd64_gcc481 CMSSW_7_1_29
      slc6_amd64_gcc472 CMSSW_5_3_36
 FIXME to let condor work, at least the following:
 - decide when to finish the jobs, see whether the control for lsf is enough

 dettagli da josh su come funzionano le chiamate degli script
 so there are two places

 FIXME update this page: https://twiki.cern.ch/twiki/bin/view/CMSPublic/SWGuideSubgroupMC#1_1_Using_wmLHE_campaigns
 one for the traditional wmLHE workflow

 and one for the new susy parameter scan workflow

 for the wmLHE workflow: https://github.com/cms-sw/cmssw/blob/master/GeneratorInterface/LHEInterface/plugins/ExternalLHEProducer.cc#L253

 https://github.com/cms-sw/cmssw/blob/master/GeneratorInterface/LHEInterface/plugins/ExternalLHEProducer.cc#L366

 for the newer susy workflow which is only used for parameter scans so far:
 https://github.com/cms-sw/cmssw/blob/master/GeneratorInterface/Core/interface/GeneratorFilter.h#L247

 https://github.com/cms-sw/cmssw/blob/09c3fce6626f70fd04223e7dacebf0b485f73f54/GeneratorInterface/Core/src/BaseHadronizer.cc#L78
 useful HN reference from Qiang: https://hypernews.cern.ch/HyperNews/CMS/get/generators/3320/1/1.html
 where to put gridpacks: ls  /cvmfs/cms.cern.ch/phys_generator/gridpacks/
 how to have gridpacks end there: https://twiki.cern.ch/twiki/bin/view/CMSPublic/SWGuideSubgroupMC#1_1_Using_wmLHE_campaigns
 for the time being, I use this place: /eos/cms/store/group/phys_generator/cvmfs/gridpacks/slc6_amd64_gcc530/13TeV/phantom
'''

import sys
import os
import commands
from commands import getstatusoutput
import datetime
import argparse
import datetime
import math
import ConfigParser
import re
import time
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import math


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def replaceParameterInFile (inputFile, outputFile, substitute):
    f = open (inputFile)
    lines = f.readlines ()
    f.close ()
    f = open (outputFile, 'w')
    for line in lines:
        if line.startswith ('*') or line.startswith (' ') or len (line) < 3 or line.startswith ('.') :
            f.write (line)
        else:
            words = line.split (' ')
            if words[0] in substitute.keys ():
                f.write (words[0] + ' ' + substitute[words[0]])
            else:
                f.write (line)
    f.close ()


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def execute (command, verbosity = False) :
    if (verbosity == True):
        print '---> running:'
        print command
    retCode = getstatusoutput (command)
    if (verbosity == True):
        for ri in retCode: print ri
    return retCode


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def findLhapdfUsed (makefilename):
    f = open (makefilename, 'r')
    for line in f.readlines ():
        if line.startswith ('#') : continue
        if re.match ('^\s*PDFLIBDIR\s*=', line):
            return line.split()[2]


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def modifySubmitfileIntelCompiler (submitfilename, pdfgridfolder, phantompdflib):
    submitfile = open (submitfilename, 'read')
    lines = submitfile.readlines ()
    submitfile.close ()
    submitfile = open (submitfilename, 'write')
    submitfile.write ('#!/bin/bash\n\n')
    submitfile.write ('source /afs/cern.ch/sw/IntelSoftware/linux/setup.sh intel64\n')
    submitfile.write ('source /afs/cern.ch/sw/IntelSoftware/linux/x86_64/xe2016/compilers_and_libraries/linux/bin/compilervars.csh intel64\n')
    submitfile.write ('export LHAPDF=' + pdfgridfolder + '\n')
    submitfile.write ('export PDFLIBDIR=/afs/cern.ch/work/b/ballest/public/phantom/LHAPDF-6.1.5_work/lib\n')
#    submitfile.write ('export PDFLIBDIR=' + phantompdflib + '\n')
#    submitfile.write ('export PDFLIBDIR=' + pdfgridfolder + '/../../lib\n')
    submitfile.write ('export LD_LIBRARY_PATH=$PDFLIBDIR:$LD_LIBRARY_PATH\n')
    for i in range (1, len (lines)) :
        submitfile.write (lines[i])
    submitfile.close ()


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def modifySubmitfileCMSSWCompiler (submitfilename, pdfgridfolder, phantompdflib, cmssw, shell, scram_arch, cmssw_path = ""):
    submitfile = open (submitfilename, 'read')
    lines = submitfile.readlines ()
    submitfile.close ()
    submitfile = open (submitfilename, 'write')
    submitfile.write ('#!/bin/bash\n\n')
    submitfile.write ('scram -a ' + scram_arch + ' project CMSSW ' + cmssw + '\n')
    submitfile.write ('cd ' + cmssw_path + cmssw + '/src\n')
    submitfile.write ('eval `scram runtime -' + shell + '`\n')
    submitfile.write ('cd -\n')
    for i in range (1, len (lines)) :
        submitfile.write (lines[i])
    submitfile.close ()


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


''' setup the shell script that will be run by the cms production framework
    in the wmLHE production mode
    NOTA BENE the script does not allow to modify the architecture on purpose
              since it keeps the one used for the gridpack calculation
'''
def prepareEventProductionScript (productionfilename, phantom, phantomfolder, cmssw, shell, scram_arch, debugging = 0):
    productionfile = open (productionfilename, 'write')
    if debugging:
        print 'creating ' + productionfilename
    productionfile.write ('#!/bin/bash\n\n')

    productionfile.write ('fail_exit() { echo "$@"; exit 1; }\n')

    productionfile.write ('echo "   ______________________________________     "\n')
    productionfile.write ('echo "         Running Phantom                      "\n')
    productionfile.write ('echo "   ______________________________________     "\n')

    productionfile.write ('nevt=${1}\n')
    productionfile.write ('echo "%MSG-PHANTOM number of events requested = $nevt"\n')
    productionfile.write ('rnum=${2}\n')
    productionfile.write ('echo "%MSG-PHANTOM random seed used for the run = $rnum"\n')
    productionfile.write ('ncpu=1\n')
    productionfile.write ('echo "%MSG-PHANTOM number of cputs for the run = $ncpu"\n')

    productionfile.write ('cmssw_version=' + cmssw + '\n')
    productionfile.write ('scram_arch=' + scram_arch + '\n')

    productionfile.write ('modify_env=false\n')

    productionfile.write ('if [ -n "$4" ]\n')
    productionfile.write ('  then\n')
    productionfile.write ('    modify_env=$4\n')
    productionfile.write ('    if [ "$modify_env" = true ]\n')
    productionfile.write ('      then\n')
    productionfile.write ('      if [ -n "$6" ]\n')
    productionfile.write ('        then\n')
    productionfile.write ('          cmssw_version=${6}\n')
    productionfile.write ('      fi\n')
    productionfile.write ('    fi  \n')
    productionfile.write ('  echo ${1}\n')
    productionfile.write ('  else\n')
    productionfile.write ('  echo "ciccia"  \n')
    productionfile.write ('fi\n')

    productionfile.write ('echo "%MSG-PHANTOM getting environment LHAPDF from CMSSW release = $cmssw_version"\n')
    productionfile.write ('echo "%MSG-PHANTOM running with architecture  = $scram_arch"\n')

    # setup the environment for the running
    productionfile.write ('scram -a ${scram_arch} project CMSSW ${cmssw_version}\n')
    productionfile.write ('cd ${cmssw_version}/src\n')
    productionfile.write ('eval `scram runtime -' + shell + '`\n')
    productionfile.write ('cd -\n')

    # get the phantom release
    productionfile.write ('wget ' + phantom + '\n')
    productionfile.write ('tar xzf ' + phantom.split ('/')[-1] + '\n')

    # set the number of events to be generated
    productionfile.write ('cat r_GEN.in | sed -e s/EVENTSNUM/${nevt}/ > r_tempo.in\n')

    # set the random seed
    productionfile.write ('cat r_tempo.in | sed -e s/RANDOMSEED/${rnum}/ > r.in\n')
    if not debugging:
        productionfile.write ('rm r_tempo.in\n')

    # call the event production
    productionfile.write ('./' + phantomfolder + '/phantom.exe >& log_GEN.txt\n')

    # FIXME check the success of the production    
    productionfile.write ('head -n `grep -n "<init>" phamom.dat | tr ":" " " | awk \'{print $1+1}\'` phamom.dat >> cmsgrid_final.lhe\n')
    productionfile.write ('tail -n 1 result | awk \'{print $4" "$6" -1 -1"}\' >> cmsgrid_final.lhe\n')
    productionfile.write ('tail -n +`grep -n "<init>" phamom.dat | tr ":" " " | awk \'{print $1+3}\'` phamom.dat >> cmsgrid_final.lhe\n')

    productionfile.close ()
    execute ('chmod 755 ' + productionfilename, debugging)



# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def addGridsToRin (filename, grids, debug = False):
    if debug : print 'preparing ' + filename + '\n'
    configfile = open (filename, 'read')
    lines = configfile.readlines ()
    configfile.close ()
    configfile = open (filename, 'write')
    for line in lines:
        if line.startswith ('nfiles'):
            configfile.write (line)
            break
        configfile.write (line)
    for gridfile in grids.split ():
        if debug : print 'adding ' + gridfile + '\n'
        configfile.write (gridfile + '\n')
    configfile.close ()


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def wordInFile (word, filename):
    thefile = open (filename, 'r')
    s = thefile.read ()
    thefile.close ()
    if word in s: return True
    return False


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


''' get the precompiled version code
    according to the input location passed as argument
    return the location of the untarred version
'''
def getPhantom (config, workingfolder, debugging):

    # get the precompiled phantom code, untar it and get the name of the pdf libraries
    # used in the compilation
    phantom = config.get ('general', 'package')
    # Check is the package is on http or in local folder
    if "http" in phantom:
        foundIT = execute ('wget ' + phantom, debugging)
        if not foundIT[0] == 0:
            print 'Phantom version: ' + phantom + ' not found, exiting'
            sys.exit (1)
    else:
        # should the phantom code be a local folder
        if not os.path.isfile (phantom):
            print 'Phantom package ' + phantom + ' not found, exiting'
            sys.exit (1)
        execute ('cp ' + phantom + ' ' + workingfolder, debugging)

    execute ('tar xzf ' + phantom.split ('/')[-1], debugging)
    phantomfolder = phantom.split ('/')[-1]
    dummy = '.tar.gz'
    phantomfolder = phantomfolder[0:-len(dummy)] if phantomfolder.endswith(dummy) else phantomfolder
    dummy = '.tgz'
    phantomfolder = phantomfolder[0:-len(dummy)] if phantomfolder.endswith(dummy) else phantomfolder
    phantompdflib = findLhapdfUsed (workingfolder + '/' + phantomfolder + '/makefile')
    return [phantomfolder, phantompdflib, phantom] #FIXME CHECK


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


''' split a txt file using a list of words as subsequent separators:
    text0 word1  text1 word2 text2 word3 text3
    gets retured as [text0, text1, text2, text3] if the separator list given is
    [word1, word2, word3]
'''
def splitWithList (filename, keywords):
  result = {}
  f = open(filename, 'r')
  filecontent = f.read ()
  foundkeys = []
  start = []
  end = []
  start.append (0)
  for word in keywords:
    position = filecontent.find (word)
    if position != -1 :
        foundkeys.append (word)
        end.append (position)
        start.append (position+len (word))
  end.append (len (filecontent))
  for ind in range (1, len (start)):
    result[foundkeys[ind-1]] = filecontent[start[ind]:end[ind]]
  f.close ()
  return result


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


''' find the last line containing a given string, in the text'''
def findStringInLines (text, keyword):
  result = ''
  for word in text.split ('\n'):
    if keyword in word: result = word
  return result


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


''' extract the integral value, its uncertainty and the chi2 from a line
    with the usual phantom formatting'''
def findIntResults (line):
  line = line.replace ('+/-', ' +/-')
  words = line.split ()
  integral = float (words[4])
  integral_unc = float (words[6])
  chi2 = float (words[9])
  return (integral, integral_unc, chi2)


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


''' read the output file of the grid generation
    and extract the values of the calculations at the various steps
    of each interation. 
    Returns a lsit of tuples.'''
def extractIntegrationResults (filename):

  keywords = ['NORMALIZATION','ALFA(i) DETERMINATION','THERMALIZATION','INTEGRATION'] 
  result = splitWithList (filename, keywords)

  calc_output = []
  # loop over stages of the calculation
  for key,chunk in result.items ():
    blockresult = chunk.split ('iphs_ind=')
    
    # loop over diagrams in a single stage
    for elem in blockresult:
      if len(elem.split ()) == 0 : continue # skip empty sections
      if 'integral' not in elem : continue # skip non informative sections
      diagram = int (elem.split ()[0])
      num_ite = 1    # iteration number
      int_ite = -1.  # integral at a given iteration
      err_ite = -1.  # integral error at a given iteration
      int_tot = -1.  # integral after all iterations
      err_tot = -1.  # integral error after all iterations
      ch2_tot = -1.  # chi2 after all iterations
      for line in elem.replace ('\n all',' all').split ('\n'):
        if ('integral' not in line) or ('iteration' not in line): continue
        num_ite = int (line.replace ('+/-', ' +/-').split ()[2].replace (':',''))
        int_ite = float (line.replace ('+/-', ' +/-').split ()[5])
        err_ite = float (line.replace ('+/-', ' +/-').split ()[7]) 
        int_tot = float (line.replace ('+/-', ' +/-').split ()[12]) 
        err_tot = float (line.replace ('+/-', ' +/-').split ()[14]) 
        ch2_tot = float (line.replace ('+/-', ' +/-').split ()[17]) 
        calc_output.append ((key.ljust (21),diagram,num_ite,int_ite,err_ite,int_tot, err_tot, ch2_tot))

  return calc_output
  

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def getAxesLimits (x, y, xerr, yerr):
    x_step = x[1] - x[0]
    x_min = x[0] - xerr[0] - 0.5 * x_step
    x_max = x[-1] + xerr[-1] + 0.5 * x_step
    y_min = min (y) - 1.2 * yerr[y.index (min (y))]
    y_max = max (y) + 1.2 * yerr[y.index (max (y))]
    return [x_min, x_max, y_min, y_max]


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def mergeAxesLimits (lim1, lim2):
    return [min (lim1[0], lim2[0]), max (lim1[1], lim2[1]), min (lim1[2], lim2[2]), max (lim1[3], lim2[3])]


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


''' checks whether in any step of the grid calculation
    the program outputs NaN results
'''    
def checkForNaN (thisoutput, logfile, process):
    for elem in thisoutput:
        isOK = True
        for ind in range (3,7):
            if math.isnan(elem[ind]):
                isOK = False
                break
        if isOK == False:
            message =  'process ' + process + ' FAILED\nissue in calculating ' + elem[0] + ' for channel ' + str (elem[1]) + ' at the iteration ' + str (elem[2]) + ': the result is ' + elem[ind]  
            print message
            logfile.write (message + '\n')
            return True
    return False


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


'''post-processing of the gridpacks: success tests
'''
def verifyGridpack (processoutputs, workingfolder, logfile):

    # verify that all jobs finished successfully
    finished = True
    logfile.write ('\n-->gridpack calculation summary\n')
    plotsFileName = os.path.dirname(logfile.name) + '/' + os.path.basename(logfile.name).split ('.')[0] + '.pdf'
    plotsFile = PdfPages (plotsFileName)
    summary_output = []

    # loop over processes
    for fil in processoutputs:

        if not wordInFile ('SIGMA', fil):
            print 'the following job had issues:\n  ' + fil
            logfile.write ('the following job had issues: ' + fil)
            finished = False

        thisoutput = extractIntegrationResults (fil)
        thisoutput = sorted (thisoutput, key = lambda x: (x[0], x[1], x[2]))

        process = fil.split ('/')[-2]
        NaNcheck =  checkForNaN (thisoutput, logfile, process)
        
        # print a summary of the grid calculation process

        if NaNcheck == False:
            logfile.write ('\nprocess: ' + process + ' \n')
            logfile.write ( 'step'.ljust (21) + '\tchannel\tit\tint             intE            intTOT          intTOTE         chi2\n')
            print 'process:', process
            for elem in thisoutput:
                writing = elem[0] + '\t' + str (elem[1]) + '\t' + str (int (elem[2]))
                for ind in range (3, len(elem)-1):
                   writing = writing + '\t{0:04.2e}'.format(elem[ind])
                writing = writing + '\t' + str (elem[-1])
                logfile.write (writing + '\n')
    
            # prepare detailed drawings of the grid calculation process
    
            # find the unique occurrences in first and second column
            steps = set ([elem[0] for elem in thisoutput])
            # for each combination of those, prepare a plot of evolution with iteration
            for step in steps:
                if 'ALFA' in step : continue
                if 'NORMALIZATION' in step: continue
                chans = set ([elem[1] for elem in thisoutput if elem[0] == step])
                for chan in chans:
                    x_val = [elem[2] for elem in thisoutput if (elem[0] == step and elem[1] == chan)]
                    x_err = [0] * len (x_val)
                    # total integral
                    y_val_tota = [elem[5] for elem in thisoutput if (elem[0] == step and elem[1] == chan)]
                    y_err_tota = [elem[6] for elem in thisoutput if (elem[0] == step and elem[1] == chan)]
                    axes_limits_tota = getAxesLimits (x=x_val, y=y_val_tota, xerr=x_err, yerr=y_err_tota)
                    fig = plt.figure ()
                    plt.errorbar (x=x_val, y=y_val_tota, xerr=x_err, yerr=y_err_tota, label='total')
                    # partial integral
                    y_val_temp = [elem[3] for elem in thisoutput if (elem[0] == step and elem[1] == chan)]
                    y_err_temp = [elem[4] for elem in thisoutput if (elem[0] == step and elem[1] == chan)]
                    axes_limits_temp = getAxesLimits (x=x_val, y=y_val_temp, xerr=x_err, yerr=y_err_temp)
                    plt.errorbar (x=x_val, y=y_val_temp, xerr=x_err, yerr=y_err_temp, color = 'g', label = 'partial')
                    
                    chi2val = [elem[7] for elem in thisoutput if (elem[0] == step and elem[1] == chan and elem[2] == len (y_val_tota))]
                    summary_output.append ((process, step, chan, y_val_temp[-1], y_err_temp[-1], y_val_tota[-1], y_err_tota[-1], chi2val[0]))
                    
                    # drawing
                    leg = plt.legend (loc='best', fancybox=True)
                    leg.get_frame().set_alpha(0.5)
                    plt.title (process + ' ' + step + ' channel ' + str (chan) + '\n')
#                    plt.ticklabel_format (axis='y', style='sci', scilimits=(-2,2))
                    plt.xlabel('iteration')
                    plt.ylabel('total integral')
                    axes_limits = mergeAxesLimits (axes_limits_temp, axes_limits_tota)
                    plt.xlim ((axes_limits[0], axes_limits[1]))
                    fig.savefig (plotsFile, format = 'pdf')
        else:
            finished = False
            
    # end loop over processes

    logfile.write ('\n summary of the results at the last iteration\n\n')
    for elem in summary_output:
        writing = elem[0] + '\t' + str (elem[1]) + '\t' + str (int (elem[2]))
        for ind in range (3, len(elem)-1):
           writing = writing + '\t{0:04.2e}'.format(elem[ind])
        writing = writing + '\t' + str (elem[-1])
        logfile.write (writing + '\n')

    # summary plots
    steps = set ([elem[1] for elem in summary_output])
    for step in steps: 

        # the calculation result

        y_val_tota = [elem[5] for elem in summary_output if (elem[1] == step)]
        y_err_tota = [elem[6] for elem in summary_output if (elem[1] == step)]
        x_label = [elem[0]+' '+str(elem[2]) for elem in summary_output if (elem[1] == step)]
        x_val = range (len (x_label))
        x_err = [0] * len (x_val)
        fig = plt.figure ()
        plt.errorbar (x_val, y_val_tota, xerr=x_err, yerr=y_err_tota)
        plt.xticks (x_val, x_label, rotation='vertical')
        plt.title (step + ' results\n')
        plt.ylabel ('integral')
        plt.yscale ('log')
        plt.xlim((-1,len (x_label)))
        plt.subplots_adjust (bottom=0.5)
        fig.savefig (plotsFile, format = 'pdf')

        # the chisquare

        y_val_tota = [elem[7] for elem in summary_output if (elem[1] == step)]
        x_label = [elem[0]+' '+str(elem[2]) for elem in summary_output if (elem[1] == step)]
        x_val = range (len (x_label))
        fig2 = plt.figure ()
        plt.plot (x_val, y_val_tota, color ='r')
        plt.xticks (x_val, x_label, rotation='vertical')
        plt.title (step + ' chi2\n')
        plt.ylabel ('chi2')
        plt.xlim((-1,len (x_label)))
        plt.subplots_adjust (bottom=0.5)
        fig2.savefig (plotsFile, format = 'pdf')

        # the relative error

        y_val_tota = []
        for elem in summary_output:
            if (elem[1] != step): continue
            if (elem[5] == 0.): y_val_tota.append (0.)
            else              : y_val_tota.append (elem[6]/elem[5])
        x_label = [elem[0]+' '+str(elem[2]) for elem in summary_output if (elem[1] == step)]
        x_val = range (len (x_label))
        fig2 = plt.figure ()
        plt.plot (x_val, y_val_tota, color ='r')
        plt.xticks (x_val, x_label, rotation='vertical')
        plt.title (step + ' relative error\n')
        plt.ylabel ('rel. error')
        plt.xlim((-1,len (x_label)))
        plt.subplots_adjust (bottom=0.5)
        fig2.savefig (plotsFile, format = 'pdf')

    # plot the alphas (importance of each integration step)
    alfas = []
    for fil in processoutputs:
        process = fil.split ('/')[-2]
        outfile = open (fil, 'r')
        content = outfile.read ().split ('\n')
        for ind in range(len(content)-1, 0, -1):
            if 'iphs_ind=' in content[ind] and 'alfa=' in content[ind]:
                channel = content[ind].split()[1]
                alfa = content[ind].split()[3]
                alfas.append ([process,channel,alfa])
                break
        outfile.close ()
                                
    y_val = [float (elem[2]) for elem in alfas]
    x_label = [elem[0]+' '+elem[1] for elem in alfas]
    x_val = range (len (x_label))
    fig = plt.figure ()
    plt.plot (x_val, y_val)
    plt.xticks (x_val, x_label, rotation='vertical')
    plt.title ('ALFA VALUE')
    plt.ylabel ('value')
    plt.yscale ('log')
    plt.xlim((-1, len (x_label)))
    plt.ylim(( 0.8 * min (y_val), 2 * max (y_val) ))
    plt.subplots_adjust (bottom=0.5)
    fig.savefig (plotsFile, format = 'pdf')

    # plot the cross-section
    sigmas = []
    for fil in processoutputs:
        process = fil.split ('/')[-2]
        outfile = open (fil, 'r')
        content = outfile.read ().split ('\n')
        for ind in range(len(content)-1, 0, -1):
            if 'SIGMA' in content[ind]:
                sigma_val = content[ind].split()[2]
                sigma_err = content[ind].split()[4]
                sigmas.append ([process,sigma_val,sigma_err])
                break
        outfile.close ()
                                
    y_val = [float (elem[1].replace ('D','E')) for elem in sigmas]
    y_err = [float (elem[2].replace ('D','E')) for elem in sigmas]
    x_label = [elem[0] for elem in sigmas]
    x_val = range (len (x_label))
    x_err = [0] * len (x_val)
    fig = plt.figure ()
    plt.errorbar (x_val, y_val, xerr=x_err, yerr=y_err)
    plt.xticks (x_val, x_label, rotation='vertical')
    plt.title ('SIGMA\n')
    plt.ylabel ('cross-section (pb)')
    plt.yscale ('log')
    plt.xlim((-1,len (x_label)))
    plt.subplots_adjust (bottom=0.5)
    fig.savefig (plotsFile, format = 'pdf')

    # check that the uncertainty on the total XS is small
    # this check stays at the end, to run all the steps before anyhow
    
    summaryfile = open (workingfolder + '/result', 'r')
    totalCrossSection = []
    for line in summaryfile.read ().split ('\n'):
        if 'total cross section=' in line:
            words = line.split ()
            totalCrossSection.append (float (words[3]))
            totalCrossSection.append (float (words[5]))
            break
    print 'TOTAL XS = ' + str (totalCrossSection[0]) + ' +/- ' + str (totalCrossSection[1]) + ' pb'
    logfile.write ('TOTAL XS = ' + str (totalCrossSection[0]) + ' +/- ' + str (totalCrossSection[1]) + ' pb\n') 

    if totalCrossSection[1] / totalCrossSection[0] > 0.02 :
        finished = False
        print 'Uncertainty on the total XS is too large:', str (totalCrossSection[1] / totalCrossSection[0])
        logfile.write ('Uncertainty on the total XS is too large: ' + str (totalCrossSection[1] / totalCrossSection[0]) + '\n') 
    summaryfile.close ()

    plotsFile.close ()
    print 'graphic summary of the calculation written in : ', plotsFileName
    print 'logfile written in : ', logfile.name

    if finished == False :
        print '\n     GRIDPACK PREPARATION FAILED\n'
        logfile.write ('\n     GRIDPACK PREPARATION FAILED\n')
 
    # check the results of each step of the grid integration,
    # for each of the processes that have been generated
  

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


''' generate the gridpack for the wmLHE prodcution framework
'''
def gridpackGeneration (debugging):
    rootfolder = os.getcwd ()
    # values to be inserted in the phantom r.in file
    substitute = {}
    config = ConfigParser.ConfigParser ()
    config.optionxform = str # to preserve the case when reading options
    print 'reading config file:',sys.argv[1]
    config.read (sys.argv[1])
    for section in config.sections ():
        options = config.options (section)
        for option in options:
            print '[' + section + '] ' + option, ':', config.get (section, option)
            if (section == 'parameters') :
                substitute [option] = config.get (section, option)
    # this means gridpack production
    substitute['ionesh'] = '0'

    # prepare the folder for the gridpack creation
    foldername = os.getcwd () + '/phantomGrid_' + datetime.datetime.now().strftime('%y-%m-%d-%H-%M')
    if config.has_option ('general', 'foldername') :
        foldername = os.getcwd () + '/' + config.get ('general', 'foldername')
    if os.path.exists (foldername + '.tar.xz'):
        print 'gridpack ' + foldername + '.tar.xz already existing, exiting'
        sys.exit (1)
    print 'creating gridpack in folder: ' + foldername
    os.system ('rm -rf ' + foldername)
    os.system ('mkdir ' + foldername)
    os.chdir (foldername)
    workingfolder = os.getcwd ()

    res = getPhantom (config, workingfolder, debugging)
    phantomfolder = res[0]
    phantompdflib = res[1]
    phantom = res[2]

    # get the cmssw environment and the location of the pdf grids
    cmssw = config.get ('general', 'CMSSW')
    scram_arch = config.get ('general', 'ARCH')
    os.environ['SCRAM_ARCH'] = scram_arch
    shell = 'sh'
    if os.environ['SHELL'].find ('c') != -1 :
        shell = 'csh'
    returnCode = execute ('scram -a ' + scram_arch + ' project CMSSW ' + cmssw, debugging)
    if returnCode[0] != 0 :
        print 'cmssw release: ', cmssw, 'not found, exiting'
        sys.exit (1)
    os.chdir (cmssw + '/src')
    execute ('pwd', debugging)
    result = execute ('eval `scram runtime -' + shell + '` ; printenv | grep LHAPDF_DATA_PATH', debugging)
    os.chdir (workingfolder)

    # get the pdf grid
#    execute ('printenv | grep LHAPDF_DATA_PATH', debugging)
#    pdfgridfolder = os.environ['LHAPDF_DATA_PATH']

    pdfgridfolder = '' 
    for line in result[1].split('\n'):
        if line.startswith ('LHAPDF_DATA_PATH=') :
            pdfgridfolder = line[len ('LHAPDF_DATA_PATH='):]
            break
    if debugging:
        print 'using pdf grids at: ' + pdfgridfolder
    pdfgrids = pdfgridfolder + '/' + config.get ('parameters','PDFname')
    if not os.path.exists (pdfgrids):
        print 'PDF grids ' + pdfgrids + ' not found, exiting'
        sys.exit (1)
    # NB this should happen before creating the phantom config file!
    # for the latest pdfs it seems that the pdf name is enough
    # and the path gets truncated
    # substitute['PDFname'] = pdfgrids
    substitute['PDFname'] = config.get ('parameters','PDFname')

    # prepare the r.in phantom config file
    templatefile = 'nonEsisto'
    if config.has_option ('general', 'template'):
        templatefile = rootfolder + '/' + config.get ('general', 'template')
    if not os.path.exists (templatefile):
        templatefile = workingfolder + '/' + phantomfolder + '/r.in'
        print 'r.in template not found, using the default one in the phantom release'
    replaceParameterInFile (templatefile, workingfolder + '/r.in', substitute)

    # get the setupdir2 script from the phantom folder
    execute ('cp ' + workingfolder + '/' + phantomfolder + '/tools/setupdir2.pl ' + workingfolder, debugging)

    channel = config.get ('generation','channel')
    command = './setupdir2.pl'
    command += ' -b ' + workingfolder + '/' + phantomfolder
    if int (config.get ('generation', 'topnumber')) > 0 :
        command += ' -T ' + config.get ('generation', 'topnumber')
    command += ' -d ' + workingfolder
    command += ' -t ' + workingfolder + '/r.in'
    command += ' -i "' + channel + '" -q ' + str (8 - len (channel.split ()))
    command += ' -s ' + config.get ('submission','scheduler') + ' -n ' + config.get ('submission', 'queue')
    if substitute['i_signal'] == '1' :
        command += ' -Hs '
    elif substitute['i_signal'] == '2' :
        command += ' -S '
    submitfilename = workingfolder + '/' + config.get ('submission','scheduler') + 'file'

    # create the executable script to submit the gridpack production
    execute (command, debugging)

    # in case of QCD or QCD+EWK generation requests
    # FIXME the option to merge the case with external gluons or not
    #       still needs to be implemented
    if (config.get ('parameters','perturbativeorder') == '2' or config.get ('parameters','perturbativeorder') == '3') and (config.get('generation', 'excludegluons') == '0'):
        submitsinglegrids = []
        submitfile = open (submitfilename, 'read')
        for line in submitfile.readlines () :
            if 'bsub' in line: submitsinglegrids.append (line)
        submitfile.close ()

        # modify the channel variable adding the gluon legs
        channel = channel + ' g g'

        command = './setupdir2.pl'
        command += ' -b ' + workingfolder + '/' + phantomfolder
        if int (config.get ('generation', 'topnumber')) > 0 :
            command += ' -T ' + config.get ('generation', 'topnumber')
        command += ' -d ' + workingfolder
        command += ' -t ' + workingfolder + '/r.in'
        command += ' -i "' + channel + '" -q ' + str (8 - len (channel.split ()))
        command += ' -s ' + config.get ('submission','scheduler') + ' -n ' + config.get ('submission', 'queue')
        if substitute['i_signal'] == '1' :
            command += ' -Hs '
        elif substitute['i_signal'] == '2' :
            command += ' -S '

        # create the executable script to submit the gridpack production
        # for the component w/ real gluons
        execute (command, debugging)

        # extend the executable script with the part w/o real gluons
        submitfile = open (submitfilename, 'a')
        submitfile.write ('\n')
        for line in submitsinglegrids :
            submitfile.write (line)
        submitfile.close ()

    # get the list of process output files
    processoutputs = []
    submitfile = open (submitfilename, 'read')
    for line in submitfile.readlines () :
        if 'bsub' in line: processoutputs.append (line.split()[6])
    submitfile.close ()

    # add to the submit file the environment setupdir
    # configuration to be setup before running the phantom program
    # modifySubmitfileIntelCompiler (submitfilename, pdfgridfolder, phantompdflib)
    modifySubmitfileCMSSWCompiler (submitfilename, pdfgridfolder, phantompdflib, cmssw, shell, scram_arch)

    # launch the submission script
    execute ('source ' + submitfilename, debugging)

    # wait until all jobs finish, before calculating the cross-section
    # and compressing the gridpack
    finished = False
    while not finished:
        finished = True
        unfinished = int (0)
        for fil in processoutputs:
            if not os.path.exists (fil):
                finished = False
                unfinished += 1
        sys.stdout.write ('waiting for: ' + str (unfinished) + ' jobs\r' )
        sys.stdout.flush ()
        time.sleep (60) # seconds

    # log file of the generation parameters
    logfilename = workingfolder + '/log_GRID.txt'
    logfile = open (logfilename, 'write')

    # calculate the cross-section
    command = 'cd ' + workingfolder + '; grep SIGMA */run.out > res ; '
    command += workingfolder + '/' + phantomfolder + '/tools/totint.exe > result '
    execute (command, debugging)
    result = execute ('tail -n 1 ' + workingfolder + '/result', debugging)
    Xsection = result[1] + ' pb'

    verifyGridpack (processoutputs, workingfolder, logfile)

    logfile.write ('CONFIG FILE\n\n')
    for section in config.sections ():
        options = config.options (section)
        for option in options:
            logfile.write ('[' + section + '] ' + option + ' : ' + config.get (section, option) + '\n')
    logfile.write ('\nSETUP COMMAND\n\n')
    logfile.write (command + '\n')
    logfile.write ('\nRESULTING CROSS-SECTION FROM GRID CREATION\n\n')
    logfile.write (Xsection + '\n')
    logfile.close ()

    # NB removing the phantom to save space, it will have to be put back (reasonably in the same place)
    #    when generating the events
    if not debugging:
        execute ('rm -rf ' + phantomfolder + '*', debugging)
        execute ('rm -rf CMSSW*', debugging)
        execute ('rm -rf LSFJOB*', debugging)

    # get all the grids, to be run in workingfolder
#    gridfiles = execute ('for fil in `find -L . -name "phavegas*.dat"` ; do echo `pwd`/$fil ; done', debugging)
    gridfiles = execute ('for fil in `find -L . -name "phavegas*.dat"` ; do echo $fil ; done', debugging)

    # prepare the r.in file for the event production, starting from the template one
    replacement = {'ionesh':'1\n', 'nfiles': str(len (gridfiles[1].split ()))+'\n', 'nunwevts':'EVENTSNUM\n', 'idum':'-RANDOMSEED\n'}
    replaceParameterInFile (workingfolder + '/r.in', workingfolder + '/r_GEN.in', replacement)
    addGridsToRin (workingfolder + '/r_GEN.in', gridfiles[1], debugging)

    execute ('mv r.in r_GRID.in', debugging)

    # prepare the script for the event generation

    execute ('cp ../' + sys.argv[1] + ' ./', debugging)

    prepareEventProductionScript (foldername + '/runcmsgrid.sh', phantom, phantomfolder, cmssw, shell, scram_arch, debugging)

    execute ('tar cJf ../' + foldername.split ('/')[-1] + '.tar.xz *', debugging)
    print 'gridpack ' + foldername.split ('/')[-1] + '.tar.xz created'

    os.chdir (rootfolder)
    sys.exit (0)


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def produceEvents (nevents, ngenerations, queue, debugging=True):
    # Parse the configuration
    substitute = {}
    config = ConfigParser.ConfigParser ()
    config.optionxform = str # to preserve the case when reading options
    print 'reading config file:',sys.argv[1]
    config.read (sys.argv[1])
    for section in config.sections ():
        options = config.options (section)
        for option in options:
            print '[' + section + '] ' + option, ':', config.get (section, option)
            if (section == 'parameters') :
                substitute [option] = config.get (section, option)

    scram_arch = config.get ('general', 'ARCH')
    os.environ['SCRAM_ARCH'] = scram_arch

    foldername = os.getcwd() + '/' + config.get ('general', 'foldername')
    if not os.path.exists (foldername):
        print "gridpack folder {} doesn't exist! exiting".format(foldername)
        sys.exit(1)
    os.chdir(foldername)

    workingfolder = os.getcwd ()
    res = getPhantom (config, workingfolder, debugging)
    phantomfolder = res[0] 
    phantompdflib = res[1]
    
    if debugging==1:
        print 'phantomfolder', phantomfolder
        print 'phantompdflib', phantompdflib
    
    # Creating folders for generation
    try:
        os.makedirs("generations/gen1")
    except Exception:
        print "generation directories already exist. exiting"
        sys.exit(1)
    os.chdir ("generations")

    execute ('cp ../'+ phantomfolder +'/tools/gendir.scr .', debugging)
    # Copying the r_GEN.in prepared in the gridpack
    # changing the EVENTSNUM and SEED variable and fixing the vegas files path
    rin = open("../r_GEN.in", "r")
    rout = open("gen1/r.in", "w")
    rin_text = rin.read()
    # Fix variables
    rin_text = rin_text.replace ("EVENTSNUM", str(nevents))
    rin_text = rin_text.replace ("RANDOMSEED", "123456789")
    # Match nfiles section
    match = re.search( r"nfiles\s*(?P<lines>\d+)\n", rin_text)
    rout.write(rin_text[:match.end()])
    # Now fixing the path of the files
    nfiles = int(match.group("lines"))
    files = rin_text[match.end():].split("\n")
    for i in range(nfiles):
        rout.write("../."+ files[i] +"\n")
    rin.close()
    rout.close()

    # Create the run file
    runFile = open ('gen1/run','w')
#    runFile.write('#!/bin/bash\n')
    runFile.write ('pwd\n')
    runFile.write ('cd '+ os.getcwd() + '/gen1\n')
    runFile.write ('pwd\n')
    runFile.write ('rm *.dat\necho $HOSTNAME\ntouch running\ndate\n')
    runFile.write ('unbuffer ../../'+ phantomfolder +"/phantom.exe\n")
    # Rename the data from phamom.dat to phamom.lhe
    runFile.write ("mv phamom.dat phamom.lhe\n")
    runFile.write ('mv running finished\ndate')
    runFile.close ()

    cmssw = config.get ('general', 'CMSSW')
    modifySubmitfileCMSSWCompiler ('gen1/run', "", "", cmssw, "sh", scram_arch)

    # Now run the phantom tool to generate the folders
    execute ("./gendir.scr -l CERN -q " + queue + " -d "+ str(ngenerations) + " -i `pwd`", debugging)
    # We have now a submit file create, we have to add the CMSSW activation at the beginning
#    modifySubmitfileCMSSWCompiler(os.getcwd() +'/submitfile', "", "", cmssw, "sh", scram_arch, cmssw_path="../")

    # Execute the submit script and launch the generation
    print "Launching the generation of {0} events * {1} generations = {2} events".format(nevents,
            ngenerations, nevents*ngenerations)
    execute('source '+ os.getcwd() +'/submitfile', debugging)
    
    print '\nAt the end of the events production, please run the following to get the total XS,'
    print 'to be compared to the one calculated in the gridpack production and stored'
    print 'in the file "result" as a cross-check of the gridpack\n' 
    print 'cd ' + workingfolder
    print 'grep -A 1 total\ integral generations/gen*/run.o* > res'    
    print workingfolder + '/' + phantomfolder + '/tools/gentotint.exe > result_gen.txt'


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def runSimpleVerification (configfilename):
    # Parse the configuration
    substitute = {}
    config = ConfigParser.ConfigParser ()
    config.optionxform = str # to preserve the case when reading options
    print 'reading config file:',configfilename
    config.read (configfilename)
    for section in config.sections ():
        options = config.options (section)
        for option in options:
            print '[' + section + '] ' + option, ':', config.get (section, option)
            if (section == 'parameters') :
                substitute [option] = config.get (section, option)

    scram_arch = config.get ('general', 'ARCH')
    os.environ['SCRAM_ARCH'] = scram_arch

    # open the gridpack
    foldername = os.getcwd() + '/' + config.get ('general', 'foldername')
    if not os.path.exists (foldername):
        print "gridpack folder {} doesn't exist! exiting".format(foldername)
        sys.exit(1)
    os.chdir(foldername)

    workingfolder = os.getcwd ()

    # open logfile of the verification
    logfilename = workingfolder + '/log_VERIFY.txt'
    logfile = open (logfilename, 'write')

    # get the list of process output files
    submitfilename = workingfolder + '/' + config.get ('submission','scheduler') + 'file'
    processoutputs = []
    submitfile = open (submitfilename, 'read')
    for line in submitfile.readlines () :
        if 'bsub' in line: 
            outfilename = line.split()[6].split (config.get ('general', 'foldername'))
#            print foldername + outfilename [1]
#            print outfilename
            processoutputs.append (foldername + outfilename [1])
    submitfile.close ()

    verifyGridpack (processoutputs, workingfolder, logfile)

    logfile.close ()


# ------------------------------------------------------------------------------


if __name__ == '__main__':
    if len (sys.argv) < 2 :
        print 'config file missing, exiting'
        sys.exit (1)

    verbose = False
    if len (sys.argv) > 2 :
        arg2 = sys.argv[2]
        if arg2 == "--verbose":
            verbose = True
        elif arg2 == "--produce":
            produce = True
            if len(sys.argv) < 5:
                print "Number of events and generations missing, exiting"
                sys.exit(1)
            else:
                nevents_produce = int(sys.argv[3])
                ngenerations_produce = int(sys.argv[4])
                queue = '8nh'
                if len (sys.argv) > 5 : queue = sys.argv[5]
                produceEvents (nevents_produce, ngenerations_produce, queue)
                sys.exit (0)
        elif arg2 == "--test":
            runSimpleVerification (sys.argv[1])
            sys.exit (0)

    gridpackGeneration (verbose)
            
    sys.exit (0)
