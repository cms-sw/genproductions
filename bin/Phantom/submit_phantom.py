#!/usr/bin/python
# - upload the script to my version of genproduction
# - prepare the script for the event generation
# - upload it to genproduction and find a way to test it


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


def modifySubmitfile (submitfilename, pdfgridfolder, phantompdflib):
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
    

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def wordInFile (word, filename):
    thefile = open (filename, 'r')
    s = thefile.read ()
    thefile.close ()
    if word in s: return True
    return False

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


if __name__ == '__main__':

    if len (sys.argv) < 2 :
        print 'config file missing, exiting'
        sys.exit (1)

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
    if os.path.exists (foldername + '.tgz'):
        print 'gridpack ' + foldername + '.tgz already existing, exiting'
        sys.exit (1)
    print 'creating gridpack in folder: ' + foldername ;
    os.system ('rm -rf ' + foldername)
    os.system ('mkdir ' + foldername)
    os.chdir (foldername)
    workingfolder = os.getcwd ()

    # get the precompiled phantom code, untar it and get the name of the pdf libraries
    # used in the compilation
    phantom = config.get ('general', 'package')
    if not os.path.isfile (phantom):
        print 'Phantom package ' + phantom + ' not found, exiting'
        sys.exit (1)
    execute ('cp ' + phantom + ' ' + workingfolder, True)
    execute ('tar xzf ' + phantom.split ('/')[-1], True)
    phantomfolder = phantom.split ('/')[-1]
    dummy = '.tar.gz'
    phantomfolder = phantomfolder[0:-len(dummy)] if phantomfolder.endswith(dummy) else phantomfolder    
    dummy = '.tgz'
    phantomfolder = phantomfolder[0:-len(dummy)] if phantomfolder.endswith(dummy) else phantomfolder    
    phantompdflib = findLhapdfUsed (workingfolder + '/' + phantomfolder + '/makefile')

    # get the cmssw environment and the location of the pdf grids
    cmssw = config.get ('general', 'CMSSW')
    os.environ['SCRAM_ARCH'] = config.get ('general', 'ARCH')
    shell = 'sh'
    if os.environ['SHELL'].find ('c') != -1 :
        shell = 'csh'
    returnCode = execute ('scram project CMSSW ' + cmssw, True)
    if returnCode[0] != 0 :
        print 'cmssw release: ', cmssw, 'not found, exiting'
        sys.exit (1)
    os.chdir (cmssw + '/src')
    execute ('pwd', True)
    result = execute ('eval `scram runtime -' + shell + '` ; printenv | grep LHAPDF_DATA_PATH', True)
    os.chdir (workingfolder)
    # get the pdf grid
#    execute ('printenv | grep LHAPDF_DATA_PATH', True)
#    pdfgridfolder = os.environ['LHAPDF_DATA_PATH']
    for line in result[1].split('\n'):
        if line.startswith ('LHAPDF_DATA_PATH=') : 
            pdfgridfolder = line[len ('LHAPDF_DATA_PATH='):]
            break
    pdfgrids = pdfgridfolder+'/' + config.get ('parameters','PDFname')
    if not os.path.exists (pdfgrids):
        print 'PDF grids ' + pdfgrids + ' not found, exiting'
        sys.exit (1)    
    # NB this should happen before creating the phantom config file!
    substitute['PDFname'] = pdfgrids

    # prepare the r.in phantom config file
    templatefile = rootfolder + '/' + config.get ('general', 'template')
    if not os.path.exists (templatefile):
        templatefile = workingfolder + '/' + phantomfolder + '/r.in'
        print 'r.in template not found, using the default one in the phantom release'
        sys.exit (1)
    replaceParameterInFile (templatefile, workingfolder + '/r.in', substitute)

    # get the setupdir2 script from the phantom folder
    execute ('cp ' + workingfolder + '/' + phantomfolder + '/tools/setupdir2.pl ' + workingfolder, True)    

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
    submitfilename = workingfolder + '/LSFfile'

    # create the executable script to submit the gridpack production
    execute (command, True)

    # in case of QCD or QCD+EWK generation requests
    if config.get ('parameters','perturbativeorder') == '2' or config.get ('parameters','perturbativeorder') == '3':
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
        execute (command, True)
        
        # extend the executable script with the part w/o real gluons
        submitfile = open (submitfilename, 'a')
        submitfile.write ('\n')
        for line in submitsinglegrids :
            submitfile.write (line)
        submitfile.close ()

    # get the list of process folders
    processoutputs = []
    submitfile = open (submitfilename, 'read')
    for line in submitfile.readlines () :
        if 'bsub' in line: processoutputs.append (line.split()[6])
    submitfile.close ()

    # add to the LSFfile the environment setupdir
    # configuration to be setup before running the phantom program
    modifySubmitfile (submitfilename, pdfgridfolder, phantompdflib)

    # launch the submission script
    execute ('source ' + workingfolder + '/LSFfile', True)

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
    logfilename = workingfolder + '/log.txt'
    logfile = open (logfilename, 'write')

    # verify that all jobs finished successfully,
    finished = True
    for fil in processoutputs:
        if not wordInFile ('SIGMA', fil):
            print 'the following job had issues:\n  ' + fil
            logfile.write ('the following job had issues: ' + fil)
            finished = False

    if finished == False :
        print 'gridpack preparation failed'
        logfile.write ('gridpack preparation failed')
        logfile.close ()
        exit (1)

    # calculate the cross-section
    command = 'cd ' + workingfolder + '; grep SIGMA */run.out > res ; '
    command += workingfolder + '/' + phantomfolder + '/tools/totint.exe > result '
    execute (command, True)            
    result = execute ('tail -n 1 ' + workingfolder + '/result', True)
    Xsection = result[1] + ' pb'

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
    execute ('rm -rf ' + phantomfolder + '*', True)
    execute ('rm -rf CMSSW*', True)
    execute ('rm -rf LSFJOB*', True)
    
    os.chdir (rootfolder)
    execute ('tar czf ' + foldername + '.tgz ' + foldername, True)
    print 'gridpack ' + foldername + '.tgz created'
    
    sys.exit (0)
    
