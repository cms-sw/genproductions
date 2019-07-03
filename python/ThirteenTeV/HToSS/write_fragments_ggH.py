#! /usr/bin/env python

import os, sys, getopt, multiprocessing
import copy, math
from array import array

masses = [a for a in range(15,61,5)]
ctaus = [0, 0.05, 0.1, 1, 5, 10, 25, 50, 100, 500, 1000, 2000, 5000, 10000]

DATACARDDIR = "Configuration/GenProduction/python/ThirteenTeV/HToSS/"

for m in masses:

    for c in ctaus:

	if c==0.05:
	    ci = "0p05"
	elif c==0.1:
	    ci = "0p1"
	else:
	    ci = str(c)
        card =  "import FWCore.ParameterSet.Config as cms \n"
        card += "externalLHEProducer = cms.EDProducer('ExternalLHEProducer', \n"
        card += "    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc6_amd64_gcc481/13TeV/powheg/V2/gg_H_quark-mass-effects_NNPDF30_13TeV_M125/v2/gg_H_quark-mass-effects_NNPDF30_13TeV_M125_tarball.tar.gz'), \n"
        card += "    nEvents = cms.untracked.uint32(60000), \n"
        card += "    numberOfParameters = cms.uint32(1), \n"
        card += "    outputFile = cms.string('cmsgrid_final.lhe'), \n"
        card += "    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh') \n"
        card += ") \n"
        card += " \n"
        card += "# Link to cards: \n"
        card += "# https://github.com/cms-sw/genproductions/blob/6718f234d90e34ac43b683e323a3edd6e8aa72e2/bin/Powheg/production/V2/13TeV/Higgs/gg_H_quark-mass-effects_NNPDF30_13TeV/gg_H_quark-mass-effects_NNPDF30_13TeV_M125.input \n"
        card += " \n"
        card += "import FWCore.ParameterSet.Config as cms \n"
        card += "from Configuration.Generator.Pythia8CommonSettings_cfi import * \n"
        card += "from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import * \n"
        card += "from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import * \n"
        card += " \n"
        card += "generator = cms.EDFilter('Pythia8HadronizerFilter', \n"
        card += "                         maxEventsToPrint = cms.untracked.int32(1), \n"
        card += "                         pythiaPylistVerbosity = cms.untracked.int32(1), \n"
        card += "                         filterEfficiency = cms.untracked.double(1.0), \n"
        card += "                         pythiaHepMCVerbosity = cms.untracked.bool(False), \n"
        card += "                         comEnergy = cms.double(13000.), \n"
        card += "                         PythiaParameters = cms.PSet( \n"
        card += "        pythia8CommonSettingsBlock, \n"
        card += "        pythia8CUEP8M1SettingsBlock, \n"
        card += "        pythia8PowhegEmissionVetoSettingsBlock, \n"
        card += "        processParameters = cms.vstring( \n"
        card += "            'POWHEG:nFinal = 1',   ## Number of final state particles \n"
        card += "                                   ## (BEFORE THE DECAYS) in the LHE \n"
        card += "                                   ## other than emitted extra parton \n"
        card += "            '9000006:all = sk   skbar    0        0          0       "+str(m)+"  1.9732e-14  1.0  75.0 "+str(c)+"', \n"
        card += "            '9000006:oneChannel = 1  1.0 101  5 -5', \n"
        card += "            '9000006:mayDecay = on', \n"
        card += "            '9000006:isResonance = on', \n"
        card += "            '25:m0 = 125.0', \n"
        card += "            '25:onMode = off', \n"
        card += "            '25:addChannel = 1 0.000000001 101 9000006 -9000006', \n"
        card += "            '25:onIfMatch = 9000006 -9000006', \n"
        card += "            '9000006:onMode = off', \n"
        card += "            '9000006:onIfAny = 5', \n"
        card += "        ), \n"
        card += "        parameterSets = cms.vstring('pythia8CommonSettings', \n"
        card += "                                    'pythia8CUEP8M1Settings', \n"
        card += "                                    'pythia8PowhegEmissionVetoSettings', \n"
        card += "                                    'processParameters' \n"
        card += "                                    ) \n"
        card += "        ) \n"
        card += "                         ) \n"
        card += " \n"
        card += "ProductionFilterSequence = cms.Sequence(generator) \n"
        card += " \n"
        card += "# Link to example generator fragment: \n"
        card += "# https://github.com/lbenato/genproductions/blob/master/python/ThirteenTeV/HToSS/NLO_HToSSTobbbb_MH125_MS40_ctauS1_13TeV.py \n"
        outname = DATACARDDIR + "GluGluH_HToSSTobbbb_MH-125_MS-"+str(m)+"_ctauS-"+str(ci)+"_13TeV.py"
        cardfile = open(outname, 'w')
        cardfile.write(card)
        cardfile.close()
        print "Written",outname
