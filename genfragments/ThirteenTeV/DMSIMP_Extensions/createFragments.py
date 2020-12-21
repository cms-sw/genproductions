#!/usr/bin/env python
import os


dirs = ['DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-200_MXd-10','DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-200_MXd-50','DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-20_MXd-1','DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-300_MXd-50','DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-500_MXd-150'] #organizing directories.

Tarballs = ['/afs/cern.ch/work/c/cfreer/public/MonoZ/CMSSW_9_4_8/src/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-200_MXd-10/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-200_MXd-10_slc6_amd64_gcc481_CMSSW_7_1_30_tarball.tar.xz',
'/afs/cern.ch/work/c/cfreer/public/MonoZ/CMSSW_9_4_8/src/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-200_MXd-50/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-200_MXd-50_slc6_amd64_gcc481_CMSSW_7_1_30_tarball.tar.xz',
'/afs/cern.ch/work/c/cfreer/public/MonoZ/CMSSW_9_4_8/src/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-20_MXd-1/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-20_MXd-1_slc6_amd64_gcc481_CMSSW_7_1_30_tarball.tar.xz',
'/afs/cern.ch/work/c/cfreer/public/MonoZ/CMSSW_9_4_8/src/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-300_MXd-50/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-300_MXd-50_slc6_amd64_gcc481_CMSSW_7_1_30_tarball.tar.xz',
'/afs/cern.ch/work/c/cfreer/public/MonoZ/CMSSW_9_4_8/src/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-500_MXd-150/DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-500_MXd-150_slc6_amd64_gcc481_CMSSW_7_1_30_tarball.tar.xz']

m5000522 = ['1','1','0.1','1','1'] #Mass of stable dark matter

ctau = ['0.5', '1', '10', '100', '1000'] #decay lengths (cm)

filename = ['MX1-1_ctau-0p5.py','MX1-1_ctau-1.py','MX1-1_ctau-10.py','MX1-1_ctau-100.py','MX1-1_ctau-1000.py']

filename2 = ['MX1-0p1_ctau-0p5.py','MX1-0p1_ctau-1.py','MX1-0p1_ctau-10.py','MX1-0p1_ctau-100.py','MX1-0p1_ctau-1000.py']
count=0

for directory in dirs:
	print "creating directory:", directory
	os.makedirs(directory)
	count2=0
        for ctaus in ctau:
		FilePrep="""
import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('%s'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

# Link to cards:
# https://github.com/AndreasAlbert/genproductions/tree/monoz_LO_forDMLL/bin/MadGraph5_aMCatNLO/cards/production/13TeV/MonoZ

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8aMCatNLOSettings_cfi import *


generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        processParameters = cms.vstring(
            'ParticleDecays:tau0Max = 1000.1',
            'LesHouches:setLifetime = 2',
            'SLHA:useDecayTable = off', # use pythia8 decay mode instead of decays defined in LH accord
            '5000522:new',
            '5000522:m0 = %s',
            '5000522:isResonance = false',
            '5000522:onMode = off',
            '5000522:mayDecay = off',
            '52:mayDecay = on',
            '52:mWidth = 0.01',  # needs to be non-zero for Pythia to decay it
            '52:onMode = off',
            '52:addChannel = 1 1 100 5000522 1 -1',
            '52:onIfAny = 5000522 1 -1',
            '52:tau0 = %s'
            ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'processParameters',
                                    )
        )
    )

"""%(Tarballs[count],m5000522[count],ctaus)
		print "creating Fragments for this directory"
		if directory=='DMSimp_DM_ZLL_LO_Vector_GQ0p25_GDM1p0_MY1-20_MXd-1':
			f=open(directory + '/' + filename2[count2],"w+")
		else:
			f=open(directory + '/' + filename[count2],"w+")

		f.write(FilePrep)
		f.close()
		count2=count2+1
	count=count+1
	
