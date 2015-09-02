# Settings from https://github.com/cms-sw/genproductions/blob/master/python/ThirteenTeV/Hadronizer_TuneCUETP8M1_13TeV_MLM_5f_max1j_LHE_pythia8_cff.py  
# as recommended by https://twiki.cern.ch/twiki/bin/viewauth/CMS/QuickGuideMadGraph5aMCatNLO#2_LO_with_jet_matching_multiple 

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

SLHA_TABLE = """# This file is read by SimG4Core/CustomPhysics/src/CustomParticleFactory.cc  
# The strings "decay", "pdg code", and "block", with correct capitalization, are used 
# to control the data input, so do not use these in any comments.  
# 
# 
# Get values for chargino and neutralino masses from:  
# ../data/AMSB_chargino_100GeV_Isajet780.slha
BLOCK MASS   
#  PDG code   mass   particle 
   1000022   9.98443985E+01   # ~neutralino(1) 
   1000024   1.00032974E+02   # ~chargino(1)+  
  -1000024   1.00032974E+02   # ~chargino(1)-  
Block 



# Set chargino lifetime 
# and decay:  chargino -> neutralino + pion 
# chargino ctau  = 100 cm 
# chargino  tau  = 3.33564604793 ns 
# chargino width = 1.97326691904e-16 GeV 
#       PDG       Width               #
DECAY  1000024  1.97326691904e-16 # +chargino decay  
#   BR       NDA      ID1      ID2  
   1.0000    2     1000022    211   
Block 


#       PDG       Width               #
DECAY  -1000024  1.97326691904e-16 # -chargino decay  
#   BR       NDA      ID1      ID2  
   1.0000    2     1000022    -211  
Block 


EOF 
"""


generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    UseExternalGenerators = cms.untracked.bool(True),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'JetMatching:setMad = off',
            'JetMatching:scheme = 1',
            'JetMatching:merge = on',
            'JetMatching:jetAlgorithm = 2',
            'JetMatching:etaJetMax = 5.',
            'JetMatching:coneRadius = 1.',
            'JetMatching:slowJetPower = 1',
            'JetMatching:qCut = 30.', #this is the actual merging scale
            'JetMatching:nQmatch = 5', #4 corresponds to 4-flavour scheme (no matching of b-quarks), 5 for 5-flavour scheme
            'JetMatching:nJetMax = 1', #number of partons in born matrix element for highest multiplicity
            'JetMatching:doShowerKt = off', #off for MLM matching, turn on for shower-kT matching
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters',
                                    )
    ),
    # The following parameters are required by Exotica_HSCP_SIM_cfi:  
    slhaFile = cms.untracked.string(''),   # value not used
    processFile = cms.untracked.string('SimG4Core/CustomPhysics/data/RhadronProcessList.txt'),
    useregge = cms.bool(False), 
    hscpFlavor = cms.untracked.string('stau'),          
    massPoint = cms.untracked.int32(100),  # value not used 
    particleFile = cms.untracked.string('%s' % SLHA_TABLE), 
)


ProducerSourceSequence = cms.Sequence(generator)



