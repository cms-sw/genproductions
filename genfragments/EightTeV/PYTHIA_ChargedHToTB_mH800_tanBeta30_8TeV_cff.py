import FWCore.ParameterSet.Config as cms

# Import settings for modules
from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.),
    PythiaParameters = cms.PSet(processParameters=cms.vstring('Higgs:useBSM = on',
                                'HiggsHchg:coup2H1W = 0.0',
                                '25:m0 = 125.',
                                '35:m0 = 800.',
                                '36:m0 = 800.',
                                '37:m0 = 800.',
                                'HiggsHchg:tanBeta = 30',
                                #'HiggsBSM:allH+- = on',
                                'HiggsBSM:bg2H+-t  = on',
                                '37:onMode = 0',
                                '37:onIfAny = 6'),
                                parameterSets = cms.vstring('processParameters')
                                 )
)
