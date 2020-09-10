COM_ENERGY = 13000. # GeV
MASS_POINT = 300   # GeV
CROSS_SECTION = 1 # pb
CTAU_POINT = 30 # mm

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(COM_ENERGY),
    crossSection = cms.untracked.double(CROSS_SECTION),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            "Higgs:useBSM = on",
            "HiggsBSM:all = off",
            "HiggsBSM:ffbar2A3H2 = on",
            "35:m0 = %s" % MASS_POINT,
            "36:m0 = %s" % MASS_POINT,
            "35:tau0 = %s" % CTAU_POINT,
            "36:tau0 = %s" % CTAU_POINT,
            "35:0:bRatio = .2",
            "35:1:bRatio = .2",
            "35:2:bRatio = .2",
            "35:3:bRatio = .2",
            "35:4:bRatio = .2",
            "35:5:bRatio =  0",
            "35:9:bRatio =  0",
            "35:10:bRatio=  0",
            "36:0:bRatio = .2",
            "36:1:bRatio = .2",
            "36:2:bRatio = .2",
            "36:3:bRatio = .2",
            "36:4:bRatio = .2",
            "36:5:bRatio =  0",
            "36:9:bRatio =  0",
            "36:10:bRatio =  0",
            "35:0:meMode = 100",
            "35:1:meMode = 100",
            "35:2:meMode = 100",
            "35:3:meMode = 100",
            "35:4:meMode = 100",
            "35:5:meMode = 100",
            "35:9:meMode = 100",
            "35:10:meMode = 100",
            "36:0:meMode = 100",
            "36:1:meMode = 100",
            "36:2:meMode = 100",
            "36:3:meMode = 100",
            "36:4:meMode = 100",
            "36:5:meMode = 100",
            "36:9:meMode = 100",
            "36:10:meMode = 100",
            "HiggsA3:coup2d = 1",
            "HiggsA3:coup2u = 1",
            "HiggsA3:coup2H1Z = 0",
            "HiggsA3:coup2H2Z = 1",
            "HiggsA3:coup2l = 0",
            "HiggsA3:coup2HchgW = 0",
            "HiggsH2:coup2d = 1",
            "HiggsH2:coup2u = 1",
            "HiggsH2:coup2l = 0",
            "HiggsH2:coup2Z = 0",
            "HiggsH2:coup2W = 0",
            "HiggsH2:coup2H1H1 = 0",
            "HiggsH2:coup2A3A3 = 0",
            "35:onMode = off",
            "35:onIfAny = 1 2 3 4 5",
            "36:onMode = off",
            "36:onIfAny = 1 2 3 4 5",
        ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
            'processParameters'
        )
    )
)

ProductionFilterSequence = cms.Sequence(generator) 
