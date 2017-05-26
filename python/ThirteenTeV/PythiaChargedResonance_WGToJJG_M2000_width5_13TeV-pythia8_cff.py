import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    comEnergy = cms.double(13000.0),
    filterEfficiency = cms.untracked.double(1),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            "37:onMode = off", 
            "37:addChannel = 1 0.00001 101 24 22", 
            "37:onIfMatch = 24 22", 
            "37:m0 = 2000", 
            "37:doForceWidth = on", 
            "37:mWidth = 100.000000", 
            "24:onMode = off", 
            "24:onIfAny = 1 2 3 4 5", 
            "Higgs:useBSM = on", 
            "HiggsBSM:ffbar2H+- = on"),
        parameterSets = cms.vstring(
            "pythia8CommonSettings", 
            "pythia8CUEP8M1Settings", 
            "processParameters")
    )
)
