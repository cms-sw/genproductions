import FWCore.ParameterSet.Config as cms
from Configuration.GenProduction.PythiaUESettings_cfi import *
from GeneratorInterface.Pythia6Interface.TauolaSettings_cff import *
#################################################
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(580.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
                               "MSEL=0                  ! user selection for process",
                               "MSUB(114) = 1          ! g + g -> gamma + gamma",
                               "CKIN(3)=10             ! pthat min for 2->2 interaction",
                               "CKIN(4)=25             ! pthat min for 2->2 interaction"
                               ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_TwoPhotonBox_Pt_25_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 SM Twophoton box Pthat > 25 GeV CM energy 10TeV')
)
