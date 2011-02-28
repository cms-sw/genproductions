import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    comEnergy = cms.double(2760.0),
    crossSection = cms.untracked.double(5.094e-06),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'MSEL=10',
            'CKIN(3)=30  ! minimum pt hat for hard interactions'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string
('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/Attic/PhotonJet_Pt_30_TuneZ2_2760GeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('PhotonJetpt-30 at 2.36TeV')
)

ProductionFilterSequence = cms.Sequence(generator)