
import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    crossSection= cms.untracked.double(25310000),
    comEnergy = cms.double(900.0),
    PythiaParameters = cms.PSet(
      pythiaUESettingsBlock,
      processParameters = cms.vstring(
        'MSEL=10',
        'CKIN(3)=-1  ! minimum pt hat for hard interactions',
        'CKIN(4)=-1  ! maximum pt hat for hard interactions'),
        # This is a vector of ParameterSet names to be read, in this order

         parameterSets = cms.vstring('pythiaUESettings', 
                                    'processParameters')
     
    )
)

ConfigurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string
('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PhotonJet_900GeV_cff.py,v $'),
    annotation = cms.untracked.string('PhotonJet at 900GeV')
)

ProductionFilterSequence = cms.Sequence(generator)


