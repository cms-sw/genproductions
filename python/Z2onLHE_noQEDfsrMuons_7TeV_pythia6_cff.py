import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0          ! User defined processes',
                                        'PMAS(5,1)=4.4   ! b quark mass',
                                        'PMAS(6,1)=172.4 ! t quark mass',
                                        'MSTJ(39)=13     ! switch off QED fsr off muons'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
                                    'processParameters'
                                    )
        )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string ('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Z2onLHE_7TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('runs Z2 Pythia6, without QED fsr off muons')
)


