import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
            pythiaHepMCVerbosity = cms.untracked.bool(False),
            maxEventsToPrint = cms.untracked.int32(0),
            pythiaPylistVerbosity = cms.untracked.int32(0),
            filterEfficiency = cms.untracked.double(1.),
            comEnergy = cms.double(7000.0),
            PythiaParameters = cms.PSet( pythiaUESettingsBlock,
                           processParameters = cms.vstring(
                                    'MSEL        = 0    !User defined processes', 
                                    'PMAS(5,1)=4.4   ! b quark mass',
                                    'PMAS(6,1)=172.5 ! t quark mass'),
                                    # This is a vector of ParameterSet names to be read, in this order
                                    parameterSets = cms.vstring('pythiaUESettings', 
                                                                'processParameters')
                                         )
)
