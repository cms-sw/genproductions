################
# Gen fragment #
################

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(8000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0          ! User defined processes',
                                        'PMAS(5,1)=4.8   ! b quark mass',
                                        'PMAS(6,1)=172.5 ! t quark mass'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
                                    'processParameters'
                                    )
        )
)

###########
# Filters #
###########

topfilter = cms.EDFilter("MCSingleParticleFilter",
    MaxEta = cms.untracked.vdouble(999, 999),
    Status = cms.untracked.vint32(3, 3),
    MinEta = cms.untracked.vdouble(-999, -999),
    MinPt = cms.untracked.vdouble(0,0),
    ParticleID = cms.untracked.vint32(6, -6)
)

ProductionFilterSequence = cms.Sequence(generator*~topfilter)
