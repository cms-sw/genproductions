import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

process.generator = cms.EDFilter("Pythia8HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.0),
    maxEventsToPrint = cms.untracked.int32(1),
    PythiaParameters = cms.PSet(
        process.pythia8CommonSettingsBlock,
        process.pythia8CUEP8M1SettingsBlock,
        parameterSets = cms.vstring('pythia8CommonSettings', 
            'pythia8CUEP8M1Settings')
    )
)

process.dirhadrongenfilter = cms.EDFilter("MCMultiParticleFilter",
	src=cms.InputTag("generator"),
    NumRequired = cms.int32(3),
    AcceptMore = cms.bool(True),
	ParticleID = cms.vint32(11,13,15),
	PtMin = cms.vdouble(4.0, 4.0, 4.0),
	EtaMax = cms.vdouble(999.0, 999.0, 999.0),
	Status = cms.vint32(1,1,2)
	
)

process.ProductionFilterSequence = cms.Sequence(process.generator*process.dirhadrongenfilter)

