import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         emissionVeto1 = cms.untracked.PSet(),
                         EV1_nFinal = cms.int32(2),
                         EV1_nFinalMode = cms.int32(2),
                         EV1_vetoOn = cms.bool(True),
                         EV1_maxVetoCount = cms.int32(10),
                         EV1_pThardMode = cms.int32(1),
                         EV1_pTempMode = cms.int32(0),
                         EV1_emittedMode = cms.int32(0),
                         EV1_pTdefMode = cms.int32(1),
                         EV1_MPIvetoOn = cms.bool(False),
                         EV1_QEDvetoMode = cms.int32(1),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    )
        )
                         )

ProductionFilterSequence = cms.Sequence(generator)
