import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         maxEventsToPrint = cms.untracked.int32(0),
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         filterEfficiency = cms.untracked.double(1.0),
                         crossSection = cms.untracked.double(74.1),
                         comEnergy = cms.double(5020.0),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        processParameters = cms.vstring(
            'WeakBosonAndParton:qqbar2gmZg = on',
            'WeakBosonAndParton:qg2gmZq = on',
            '23:onMode = off',
            '23:onIfAny = 11',
            'PhaseSpace:pTHatMin = 30.',
            'PhaseSpace:bias2Selection = on',
            'PhaseSpace:bias2SelectionPow = 1.5',
            'PhaseSpace:bias2SelectionRef = 30',
            ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'processParameters',
                                    )
        )
                         )

zgenfilter = cms.EDFilter("PythiaFilter",
    MaxRapidity = cms.untracked.double(2.5),
    MinRapidity = cms.untracked.double(-2.5),
    MinPt = cms.untracked.double(30.),
    ParticleID = cms.untracked.int32(23)
)


ProductionFilterSequence = cms.Sequence(generator*zgenfilter)
