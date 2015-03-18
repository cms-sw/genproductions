import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    maxeventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(0.5),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    SLHAFileForPythia8 = cms.string('Configuration/Generator/data/LQ_cmusnumu_beta0.5.out'),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'Main:timesAllowErrors = 10000',
            'Tune:ee 3',
            'Tune:pp 5',
            'LeptoQuark:gg2LQLQbar = on',
            'LeptoQuark:qqbar2LQLQbar = on',
            '42:m0 = 700 ! LQ mass',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters',
                                    )
   )
)

enuejjFilter = cms.EDFilter("LQGenFilter",
    src        = cms.untracked.InputTag("generator"),
    eejj       = cms.bool(False),
    enuejj     = cms.bool(False),
    nuenuejj   = cms.bool(False),
    mumujj     = cms.bool(False),
    munumujj   = cms.bool(True),
    numunumujj = cms.bool(False)
)   

ProductionFilterSequence = cms.Sequence(generator*enuejjFilter)
