import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP2Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP2SettingsBlock,
        processParameters = cms.vstring(
            'SLHA:useDecayTable = off',  # Use pythia8s own decay mode instead of decays defined in LH accord
            '25:m0 = 125.0', 
            '23:mMin = 0.05',                 # Solve problem with mZ cut
            '25:onMode = off',
            '25:onIfAll = 23 23',           # turn ON H->ZZ
            '23:onMode = off',                # turn OFF all Z decays
            '23:onIfAny = 11 13 15'           # turn ON Z->ll

            ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP2Settings',
                                    'processParameters'
                                    )
        )
                         )

ProductionFilterSequence = cms.Sequence(generator)
