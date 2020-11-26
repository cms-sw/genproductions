import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP3Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP3SettingsBlock,
        processParameters = cms.vstring(
            'SLHA:useDecayTable = off',                        # Use pythia8s own decay mode instead of decays defined in LH accord
            '25:m0 = 125.0', 
            '25:onMode = off',
            '25:onIfMatch = 24 -24',                           # turn ON H->WW
            '24:mMin = 0.05',                                  #  
            '24:onMode = off',                                 # turn OFF all W decays
            '24:onIfAny = 11 13 15 12 14 16 1 2 3 4 5',        # turn ON W->lnuq 
            'ResonanceDecayFilter:filter = on',       
            'ResonanceDecayFilter:exclusive = off',            # require at least specified daughters (on -> require exactly specified daughters)
            'ResonanceDecayFilter:eMuTauAsEquivalent = on',    # treat electrons, muons , and taus as equivalent
            'ResonanceDecayFilter:allNuAsEquivalent  = on',    # treat all three neutrino flavours as equivalent
            'ResonanceDecayFilter:udscbAsEquivalent  = on',    # treat u,d,s,c,b quarks as equivalent
            'ResonanceDecayFilter:mothers = 24',               # require mother to be W
            'ResonanceDecayFilter:daughters = 1,11',           # require one quark decay and one leptonic decay
            ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP3Settings',
                                    'processParameters'
                                    )
        )
                         )

ProductionFilterSequence = cms.Sequence(generator)
