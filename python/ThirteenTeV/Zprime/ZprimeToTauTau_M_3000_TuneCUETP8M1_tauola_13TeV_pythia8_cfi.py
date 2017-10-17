import FWCore.ParameterSet.Config as cms

from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import * 

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         comEnergy = cms.double(13000.0),
                         crossSection = cms.untracked.double(0.001291),
                         filterEfficiency = cms.untracked.double(1),
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         ExternalDecays = cms.PSet(Tauola = cms.untracked.PSet(TauolaPolar, TauolaDefaultInputCards ),
                                                   parameterSets = cms.vstring('Tauola')
                                                   ),
                         PythiaParameters = cms.PSet(        
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'NewGaugeBoson:ffbar2gmZZprime = on',
            'Zprime:gmZmode = 3', # only pure Z' contribution
            '32:m0 = 3000', # mass of Z'
            '32:onMode = off', # switch off all of the Z' decay
            '32:onIfAny = 15', # switch on the Z'->tau-tau+
            ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
            'processParameters')       
        )
                         )
#ProductionFilterSequence = cms.Sequence(generator)
