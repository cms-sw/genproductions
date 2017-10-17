import FWCore.ParameterSet.Config as cms

from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    comEnergy = cms.double(13000.0),
      ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
            ),
        parameterSets = cms.vstring('Tauola')
      ),
    crossSection = cms.untracked.double(1.0),
    filterEfficiency = cms.untracked.double(1),
    maxeventsToPrint = cms.untracked.int32(10),
    pyhiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    PythiaParameters = cms.PSet(
      processParameters = cms.vstring(
            
            

            'LeptoQuark:gg2LQLQbar = on',
            'LeptoQuark:qqbar2LQLQbar = on',
            '6:m0 = 172.5 ! top mass',
            '42:m0 = 650 ! LQ mass',
            '42:addChannel = 1 1.0 0 5 -15 ! add channel LQ to tau b',
            '42:0:onMode = 0 ! switch off LQ to e u',
            '42:0:bRatio = 0.0 ! switch off LQ to e u',

            ),
      parameterSets = cms.vstring('processParameters')
   )
)
