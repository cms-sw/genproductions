import FWCore.ParameterSet.Config as cms

from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
generator = cms.EDFilter("Pythia8HadronizerFilter",
                         ExternalDecays = cms.PSet(
    Tauola = cms.untracked.PSet(
    TauolaPolar,
    TauolaDefaultInputCards
    ),
    parameterSets = cms.vstring('Tauola')
    ),
    UseExternalGenerators = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
	    'Main:timesAllowErrors    = 10000', 
        'ParticleDecays:limitTau0 = on',
	    'ParticleDecays:tau0Max = 10',
        'Tune:ee 3',
        'Tune:pp 5',
        'SpaceShower:pTmaxMatch = 1',
        'SpaceShower:pTmaxFudge = 1',
        'SpaceShower:MEcorrections = off',
        'TimeShower:pTmaxMatch = 1',
        'TimeShower:pTmaxFudge = 1',
        'TimeShower:MEcorrections = off',
        'TimeShower:globalRecoil = on',
        'TimeShower:limitPTmaxGlobal = on',
        'TimeShower:nMaxGlobalRecoil = 1',
        'TimeShower:globalRecoilMode = 2',
        'TimeShower:nMaxGlobalBranch = 1',
        'SLHA:keepSM = on',
        'SLHA:minMassSM = 1000.',        
        'Check:epTolErr = 0.01',
        ),
        parameterSets = cms.vstring('pythia8PSweightsSettings',
                                    'processParameters')
    )
)
