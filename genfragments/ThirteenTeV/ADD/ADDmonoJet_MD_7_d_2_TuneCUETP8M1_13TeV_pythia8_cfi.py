import FWCore.ParameterSet.Config as cms


from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.0),
    maxEventsToPrint = cms.untracked.int32(1),
    PythiaParameters = cms.PSet(
        pythia8CommonSettings = cms.vstring('Tune:preferLHAPDF = 2', 
            'Main:timesAllowErrors = 10000', 
            'Check:epTolErr = 0.01', 
            'Beams:setProductionScalesFromLHEF = off', 
            'SLHA:keepSM = on', 
            'SLHA:minMassSM = 1000.', 
            'ParticleDecays:limitTau0 = on', 
            'ParticleDecays:tau0Max = 10', 
            'ParticleDecays:allowPhotonRadiation = on'),
        pythia8CUEP8M1Settings = cms.vstring('Tune:pp 14', 
            'Tune:ee 7', 
            'MultipartonInteractions:pT0Ref=2.4024', 
            'MultipartonInteractions:ecmPow=0.25208', 
            'MultipartonInteractions:expPow=1.6'),
        pythia8_unparticle = cms.vstring('ExtraDimensionsLED:monojet = on', 
            'ExtraDimensionsLED:CutOffmode = 1', 
            'ExtraDimensionsLED:t = 0.5', 
            'ExtraDimensionsLED:n = 2', 
            'ExtraDimensionsLED:MD = 7000.', 
            '5000039:m0 = 1200.', 
            '5000039:mWidth = 1000.', 
            '5000039:mMin = 1.', 
            '5000039:mMax = 13990.', 
            'PhaseSpace:pTHatMin = 80.', 
            'PartonLevel:ISR = on', 
            'PartonLevel:FSR = on', 
            'ParticleDecays:limitTau0 = on', 
            'ParticleDecays:tauMax = 10'),
        parameterSets = cms.vstring('pythia8CommonSettings', 
            'pythia8CUEP8M1Settings', 
            'pythia8_unparticle')
    )
)
