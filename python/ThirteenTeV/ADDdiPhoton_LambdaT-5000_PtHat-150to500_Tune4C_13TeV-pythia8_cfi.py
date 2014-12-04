import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.0),
    crossSection = cms.untracked.double(0.170745000), # pb
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
            'Main:timesAllowErrors    = 10000', 
            'ParticleDecays:limitTau0 = on', 
            'ParticleDecays:tauMax = 10', 
            'Tune:pp 5', # Tune 4C
            'Tune:ee 3', 
            'ExtraDimensionsLED:ffbar2gammagamma = on', 
            'ExtraDimensionsLED:CutOffmode = 0',
            'ExtraDimensionsLED:LambdaT = 5000.',
            'PhaseSpace:pTHatMin = 150.',
            'PhaseSpace:pTHatMax = 500.',
            'PartonLevel:MI = on', 
            'PartonLevel:ISR = on', 
            'PartonLevel:FSR = on'
        ),
        parameterSets = cms.vstring('processParameters')
    )
)