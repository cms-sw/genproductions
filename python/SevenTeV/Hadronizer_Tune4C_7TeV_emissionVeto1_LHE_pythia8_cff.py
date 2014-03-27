import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8175HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.),
    EV_CheckHard = cms.bool(True),    
    emissionVeto1 = cms.untracked.PSet(),
    EV1_nFinal = cms.int32(1),
    EV1_vetoOn = cms.bool(True),
    EV1_maxVetoCount = cms.int32(10000),
    EV1_pThardMode = cms.int32(1),
    EV1_pTempMode = cms.int32(0),
    EV1_emittedMode = cms.int32(0),
    EV1_pTdefMode = cms.int32(1),
    EV1_MPIvetoOn = cms.bool(False),    
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
	    'Main:timesAllowErrors    = 10000', 
            'ParticleDecays:limitTau0 = on',
	    'ParticleDecays:tauMax = 10',
            'Tune:ee 3',
            'Tune:pp 5'
            'SpaceShower:pTmaxMatch = 2',
            'TimeShower:pTmaxMatch  = 2',
            "TimeShower:pTminChgL = 1e-10", # BARZE to lower Pythia EWK cutoff
            "SpaceShower:pTminChgL = 1e-10", # BARZE to lower Pythia EWK cutoff
        ),
        parameterSets = cms.vstring('processParameters')
    )
)
