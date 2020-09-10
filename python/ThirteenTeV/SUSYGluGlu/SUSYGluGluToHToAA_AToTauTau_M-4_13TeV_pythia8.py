import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
#    SLHATableForPythia8 = cms.string('%s' % SLHA_TABLE),				 
    comEnergy = cms.double(13000.0),
    crossSection = cms.untracked.double(518.3),
    maxEventsToPrint = cms.untracked.int32(1),
    PythiaParameters = cms.PSet(
        pythia8CommonSettings = cms.vstring('Main:timesAllowErrors = 10000', 
            'Check:epTolErr = 0.01', 
            'Beams:setProductionScalesFromLHEF = off', 
#            'SLHA:keepSM = on', 
#            'SLHA:minMassSM = 100.', 
            'ParticleDecays:limitTau0 = on', 
            'ParticleDecays:tau0Max = 10', 
            'ParticleDecays:allowPhotonRadiation = on'),
        pythia8CUEP8M1Settings = cms.vstring('Tune:pp 14', 
            'Tune:ee 7', 
            'MultipartonInteractions:pT0Ref=2.4024', 
            'MultipartonInteractions:ecmPow=0.25208', 
            'MultipartonInteractions:expPow=1.6'),
        processParameters = cms.vstring('Higgs:useBSM = on', 
            'HiggsBSM:gg2H2 = on', 
	    '35:m0 = 125.',
	    '35:onMode = off',
            '35:onIfMatch = 25 25',
	    '25:mMin = 3',
  	    '25:m0 = 4.',
	    '25:onMode = off',
	    '25:onIfMatch = 15 -15'
					),
        parameterSets = cms.vstring('pythia8CommonSettings', 
            'pythia8CUEP8M1Settings', 
            'processParameters')
    )
)

