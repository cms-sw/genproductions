import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
    ##crossSection = cms.untracked.double(),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
	    'Main:timesAllowErrors    = 10000', 
            
	    'ParticleDecays:limitTau0 = on',
	    'ParticleDecays:tauMax = 10',
            
	    'Tune:pp 5',
	    
	    'PromptPhoton:gg2ggamma = on',
	    'PromptPhoton:qg2qgamma = on',
	    'PromptPhoton:qqbar2ggamma = on',

	    'PhaseSpace:pTHatMin = 50.',
            'PhaseSpace:sameForSecond = off',
            'PhaseSpace:pTHatMinSecond = 20',
	    
	    'SecondHard:generate = on',
            'SecondHard:TwoJets = on'
	    
	    ),
        parameterSets = cms.vstring('processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/G_Pt50_Tune4C_MPIenriched_7TeV_pythia8_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA8 Tune4C at 7TeV- Prompt photon production, pThat >= 50 GeV, second hard interaction forced, pThat >=20GeV')
)


ProductionFilterSequence = cms.Sequence(generator)
