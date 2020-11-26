import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
    crossSection = cms.untracked.double(237.3),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
	     'Tune:pp 5',

             'WeakSingleBoson:ffbar2W = on ' 
             'Main:timesAllowErrors    = 10000', 
            
             'ParticleDecays:limitTau0 = on',
             'ParticleDecays:tauMax = 10',
 
             'SecondHard:generate = on',
             'SecondHard:TwoJets = on',
             'PhaseSpace:pTHatMinSecond = 15',
             'PhaseSpace:sameForSecond = off', 

             '24:onMode = off',  #turn off all W decays
             '24:onIfAny = 13',   # turn ON W->munu
	    ),
        parameterSets = cms.vstring('processParameters')
    )
)


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/WtoMuNu_MPIenrichedPt15_7TeV-pythia8_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA8 WToMuNu 7TeV MPI enriched')
)

ProductionFilterSequence = cms.Sequence(generator)
