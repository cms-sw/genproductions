import FWCore.ParameterSet.Config as cms

source = cms.Source("LHESource",
    fileNames = cms.untracked.vstring('file:test.lhe')
)

from Configuration.GenProduction.PythiaUESettings_cfi import *

#generator = cms.EDFilter("Pythia8GeneratorFilter",
generator = cms.EDFilter("Pythia8HadronizerFilter",
    crossSection = cms.untracked.double(55000000000.),
    maxEventsToPrint = cms.untracked.int32(3),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(10000.0),

    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
	    'Main:timesAllowErrors    = 10000', 
            'ParticleDecays:limitTau0 = on',
	    'ParticleDecays:tauMax = 10',
            'MultipleInteractions:pTmin = 1.8387',
            'SigmaProcess:alphaSorder = 1',
            'SigmaProcess:Kfactor = 1',
	    'PDF:useLHAPDF = on',
	    'PDF:LHAPDFset = cteq6ll.LHpdf',
	    'Beams:allowVertexSpread = on',
	    #'PhaseSpace:pTHatMin      = 20.'
	    #'Top:all = on',
	    #'Top:gg2ttbar = on',
	    #'Top:qqbar2ttbar = on',
	    #'WeakSingleBoson:all = off',
	    #'WeakSingleBoson:ffbar2W = on',
	    #'24:0:bRatio = 0',		
	    #'24:1:bRatio = 0',		
	    #'24:2:bRatio = 0',		
	    #'24:3:bRatio = 0',		
	    #'24:4:bRatio = 0',		
	    #'24:5:bRatio = 0',
	    #'24:6:bRatio = 0.333',		
	    #'24:7:bRatio = 0.333',		
	    #'24:8:bRatio = 0.334',
	    #'SecondHard:generate = on',	    
            #'SecondHard:TwoJets = on'
					),
        parameterSets = cms.vstring('processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    name = cms.untracked.string ('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCDEnrichedpt_120_170_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('TestLHE')
)

ProductionFilterSequence = cms.Sequence(generator)
