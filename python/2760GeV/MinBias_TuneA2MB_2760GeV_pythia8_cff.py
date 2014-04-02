# Pythia8 fragment for minbias with tune A2MB-MSTW2008LO

import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
   crossSection = cms.untracked.double(62040000000),     
   maxEventsToPrint = cms.untracked.int32(0),
   pythiaPylistVerbosity = cms.untracked.int32(1),
   filterEfficiency = cms.untracked.double(1),
   pythiaHepMCVerbosity = cms.untracked.bool(False),
   comEnergy = cms.double(2760.0),
   PythiaParameters = cms.PSet(
       processParameters = cms.vstring(
	   'Main:timesAllowErrors    = 10000', 
           'ParticleDecays:limitTau0 = on',
	   'ParticleDecays:tauMax = 10',
           'SoftQCD:minBias = on',           
           'SoftQCD:singleDiffractive = on', 
           'SoftQCD:doubleDiffractive = on', 
           'Tune:pp 8',                      
           'Tune:ee 3'),
       parameterSets = cms.vstring('processParameters')
   )
)

ProductionFilterSequence = cms.Sequence(generator)
