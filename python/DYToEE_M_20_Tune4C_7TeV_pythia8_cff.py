import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
   crossSection = cms.untracked.double(1300),
   maxEventsToPrint = cms.untracked.int32(0),
   pythiaPylistVerbosity = cms.untracked.int32(1),
   filterEfficiency = cms.untracked.double(1.0),
   pythiaHepMCVerbosity = cms.untracked.bool(False),
   comEnergy = cms.double(7000.0),
   PythiaParameters = cms.PSet(
       processParameters = cms.vstring(

           'Main:timesAllowErrors = 10000',
           'WeakSingleBoson:ffbar2gmZ = on', # turn on Z production
           'ParticleDecays:limitTau0 = on',
           'ParticleDecays:tau0Max = 10.',

           'PartonLevel:MI = on',
           'SecondHard:generate = off',
           'SecondHard:TwoJets = off',
           'PhaseSpace:sameForSecond = off', 
	   
           'PhaseSpace:mHatMin = 20',
           '23:onMode = off',  #turn off all Z decays
           '23:onIfAny = 11',   # turn ON Z->ee

           'Tune:pp 5'
       ),
       parameterSets = cms.vstring('processParameters')
   )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/DYToEE_M_20_Tune4C_7TeV_pythia8_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA8 Z/gamma* to ee, M(e+e-) > 20 GeV at sqrt(s) = 7TeV, Tune 4c')
)
