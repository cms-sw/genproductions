import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
   crossSection = cms.untracked.double(760),
   maxEventsToPrint = cms.untracked.int32(0),
   pythiaPylistVerbosity = cms.untracked.int32(1),
   filterEfficiency = cms.untracked.double(1.0),
   pythiaHepMCVerbosity = cms.untracked.bool(False),
   comEnergy = cms.double(8000.0),
   PythiaParameters = cms.PSet(
       processParameters = cms.vstring(
           'Tune:pp 5',

           'Main:timesAllowErrors = 10000',
           'WeakSingleBoson:ffbar2gmZ = on', # turn on Z production
           'ParticleDecays:limitTau0 = on',
           'ParticleDecays:tau0Max = 10.',

           'PartonLevel:MI = on',
           'SecondHard:generate = on',
           'SecondHard:TwoJets = on',
           'PhaseSpace:sameForSecond = off',
           'PhaseSpace:pTHatMinSecond = 15',

           'PhaseSpace:mHatMin = 50',
           'PhaseSpace:mHatMax = 130',
           '23:onMode = off',  #turn off all Z decays
           '23:onIfAny = 13'  # turn ON Z->mumu

       ),
       parameterSets = cms.vstring('processParameters')
   )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/DYToMuMu_M_50To130_Tune4C_MPIenhanced_7TeV_pythia8_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA8 Z/gamma* to mumu, 50 < M(mu+mu-) < 130 GeV at sqrt(s) = 8TeV, Tune 4C MPI enhanced')
)
