import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *


generator = cms.EDFilter("Pythia8GeneratorFilter",
   pythiaPylistVerbosity = cms.untracked.int32(0),
   filterEfficiency = cms.untracked.double(1.0),
   pythiaHepMCVerbosity = cms.untracked.bool(False),
   comEnergy = cms.double(13000.0),
   crossSection = cms.untracked.double(-1.0),
   maxEventsToPrint = cms.untracked.int32(0),
   crossSectionNLO = cms.untracked.double(-1.0),
   PythiaParameters = cms.PSet(
       processParameters = cms.vstring('Main:timesAllowErrors    = 10000', 
           'ParticleDecays:limitTau0 = on', 
           'ParticleDecays:tau0Max   = 10.', 
           'PDF:pSet = 13', 
           'Beams:allowVertexSpread = on', 
           'PhaseSpace:pTHatMin      = 0.', 
           'WeakSingleBoson:all = off', 
           'WeakSingleBoson:ffbar2W = on', 
           '24:onMode = off',
           '24:onIfAny = 11 -12',
           '24:onIfAny = 13 -14',
           '24:onIfAny = 15 -16',
           'WeakBosonAndParton:qqbar2Wg = on', 
           'WeakBosonAndParton:qg2Wq = on', 
           'SecondHard:generate = on', 
           'SecondHard:SingleW = on', 
           'SecondHard:WAndJet = on'),
       parameterSets = cms.vstring('processParameters')
   )
)
ProductionFilterSequence = cms.Sequence(generator)
