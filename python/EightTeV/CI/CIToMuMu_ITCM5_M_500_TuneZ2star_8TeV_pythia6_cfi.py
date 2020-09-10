
import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")
from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.873),
    comEnergy = cms.double(8000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(

                  'MSEL         = 0      !User defined process', 
   	          'MSUB(165)    = 1      !CI+g*/Z->mumu', 
	          'MSTP(32)     = 4      !forcing a 2->2 process to 2->1 process',




         	  'ITCM(5)      = 0      !LL, all upper quarks composite',
	  	  'KFPR(165,1)  = 13     !mu+ mu- final state',
           	  'CKIN(1)      = 500  !Minimum sqrt (s_hat) value'),
 #          	  'CKIN(2)      = 500  !Maximum sqrt (s_hat) value'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(40, 40),
    MaxEta = cms.untracked.vdouble(2.6, 2.6),
    MinEta = cms.untracked.vdouble(-2.6, -2.6),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)

ProductionFilterSequence = cms.Sequence(generator*mumugenfilter)
