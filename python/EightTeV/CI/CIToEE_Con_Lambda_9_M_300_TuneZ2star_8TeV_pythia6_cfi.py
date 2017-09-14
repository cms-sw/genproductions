
import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")
from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.76),
    comEnergy = cms.double(8000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(

                  'MSEL         = 0      !User defined process', 
   	          'MSUB(165)    = 1      !CI+g*/Z->ee', 
	          'MSTP(32)     = 4      !forcing a 2->2 process to 2->1 process',




	          'RTCM(42)     =-1 !Constructive Interference',
	  	  'RTCM(41)     = 9000  !Lambda = 9 TeV',
 	  	 'ITCM(5)      = 2       !LL, all upper quarks composite',
                 'KFPR(165,1)  = 11      !e+ e- final state',
           	  'CKIN(1)      = 300  !Minimum sqrt (s_hat) value'),
 #          	  'CKIN(2)      = 500  !Maximum sqrt (s_hat) value'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

eegenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(30, 30),
    MaxEta = cms.untracked.vdouble(3.0, 3.0),
    MinEta = cms.untracked.vdouble(-3.0, -3.0),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(11),
    ParticleID2 = cms.untracked.vint32(11)
)

ProductionFilterSequence = cms.Sequence(generator*eegenfilter)
