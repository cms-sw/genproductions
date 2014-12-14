import FWCore.ParameterSet.Config as cms
from Configuration.Generator.PythiaUESettings_cfi import *
#from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
pythiaHepMCVerbosity = cms.untracked.bool(False),
maxEventsToPrint = cms.untracked.int32(3),
pythiaPylistVerbosity = cms.untracked.int32(1),
displayPythiaCards = cms.untracked.bool(False),
comEnergy = cms.double(8000.0),
PythiaParameters = cms.PSet(
pythiaUESettingsBlock,
pythiaEtab = cms.vstring(
####Added by Maksat#########
'MSEL=0',
'MSUB(471)=1',
'MDME(1521,1)=0',
'KFDP(1520,1)=553',
'KFDP(1520,2)=13',
'KFDP(1520,3)=-13',
'PMAS(C10551,1)=50.0',
'PMAS(C10551,2)=0.0',
'BRAT(1034)=0 ! decay forbidden',
'BRAT(1035)=1 ! decay dimuon',
'BRAT(1036)=0 ! decay forbidden',
'BRAT(1037)=0 ! decay forbidden',
'BRAT(1038)=0 ! decay forbidden',
'BRAT(1039)=0 ! decay forbidden',
'BRAT(1040)=0 ! decay forbidden',
'BRAT(1041)=0 ! decay forbidden',
'BRAT(1042)=0 ! decay forbidden',
'MDME(1034,1)=0 ! ',
'MDME(1035,1)=1 ! ',
'MDME(1036,1)=0 ! ',
'MDME(1037,1)=0 ! ',
'MDME(1038,1)=0 ! ',
'MDME(1039,1)=0 ! ',
'MDME(1040,1)=0 ! ',
'MDME(1041,1)=0 ! ',
'MDME(1042,1)=0 ! '),
# This is a vector of ParameterSet names to be read, in this order
parameterSets = cms.vstring(
'pythiaUESettings',
'pythiaEtab')
)
)
etafilter = cms.EDFilter("PythiaFilter",
MaxEta = cms.untracked.double(9999.0),
MinEta = cms.untracked.double(-9999.0),
#ParticleID = cms.untracked.int32(10551)
ParticleID = cms.untracked.int32(10551)
)
oniafilter2 = cms.EDFilter("PythiaFilter",
Status = cms.untracked.int32(1),
MotherID = cms.untracked.int32(553),
MinPt = cms.untracked.double(0.0),
ParticleID = cms.untracked.int32(13)
)
mumugenfilter = cms.EDFilter("MCParticlePairFilter",
Status = cms.untracked.vint32(1, 1),
MaxEta = cms.untracked.vdouble(2.6, 2.6),
MinEta = cms.untracked.vdouble(-2.6, -2.6),
MinP = cms.untracked.vdouble(2.5, 2.5),
ParticleID1 = cms.untracked.vint32(13),
ParticleID2 = cms.untracked.vint32(-13)
)
######################################
#process.ProductionFilterSequence = cms.Sequence(process.generator+process.oniafilter+process.oniafilter2+process.mumugenfilter)
ProductionFilterSequence = cms.Sequence(generator*etafilter*oniafilter2*mumugenfilter)
