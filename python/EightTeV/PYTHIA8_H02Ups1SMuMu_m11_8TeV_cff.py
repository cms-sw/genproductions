import FWCore.ParameterSet.Config as cms
from Configuration.Generator.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia8GeneratorFilter",
pythiaHepMCVerbosity = cms.untracked.bool(False),
maxEventsToPrint = cms.untracked.int32(1),
pythiaPylistVerbosity = cms.untracked.int32(1),
displayPythiaCards = cms.untracked.bool(False),
comEnergy = cms.double(8000.0),
PythiaParameters = cms.PSet(
pythiaUESettingsBlock,
pythiaEtab = cms.vstring(
'Higgs:useBSM = on',
'HiggsBSM:gg2H2 = on',
'HiggsH2:coup2d = 10.0',
'HiggsH2:coup2u = 10.0',
'HiggsH2:coup2Z = 0.0',
'HiggsH2:coup2W = 0.0',
'HiggsA3:coup2H2Z = 0.0',
'HiggsH2:coup2A3A3 = 0.0',
'HiggsH2:coup2H1H1 = 0.0',
'443:onMode = off',
'443:onIfMatch 13 13',
'333:onMode = off',
'333:onIfMatch 13 13',
'553:onMode = off',
'553:onIfMatch 13 13',
############# For Fixed Mass Distribution#############
'35:mMin = 0',
'35:mMax = 25',
'35:m0 = 11.0',
'35:mWidth = 0.00',
'35:addChannel 1 1.00 100 13 -13 553',
'35:onMode = off',
'35:onIfMatch 13 -13 553'), ## Y(1S) mumu
parameterSets = cms.vstring(
'pythiaEtab')
)
)
etafilter = cms.EDFilter("PythiaFilter",
MaxEta = cms.untracked.double(9999.0),
MinEta = cms.untracked.double(-9999.0),
# ParticleID = cms.untracked.int32(10551)
ParticleID = cms.untracked.int32(35)
#ParticleID = cms.untracked.int32(551)
)
jpsifilter = cms.EDFilter("PythiaDauVFilter",
MotherID = cms.untracked.int32(10551),
verbose = cms.untracked.int32(0),
ParticleID = cms.untracked.int32(443),
MaxEta = cms.untracked.vdouble(2.6, 2.6),
MinEta = cms.untracked.vdouble(-2.6, -2.6),
DaughterIDs = cms.untracked.vint32(13, -13),
MinPt = cms.untracked.vdouble(1.8, 1.8),
# MinP = cms.untracked.vdouble(2.5, 2.5),
NumberDaughters = cms.untracked.int32(2)
)
ProductionFilterSequence = cms.Sequence(generator*etafilter)
