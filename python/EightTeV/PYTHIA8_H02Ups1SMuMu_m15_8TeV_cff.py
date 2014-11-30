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
# 'Bottomonium:gg2QQbar[3S1(1)]g = on', #Upsilon(1S) Process
# 'Bottomonium:gg2QQbar[3P0(1)]g = on ', #Turns on Chi_0b Process(10551)
# 'Bottomonium:qg2QQbar[3P0(1)]q = on', (3=2S+1, P=L, 0 = J(Total Angular Mom.), 1=Color)
# 'Bottomonium:qqbar2QQbar[3P0(1)]g = on',
# '10551:addChannel 1 1.00 100 443 443', #Chi_0b -> JpsiJpsi
# '10551:m0 = 8.',
# '10551:mMin = 6.0',
# '10551:mMax = 9.5',
# '10551:mWidth = 4',
# '10551:oneChannel = 1 1.00 100 443 223',
# ' Bottomonium:gg2QQbar[3P1(1)]g = on', #Turns on (chi_1b) Process (20551)
# 'Bottomonium:qg2QQbar[3P1(1)]q = on',
# 'Bottomonium:qqbar2QQbar[3P1(1)]g = on',
# 'Bottomonium:gg2QQbar[3P2(1)]g = on', #Turns on (chi_2b) Process(555)
# 'Bottomonium:qg2QQbar[3P2(1)]q = on',
# 'Bottomonium:qqbar2QQbar[3P2(1)]g = on',
# 'Bottomonium:gg2QQbar[3S1(8)]g = on',
# 'Charmonium:gg2QQbar[3P0(1)]g = on',
# 'Bottomonium:qg2QQbar[1S0(8)]q = on',
# 'Bottomonium:qqbar2QQbar[1S0(8)]g = on',
'Higgs:useBSM = on',
'HiggsBSM:gg2H2 = on',
#'HiggsBSM:ffbar2H2 = on',
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
#'223:onMode = off',
#'223:onIfMatch 13 13',
'553:onMode = off',
'553:onIfMatch 13 13',
############### For Floating Mass Distribution#######
# '35:mMin = 15',
# '35:mMax = 25',
# '35:m0 = 23. ! Higgs mass',
# '35:mWidth = 10 !Higgs Width',
############# For Fixed Mass Distribution#############
'35:mMin = 0',
'35:mMax = 25',
'35:m0 = 15',
'35:mWidth = 0.00',
# '35:addChannel 1 1.00 100 443 443',
'35:addChannel 1 1.00 100 13 -13 553',
# '35:addChannel 1 1.00 100 443 333',
# '35:addChannel 1 1.00 100 443 223',
# '35:addChannel 1 1.00 100 443 553',
# '35:addChannel 1 1.00 100 333 553',
# '35:addChannel 1 1.00 100 223 553',
'35:onMode = off',
#'35:onIfMatch 443 333'), ##Jpsi Phi decay channel!
'35:onIfMatch 13 -13 553'), ## Y(1S) mumu
# '35:onIfMatch 443 443'), ##Jpsi Jpsi decay channel!
# '35:onIfMatch 443 223'), ##Jpsi Omega decay channel!
# '35:onIfMatch 443 553'), ##Jpsi Upsilon decay channel!
# '35:onIfMatch 333 553'), ##Phi Upsilon decay channel!
# '35:onIfMatch 223 553'), ##Omega Upsilon decay channel!
# This is a vector of ParameterSet names to be read, in this order
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
