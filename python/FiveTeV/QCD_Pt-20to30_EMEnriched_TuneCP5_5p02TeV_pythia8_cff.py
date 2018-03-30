
import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
generator = cms.EDFilter("Pythia8GeneratorFilter",
maxEventsToPrint = cms.untracked.int32(1),
pythiaPylistVerbosity = cms.untracked.int32(1),
filterEfficiency = cms.untracked.double(1.0),
pythiaHepMCVerbosity = cms.untracked.bool(False),
comEnergy = cms.double(5020.0),
crossSection = cms.untracked.double(5.576e+08),
PythiaParameters = cms.PSet(
pythia8CommonSettingsBlock,
pythia8CP5SettingsBlock,
processParameters = cms.vstring(
'HardQCD:all = on',
'PhaseSpace:pTHatMin = 20 ',
'PhaseSpace:pTHatMax = 30 ',
),
parameterSets = cms.vstring('pythia8CommonSettings',
'pythia8CP5Settings',
'processParameters',
)
)
)
genParticlesForFilter = cms.EDProducer("GenParticleProducer",
saveBarCodes = cms.untracked.bool(True),
src = cms.InputTag("generator","unsmeared"),
abortOnUnknownPDGCode = cms.untracked.bool(False)
)
bctoefilter = cms.EDFilter("BCToEFilter",
filterAlgoPSet = cms.PSet(eTThreshold = cms.double(10),
genParSource = cms.InputTag("genParticlesForFilter")
)
)
emenrichingfilter = cms.EDFilter("EMEnrichingFilter",
filterAlgoPSet = cms.PSet(isoGenParETMin=cms.double(20.),
isoGenParConeSize=cms.double(0.1),
clusterThreshold=cms.double(20.),
isoConeSize=cms.double(0.2),
hOverEMax=cms.double(0.5),
tkIsoMax=cms.double(5.),
caloIsoMax=cms.double(10.),
requireTrackMatch=cms.bool(False),
genParSource = cms.InputTag("genParticlesForFilter")
)
)
configurationMetadata = cms.untracked.PSet(
version = cms.untracked.string('\$Revision$'),
name = cms.untracked.string('\$Source$'),
annotation = cms.untracked.string('QCD pthat 20to30 GeV, 5.02 TeV, TuneCP5')
)
# add your filters to this sequence
ProductionFilterSequence = cms.Sequence(generator * (genParticlesForFilter + ~bctoefilter + emenrichingfilter))