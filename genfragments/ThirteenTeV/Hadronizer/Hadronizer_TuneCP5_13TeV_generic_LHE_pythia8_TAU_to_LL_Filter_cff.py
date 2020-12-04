import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *


externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('GRIDPACK'),
    nEvents = cms.untracked.uint32(10000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'pythia8PSweightsSettings',
                                    )
    )
)


cctauFilter = cms.EDFilter("PythiaDauVFilter",
    ChargeConjugation = cms.untracked.bool(True),
    DaughterIDs = cms.untracked.vint32(11, 13),
    MaxEta = cms.untracked.vdouble(999.0, 999.0),
    MinEta = cms.untracked.vdouble(-999.0, -999.0),
    MinPt = cms.untracked.vdouble(0.0, 0.0),
    MotherID = cms.untracked.int32(24),
    NumberDaughters = cms.untracked.int32(0),
    ParticleID = cms.untracked.int32(15),
    verbose = cms.untracked.int32(0)
)



ProductionFilterSequence = cms.Sequence(generator+~cctauFilter)
