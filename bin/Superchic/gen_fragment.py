import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring(),
    nEvents = cms.untracked.uint32(1),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        exclusive_process = cms.vstring(
            # Recomendation from SuperChic v5.0
            'PartonLevel:ISR = off',
            'PartonLevel:MPI = off',
            'PartonLevel:Remnants = off',
            'Check:event = off',
            'LesHouches:matchInOut = off',
            'SpaceShower:pTmaxMatch = 2',
            'SpaceShower:pTdampMatch = 1',
            'BeamRemnants:primordialKT = off',
            'BeamRemnants:unresolvedHadron = 3'
        ),
        parameterSets = cms.vstring('pythia8CommonSettings','pythia8CP5Settings','pythia8PSweightsSettings','exclusive_process')
    ),
    comEnergy = cms.double(5360.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1)
)

ProductionFilterSequence = cms.Sequence(generator)
