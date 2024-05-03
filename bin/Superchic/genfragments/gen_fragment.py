import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring(PATH_TO_TARBALL),
    nEvents = cms.untracked.uint32(1),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8PSweightsSettingsBlock,
        exclusive_process = cms.vstring(
            # Recomendation from https://anstahll.web.cern.ch/anstahll/superchic/SuperChic_5_1.pdf
            # For purely elastic production (diff set to el)
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
        parameterSets = cms.vstring('pythia8CommonSettings','pythia8PSweightsSettings','exclusive_process')
    ),
    comEnergy = cms.double(BEAM_ENERGY),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1)
)

ProductionFilterSequence = cms.Sequence(generator)
