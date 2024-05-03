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
        semiexclusive_process = cms.vstring(
            # Recomendation from https://anstahll.web.cern.ch/anstahll/superchic/SuperChic_5_1.pdf
            # For semi–exclusive photon–initiated production (diff set to sda, sdb, dd)
            'Check:event = off',
            'PDF:pSet = LHAPDF6:MSHT20qed nnlo',
            'LesHouches:matchInOut = off',
            'BeamRemnants:primordialKT = off',
            'PartonLevel:MPI = off',
            'SpaceShower:dipoleRecoil = on',
            'SpaceShower:pTmaxMatch = 2',
            'SpaceShower:QEDshowerByQ = off',
            'SpaceShower:pTdampMatch = 1',
            'BeamRemnants:unresolvedHadron = 0' #diff= dd: 0, sdb: 1, sda: 2, el: 3
        ),
        parameterSets = cms.vstring('pythia8CommonSettings','pythia8PSweightsSettings','semiexclusive_process')
    ),
    comEnergy = cms.double(BEAM_ENERGY),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1)
)

ProductionFilterSequence = cms.Sequence(generator)
