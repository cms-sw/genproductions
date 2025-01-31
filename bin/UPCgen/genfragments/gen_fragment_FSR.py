import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring(PATH_TO_TARBALL),
    nEvents = cms.untracked.uint32(1),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        processParameters = cms.vstring(
            # For purely elastic production
            'PartonLevel:ISR = off',
            'PartonLevel:MPI = off',
            'PartonLevel:Remnants = off',
            'PartonLevel:FSR = on',
            'Check:abortIfVeto = on',
            'Check:event = off',
            #'Check:particleData = on', #check particle data
            'LesHouches:matchInOut = off',
            'BeamRemnants:primordialKT = off',
            'BeamRemnants:unresolvedHadron = 3',
            'SpaceShower:dipoleRecoil = on',
            'SpaceShower:pTmaxMatch = 2',
            'SpaceShower:QEDshowerByQ = off',
            'SpaceShower:pTdampMatch = 1',
            'Beams:frameType = 4',
            #'15:mayDecay = false', #disable tau decays
            'Next:numberShowLHA = 0',
            'Next:numberShowInfo = 0'
        ),
        parameterSets = cms.vstring('pythia8CommonSettings', 'processParameters')
    ),
    comEnergy = cms.double(BEAM_ENERGY),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1)
)

ProductionFilterSequence = cms.Sequence(generator)
