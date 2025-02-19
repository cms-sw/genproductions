import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/RunIII/5p36TeV/superchic/superchic_dielectron_el8_amd64_gcc11_CMSSW_13_0_18_HeavyIon_tarball.tgz'),
    nEvents = cms.untracked.uint32(1),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

#Link to datacards:
#https://github.com/cms-sw/genproductions/blob/master/bin/Superchic/production/PbPb_5p36TeV/superchic_dielectron.dat

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
    comEnergy = cms.double(5362.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1)
)

elelgenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(0.5, 0.5),
    MinP = cms.untracked.vdouble(0., 0.),
    MaxEta = cms.untracked.vdouble(3.0, 3.0),
    MinEta = cms.untracked.vdouble(-3.0, -3.0),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(11),
    ParticleID2 = cms.untracked.vint32(11)
)

ProductionFilterSequence = cms.Sequence(generator*elelgenfilter)
