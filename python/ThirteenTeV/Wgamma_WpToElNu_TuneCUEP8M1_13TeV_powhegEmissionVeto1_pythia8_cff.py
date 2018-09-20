import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc6_amd64_gcc630//pre2017/13TeV/powheg/WGamma/Wgamma_slc6_amd64_gcc630_CMSSW_9_3_0_WgammaWpToElNu.tgz'),
    nEvents = cms.untracked.uint32(5000),
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
                         emissionVeto1 = cms.untracked.PSet(),
                         EV1_nFinal = cms.int32(2),
                         EV1_nFinalMode = cms.int32(2),
                         EV1_vetoOn = cms.bool(True),
                         EV1_maxVetoCount = cms.int32(10),
                         EV1_pThardMode = cms.int32(0),
                         EV1_pTempMode = cms.int32(0),
                         EV1_emittedMode = cms.int32(0),
                         EV1_pTdefMode = cms.int32(1),
                         EV1_MPIvetoOn = cms.bool(False),
                         EV1_QEDvetoMode = cms.int32(1),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings')
        )
                         )

ProductionFilterSequence = cms.Sequence(generator)
