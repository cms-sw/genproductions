import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/RunIII/5p36TeV/Powheg/el8_amd64_gcc12/WZ_el8_amd64_gcc12_CMSSW_14_1_7_Run3_pp_5p36TeV_WZTo2L2Q.tgz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh'),
    generateConcurrently = cms.untracked.bool(True)
)

#Link to datacards:
#https://github.com/cms-sw/genproductions/blob/master/bin/Powheg/production/Run3/5p36TeV/WZTo2L2Q/WZTo2L2Q_NNPDF31_5p36TeV.input

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentHadronizerFilter",
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PowhegEmissionVetoSettingsBlock,
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            'POWHEG:nFinal = 2', ## Number of final state particles
            ## (BEFORE THE DECAYS) in the LHE
            ## other than emitted extra parton
            'TimeShower:mMaxGamma = 4.0'
        ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CP5Settings',
            'pythia8PowhegEmissionVetoSettings',
            'processParameters',
            'pythia8PSweightsSettings',
        )
    ),
    comEnergy = cms.double(5360.),
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1),
)

ProductionFilterSequence = cms.Sequence(generator)
