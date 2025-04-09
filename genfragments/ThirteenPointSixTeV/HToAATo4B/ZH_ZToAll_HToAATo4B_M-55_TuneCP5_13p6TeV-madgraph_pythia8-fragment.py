import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    nEvents = cms.untracked.uint32(5000),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh'),
    numberOfParameters = cms.uint32(4),
    args = cms.vstring(
        '/afs/cern.ch/user/a/ataxeidi/public/gen_run3/gridpacks/ZH_ZToAll_HToAATo4B/ZH_ZToAll_HToAATo4B_M-55_el8_amd64_gcc10_CMSSW_12_4_8_tarball.tar.xz',
        'false',
        'el8_amd64_gcc10',
        'CMSSW_12_4_8'
    )
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13600.0),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CP5Settings'
        )
    )
)

ProductionFilterSequence = cms.Sequence(generator)
