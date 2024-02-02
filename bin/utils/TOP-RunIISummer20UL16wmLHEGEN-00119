import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/UL/13TeV/madgraph/V5_2.6.5/st_tch_4f_ckm_UL_powheg_madspin/st_tch_4f_ckm_antitop_13TeV_UL_powheg_madspin_slc7_amd64_gcc700_CMSSW_10_6_19_tarball.tar.xz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)


from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
maxEventsToPrint = cms.untracked.int32(1),
pythiaPylistVerbosity = cms.untracked.int32(1),
filterEfficiency = cms.untracked.double(1.0),
pythiaHepMCVerbosity = cms.untracked.bool(False),
comEnergy = cms.double(13000.),
PythiaParameters = cms.PSet(
pythia8CommonSettingsBlock,
pythia8CP5SettingsBlock,
pythia8PowhegEmissionVetoSettingsBlock,
pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            'POWHEG:nFinal = 3',   ## Number of final state particles
          ),
parameterSets = cms.vstring('pythia8CommonSettings',
                            'pythia8PowhegEmissionVetoSettings',
							'pythia8CP5Settings',
                            'pythia8PSweightsSettings',
							'processParameters'
)
)
)
ProductionFilterSequence = cms.Sequence(generator)
