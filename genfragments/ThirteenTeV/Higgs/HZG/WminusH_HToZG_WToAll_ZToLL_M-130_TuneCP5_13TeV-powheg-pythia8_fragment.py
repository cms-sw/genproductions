import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc6_amd64_gcc630/13TeV/Powheg/V2/HWminusJ_HanythingJ_NNPDF31_13TeV_M130/v1/HWminusJ_HanythingJ_NNPDF31_13TeV_M130.tgz'),
    nEvents = cms.untracked.uint32(1000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    generateConcurrently = cms.untracked.bool(True),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *


generator = cms.EDFilter("Pythia8ConcurrentHadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
            pythia8CommonSettingsBlock,
            pythia8CP5SettingsBlock,
            pythia8PSweightsSettingsBlock,
            pythia8PowhegEmissionVetoSettingsBlock,
            processParameters = cms.vstring(
                'POWHEG:nFinal = 3', ## Number of final state particles
                                     ## (BEFORE THE DECAYS) in the LHE
                                     ## other than emitted extra parton
                '25:m0 = 130.0',
                '25:onMode = off',
                '25:onIfMatch = 23 22',
		'23:mMin = 50.0',
		'23:onMode = off',
 		'23:onIfAny = 11 13 15'
                ),
            parameterSets = cms.vstring('pythia8CommonSettings',
					'pythia8CP5Settings',
                                        'pythia8PowhegEmissionVetoSettings',
                                        'pythia8PSweightsSettings',
                                        'processParameters'
                                        )
               )
                          )
ProductionFilterSequence = cms.Sequence(generator)
