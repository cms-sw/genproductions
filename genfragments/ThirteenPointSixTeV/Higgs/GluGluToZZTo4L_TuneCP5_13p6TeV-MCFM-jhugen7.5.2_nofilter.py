import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('Name'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    generateConcurrently = cms.untracked.bool(True),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh'),
)

# Link to cards:
# https://raw.githubusercontent.com/cms-sw/genproductions/a8ea4bc76df07ee2fa16bd9a67b72e7b648dec64/bin/MCFM/cards/MCFM+JHUGen/MCFM_JHUGen_13TeV_ggZZtoELMU_BKG_NNPDF31.DAT
# https://raw.githubusercontent.com/cms-sw/genproductions/a8ea4bc76df07ee2fa16bd9a67b72e7b648dec64/bin/MCFM/ACmdataConfig.py
#    --coupling 0PM --bsisigbkg BKG

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentHadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13600.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            'SpaceShower:pTdampMatch = 1',
            'SpaceShower:pTdampFudge = 0.85',
            'SpaceShower:MEcorrections = off',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'pythia8PSweightsSettings',
                                    'processParameters',
                                    )
    )
)

ProductionFilterSequence = cms.Sequence(generator)
