import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/afs/cern.ch/user/p/ptiwari/public/DMSimp_bbDM_2017_GridPacks/bbDM_DMSimp_pseudo_NLO_MPhi_10_MChi_1/bbDM_DMSimp_pseudo_NLO_MPhi_10_MChi_1_slc6_amd64_gcc630_CMSSW_9_3_8_tarball.tar.xz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

# Link to cards:
# https://github.com/cms-sw/genproductions/tree/master/bin/MadGraph5_aMCatNLO/cards/production/2017/13TeV/bbDM_DMsimp

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8aMCatNLOSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8aMCatNLOSettingsBlock,
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'pythia8aMCatNLOSettings',
                                    )
    )
)
