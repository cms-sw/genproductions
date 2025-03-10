import FWCore.ParameterSet.Config as cms

# link to cards

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/RunIII/13p6TeV/el9_amd64_gcc11/MadGraph5_aMCatNLO/Hmesongamma/ZtoRhoGamma_01j_el9_amd64_gcc11_CMSSW_13_2_9_tarball.tar.xz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         comEnergy = cms.double(13600.0),
                         crossSection = cms.untracked.double(1),
                         filterEfficiency = cms.untracked.double(1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         maxEventsToPrint = cms.untracked.int32(5),
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         PythiaParameters = cms.PSet(
                             pythia8CommonSettingsBlock,
                             pythia8CP5SettingsBlock,
                             pythia8PSweightsSettingsBlock,
                             processParameters = cms.vstring( 
                                 'JetMatching:setMad = off',
                                 'JetMatching:scheme = 1',
                                 'JetMatching:merge = on',
                                 'JetMatching:jetAlgorithm = 2',
                                 'JetMatching:etaJetMax = 5.',
                                 'JetMatching:coneRadius = 1.',
                                 'JetMatching:slowJetPower = 1',
                                 'JetMatching:qCut = 19.', #this is the actual merging scale
                                 'JetMatching:nQmatch = 4', #4 corresponds to 4-flavour scheme (no matching of b-quarks), 5 for 5-flavour scheme
                                 'JetMatching:nJetMax = 1', #number of partons in born matrix element for highest multiplicity
                                 'JetMatching:doShowerKt = off', #off for MLM matching, turn on for shower-kT matching
                                 'TimeShower:mMaxGamma = 4.0',
                                 'BeamRemnants:primordialKThard = 2.48'
                             ),
                             parameterSets = cms.vstring(
                                 'pythia8CommonSettings',
                                 'pythia8CP5Settings',
                                 'pythia8PSweightsSettings',
                                 'processParameters',
                             )
                         )
)

ProductionFilterSequence = cms.Sequence(generator)



