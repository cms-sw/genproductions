MCHI = XXX  # GeV
CTAU = YYY  # mm
XQCUT = AAA
COM_ENERGY = 13600.

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('root://eosuser.cern.ch//eos/user/b/borzari/GenValidation/gridpack_AMSB_Run3_MG5v299_correctEvents/%scm/Higgsino_M%sGeV_ctau%scm_slc7_amd64_gcc900_CMSSW_12_0_2_tarball.tar.xz' % (int(CTAU/10), MCHI, int(CTAU/10))),
    nEvents = cms.untracked.uint32(10),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_xrootd.sh')
)

generator = cms.EDFilter("Pythia8HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(-1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(COM_ENERGY),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            '1000024:isResonance = false',
            '1000024:oneChannel = 1 0.4775 100 1000022 211',
            '1000024:addChannel = 1 0.4775 100 1000023 211',
            '1000024:addChannel = 1 0.0150 100 1000022 -11 12',
            '1000024:addChannel = 1 0.0150 100 1000023 -11 12',
            '1000024:addChannel = 1 0.0075 100 1000022 -13 14',
            '1000024:addChannel = 1 0.0075 100 1000023 -13 14',
            '1000024:tau0 = %.1f' % CTAU,
            '1000023:mayDecay = false',
            'ParticleDecays:tau0Max = %.1f' % (CTAU * 10),
            'JetMatching:setMad = off',
            'JetMatching:scheme = 1',
            'JetMatching:merge = on',
            'JetMatching:jetAlgorithm = 2',
            'JetMatching:etaJetMax = 5.',
            'JetMatching:coneRadius = 1.',
            'JetMatching:slowJetPower = 1',
            'JetMatching:qCut = %.1f' % (XQCUT * 1.5), #this is the actual merging scale
            'JetMatching:nQmatch = 4', #4 corresponds to 4-flavour scheme (no matching of b-quarks), 5 for 5-flavour scheme
            'JetMatching:nJetMax = 1', #number of partons in born matrix element for highest multiplicity
            'JetMatching:doShowerKt = off', #off for MLM matching, turn on for shower-kT matching
       ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CP5Settings',
            'pythia8PSweightsSettings',
            'processParameters')
    ),
    # The following parameters are required by Exotica_HSCP_SIM_cfi:
    slhaFile = cms.untracked.string(''),   # value not used
    processFile = cms.untracked.string('SimG4Core/CustomPhysics/data/RhadronProcessList.txt'),
    useregge = cms.bool(False),
    hscpFlavor = cms.untracked.string('stau'),
    massPoint = cms.untracked.int32(MCHI),  # value not used
    particleFile = cms.untracked.string('DisappTrks/SignalMC/data/geant4_higgsino/geant4_higgsino_%sGeV_ctau%scm.slha' % (MCHI, int(CTAU/10)))
)

ProductionFilterSequence = cms.Sequence(externalLHEProducer*generator)
