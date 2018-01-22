import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring(''),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)


import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'JetMatching:setMad = off',     
            'JetMatching:scheme = 1',       # Madgraph MLM
            'JetMatching:exclusive    = 2', # Should be 2
            'JetMatching:merge = on',       # should be on. Off = all events accepted
            'JetMatching:jetAlgorithm = 2', # Should be 2 for MLM
            'JetMatching:etaJetMax = 5.',   
            'JetMatching:coneRadius = 1.',
            'JetMatching:slowJetPower  = 1', # -1 antikt, 0 CA, 1 kt
            'JetMatching:qCut = 24',         # this is the actual merging scale
            'JetMatching:nQmatch = 5',       # 4 corresponds to 4-flavour scheme (no matching of b-quarks), 5 for 5-flavour scheme
            'JetMatching:nJetMax = 1',       # number of partons in born matrix element for highest multiplicity
            'JetMatching:doShowerKt = off',  # off for MLM matching, turn on for shower-kT matching
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters',
                                    )
    )
)
