import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *
#from directorypath.gridpack import *
externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/afs/cern.ch/work/s/sabaxter/CMSSW_7_1_26/src/genproductions/bin/MadGraph5_aMCatNLO/DH_4f_LO_xqcut_20_tarball.tar.xz'),
    nEvents = cms.untracked.uint32(10000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)



#pythia 8 matching
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
            'JetMatching:scheme = 1',
            'JetMatching:merge = on',
            'JetMatching:jetAlgorithm = 2',
            'JetMatching:etaJetMax = 5.',
            'JetMatching:coneRadius = 1.',
            'JetMatching:slowJetPower = 1',
            'JetMatching:qCut = 25',  # actual merging scale 15
            'JetMatching:nQmatch = 4', # 4: 4-flavor scheme (no matching of b), 5: 5-flavour scheme
            'JetMatching:nJetMax = 1', # nPartons in born matrix element for highest multiplicity
            'JetMatching:doShowerKt = off', #off for MLM matching, turn on for shower-kT matching
            'Check:epTolErr = 0.0003',
            '55:new  = Zp  Zp  3 0 0 X_MZP_X  0 0 0 99999',
            '54:new  = Hs  Hs  1 0 0 X_MHs_X  0 0 0 99999',
            '52:new  = DM  DM  2 0 0 X_MDM_X  0 0 0 99999',
            '52:mayDecay = off'
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters'
                                    )
    )
)

ProductionFilterSequence = cms.Sequence(generator)
