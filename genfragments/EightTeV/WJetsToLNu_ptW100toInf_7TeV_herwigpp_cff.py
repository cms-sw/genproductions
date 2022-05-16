import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *

generator = cms.EDFilter(
    "ThePEGGeneratorFilter",
    herwigDefaultsBlock,
    configFiles = cms.vstring(),

    parameterSets = cms.vstring(
    'cm8TeV',
    'pdfMRST2001',
    'WlnuJetParameters',
    'basicSetup',
    'setParticlesStableForDetector',
    ),

    WlnuJetParameters = cms.vstring(
    'cd /Herwig/MatrixElements/',
    'insert SimpleQCD:MatrixElements[0] MEWJet',
    'set MEWJet:WDecay Leptons',
    'set /Herwig/Cuts/WBosonKtCut:MinKT 100.0*GeV'
    ),
    
    crossSection     = cms.untracked.double(1.0),
    filterEfficiency = cms.untracked.double(1.0)
    )


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('\$Revision: 1.1 $'),
    name = cms.untracked.string('\$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/WJetsToLNu_ptW100toInf_7TeV_herwigpp_cff.py,v $'),
    annotation = cms.untracked.string('HERWIGPP: W->lnu+Jets, pT(W)>100.GeV, l=e or mu or tau.')
)
