import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *

generator = cms.EDFilter(
    "ThePEGGeneratorFilter",
    herwigDefaultsBlock,
    configFiles = cms.vstring(),

    parameterSets = cms.vstring(
    'cm8TeV',
    'pdfMRST2001',
    'ZllJetParameters',
    'basicSetup',
    'setParticlesStableForDetector',
    ),

    ZllJetParameters = cms.vstring(
    'cd /Herwig/MatrixElements/',
    'insert SimpleQCD:MatrixElements[0] MEZJet',
    'set MEZJet:GammaZ Z',
    'set MEZJet:ZDecay ChargedLeptons',
    'set /Herwig/Cuts/ZBosonKtCut:MinKT 100.0*GeV',
    ),
    
    crossSection     = cms.untracked.double(1.0),
    filterEfficiency = cms.untracked.double(1.0)
    )


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('\$Revision: 1.1 $'),
    name = cms.untracked.string('\$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/ZJetsToLL_ptZ100toInf_8TeV_herwigpp_cff.py,v $'),
    annotation = cms.untracked.string('HERWIGPP: Z->ll+Jets, pT(Z)>100.GeV, l=e or mu or tau.')
)
