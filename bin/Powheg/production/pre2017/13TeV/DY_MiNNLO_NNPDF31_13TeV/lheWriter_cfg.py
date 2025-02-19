#!/usr/bin/env cmsRun
import FWCore.ParameterSet.Config as cms
import os

if os.path.isfile('../PSet.pkl'): # CRAB
    import pickle
    originalConfig = pickle.load(open('../PSet.pkl', 'rb'))
else: # local testing
    from shutil import copyfile
    f = 'runMassWeights_ZJToMuMu_cfg.py'
    copyfile('../'+f, f)
    originalConfig = __import__(f.replace('.py','')).process

process = cms.Process("Writer")

process.maxEvents = originalConfig.maxEvents

process.source = originalConfig.source

process.load("FWCore.MessageService.MessageLogger_cfi")
process.MessageLogger.cerr.threshold = 'INFO'

process.writer = cms.EDAnalyzer("LHEWriter",
    moduleLabel = cms.untracked.InputTag('externalLHEProducer')
)

process.outpath = cms.EndPath(process.writer)
