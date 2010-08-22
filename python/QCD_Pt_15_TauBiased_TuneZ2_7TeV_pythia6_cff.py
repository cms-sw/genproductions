import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.03605),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=1               ! QCD hight pT processes', 
            'CKIN(3)=15.          ! minimum pt hat for hard interactions'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

genLeadTrackFilter = cms.EDFilter('GenLeadTrackFilter',
  HepMCProduct             = cms.InputTag("generator"),
  GenLeadTrackPt           = cms.double(12),
  GenEta                   = cms.double(2.5)
)

configurationMetadata = cms.untracked.PSet(
  version = cms.untracked.string('$Revision: 1.1 $'),
  name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/Attic/PYTHIA6_QCD_Pt_15_7TeV_GenLeadTrackFilter_cff.py,v $'),
  annotation = cms.untracked.string('PYTHIA6-Biased QCD with 12 GeV lead MC track at 7 TeV')
)

ProductionFilterSequence = cms.Sequence(generator*genLeadTrackFilter)



