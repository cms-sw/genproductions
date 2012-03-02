import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from Configuration.GenProduction.EightTeV.LongLivedChi0_TuneZ2Star_8TeV_pythia6_cff import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter", 
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(0.01070),
    comEnergy = cms.double(8000.0),
    UseExternalGenerators = cms.untracked.bool(False),
    PythiaParameters = cms.PSet(
            pythiaUESettingsBlock,
            pythiaStandardRPVBlock,
            pythiaParameters = cms.vstring(         
            'IMSS(51)=3               ! Turn on Lepton number violating LLE decay channels with all couplings set to zero',
            'RVLAM(1,2,2)=0.0011    ! Set lambda (1,2,2)',
            'RVLAM(1,2,1)=0.0011    ! Set lambda (2,1,1)',
            'RMSS(1)=150              ! M1 mass',
            'RMSS(8)=1000              ! Left squark mass',
            'RMSS(9)=1000              ! Right squark mass'
        ),
        parameterSets = cms.vstring('pythiaUESettings',
            'pythiaStandardRPVSettings',
            'pythiaParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator) 
