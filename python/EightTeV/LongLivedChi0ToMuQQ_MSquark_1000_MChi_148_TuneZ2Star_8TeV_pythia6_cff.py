import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from Configuration.GenProduction.EightTeV.LongLivedChi0_TuneZ2Star_8TeV_pythia6_cff import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter", 
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(0.00268),
    comEnergy = cms.double(8000.0),
    UseExternalGenerators = cms.untracked.bool(False),
    PythiaParameters = cms.PSet(
            pythiaUESettingsBlock,
            pythiaStandardRPVBlock,
            pythiaParameters = cms.vstring(         
            'IMSS(52)=3               ! Turn on Lepton number violating LQD decay channels with all couplings set to zero',
            'RVLAMP(2,1,1)=0.000175   ! Set lambda Prime(2,1,1)',
            'MDME(2241,1)=0           ! Turn off LQD decays to neutrinos',
            'MDME(2242,1)=0           ! Turn off LQD decays to neutrinos',        
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
