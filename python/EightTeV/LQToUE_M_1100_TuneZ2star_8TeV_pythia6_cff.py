import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(6.844e-05),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'PMAS(42,1)=1100.0          ! LQ mass',
            'IMSS(21)=33             ! LUN number for SLHA File (must be 33)',
            'IMSS(22)=33             ! Read-in SLHA decay table',
            'MSEL=0                  ! (D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative',
            'MSUB(163)=1             ! g+g->LQ+LQbar',
            'MSUB(164)=1             ! q+qbar->LQ+LQbar'
        ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring(
            'pythiaUESettings',
            'processParameters',
            'SLHAParameters'
        ),
        SLHAParameters = cms.vstring('SLHAFILE = Configuration/Generator/data/LQ_ue_beta1.0.out')
    )
)

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.1 $'),
        name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/LQToUE_M_350_7TeV_pythia6_cff.py,v $'),
        annotation = cms.untracked.string('default documentation string for LQToUE_M_350_7TeV_pythia6_cff.py')
)
