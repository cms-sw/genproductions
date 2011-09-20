import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUED6TSettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(.007341),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'PMAS(42,1)=800.0        ! LQ mass', 

            'MSEL=0                  ! (D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative', 
            'KFDP(539,1)=4           ! 1st particle c',
            'KFDP(539,2)=13          ! 2nd particle mu',
            'MSUB(162)=1             ! q+qbar->LQ+l'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')

    )
)

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.1 $'),
        name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/LQToCMu_M_250_7TeV_pythia6_cff.py,v $')
,
        annotation = cms.untracked.string('default documentation string for PYTHIA6_Exotica_LQ_cmu_250_7TeV_mumujj_cff.py')
)

ProductionFilterSequence = cms.Sequence(generator)
