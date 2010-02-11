import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.5),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(0.64),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(42,1)=300.0        ! LQ mass', 
            'IMSS(21)=33             ! LUN number for SLHA File (must be 33)', 
            'IMSS(22)=33             ! Read-in SLHA decay table', 
            'MDCY(C111,1)=0          ! ', 
            'MSEL=0                  ! (D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative', 
            'MSUB(163)=1             ! g+g->LQ+LQbar', 
            'MSUB(164)=1             ! q+qbar->LQ+LQbar'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters',
            'SLHAParameters'),
        SLHAParameters = cms.vstring('SLHAFILE = Configuration/Generator/data/LQ_cmusnumu_beta0.5.out')
    )
)

#munumujjFilter = cms.EDFilter("LQGenFilter",
#    src	= cms.untracked.InputTag("generator"),
#    eejj	= cms.bool(False),
#    enuejj	= cms.bool(False),
#    nuenuejj	= cms.bool(False),
#    mumujj	= cms.bool(False),
#    munumujj	= cms.bool(True),
#    numunumujj = cms.bool(False)
#)
#ProductionFilterSequence = cms.Sequence(generator*munumujjFilter)
ProductionFilterSequence = cms.Sequence(generator)
