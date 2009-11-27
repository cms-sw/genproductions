import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(7000.0),
    crossSection  = cms.untracked.double(0.432000),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0                  !(D=1) to select between full user control (0,\\ then use MSUB) and some preprogrammed alternative', 
            'PMAS(42,1)=300.0         !LQ mass', 
            'KFDP(539,1)=4           !LQ-> mu+c', 
            'KFDP(539,2)=13          !mu', 
            'MSUB(163)=1             !g+g->LQ+LQbar', 
            'MSUB(164)=1             !q+qbar->LQ+LQbar'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)
ProductionFilterSequence = cms.Sequence(generator)
