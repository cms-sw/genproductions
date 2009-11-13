import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter", 
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(10000.0),
    crossSection = cms.untracked.double(14.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring( 
            'MSEL = 39                !',    
            'IMSS(1) = 2              !  mSUGRA', 
            'IMSS(51) = 3             !  R-parity violation',
            'RMSS(8)  =   60.         !  m_0',
            'RMSS(1)  =   250.        !  m_1/2',
            'RMSS(4)  =   1.D0        !  Sign(mu)', 
            'RMSS(5)  =   10.D0       !  tan(beta)',
            'RMSS(16) =   0.D0        !  A_top',
            'RVLAM(1,2,1) = 0.                ',         
            'RVLAM(1,2,2) = 0.                ', 
            'RVLAM(1,2,3) = 0.                ', 
            'RVLAM(1,3,1) = 0.                ',
            'RVLAM(1,3,2) = 0.                ',
            'RVLAM(1,3,3) = 0.                ',
            'RVLAM(2,1,1) = 0.                ',
            'RVLAM(2,1,2) = 0.                ',
            'RVLAM(2,1,3) = 0.                ',
            'RVLAM(3,1,1) = 0.                ',
            'RVLAM(3,1,2) = 0.                ',
            'RVLAM(3,1,3) = 0.                ',
            'RVLAM(3,2,1) = 0.                ',
            'RVLAM(3,2,2) = 0.                ', 
            'RVLAM(3,2,3) = 0.                ',
            'RVLAM(2,3,1) = 0.                ',
            'RVLAM(2,3,2) = 0.0001            ',  
            'RVLAM(2,3,3) = 0.                '),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)
# the definition of the Filter Sequence
ProductionFilterSequence = cms.Sequence(generator)

