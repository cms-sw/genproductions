import FWCore.ParameterSet.Config as cms

pythiaUESettingsBlock = cms.PSet(
        pythiaUESettings = cms.vstring(
                'MSTU(21)=1     ! Check on possible errors during program execution',
                'MSTJ(22)=2     ! Decay those unstable particles',
                'PARJ(71)=10 .  ! for which ctau  10 mm',
                'MSTP(33)=0     ! no K factors in hard cross sections',
                'MSTP(2)=1      ! which order running alphaS',
                'MSTP(51)=10042 ! structure function chosen (external PDF CTEQ6L1)',
                'MSTP(52)=2     ! work with LHAPDF',

                'PARP(82)=1.921 ! pt cutoff for multiparton interactions',
                'PARP(89)=1800. ! sqrts for which PARP82 is set',
                'PARP(90)=0.227 ! Multiple interactions: rescaling power',

                'MSTP(95)=6     ! CR (color reconnection parameters)',
                'PARP(77)=1.016 ! CR',
                'PARP(78)=0.538 ! CR',

                'PARP(80)=0.1   ! Prob. colored parton from BBR',

                'PARP(83)=0.356 ! Multiple interactions: matter distribution parameter',
                'PARP(84)=0.651 ! Multiple interactions: matter distribution parameter',

                'PARP(62)=1.025 ! ISR cutoff',

                'MSTP(91)=1     ! Gaussian primordial kT',
                'PARP(93)=10.0  ! primordial kT-max',

                'MSTP(81)=21    ! multiple parton interactions 1 is Pythia default',
                'MSTP(82)=4     ! Defines the multi-parton model',
        )
)

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(14000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL         = 11 ', 
            'MDME( 174,1) = 0    !Z decay into d dbar', 
            'MDME( 175,1) = 0    !Z decay into u ubar', 
            'MDME( 176,1) = 0    !Z decay into s sbar', 
            'MDME( 177,1) = 0    !Z decay into c cbar', 
            'MDME( 178,1) = 0    !Z decay into b bbar', 
            'MDME( 179,1) = 0    !Z decay into t tbar', 
            'MDME( 182,1) = 1    !Z decay into e- e+', 
            'MDME( 183,1) = 0    !Z decay into nu_e nu_ebar', 
            'MDME( 184,1) = 0    !Z decay into mu- mu+', 
            'MDME( 185,1) = 0    !Z decay into nu_mu nu_mubar', 
            'MDME( 186,1) = 0    !Z decay into tau- tau+', 
            'MDME( 187,1) = 0    !Z decay into nu_tau nu_taubar', 
            'CKIN( 1)     = 40.  !(D=2. GeV)', 
            'CKIN( 2)     = -1.  !(D=-1. GeV)',
            'CKIN( 13)    = -4.0 ',
	    'CKIN( 14)    = 4.0  ',
	    'CKIN( 15)    = -4.0 ',
	    'CKIN( 16)    = 4.0  '),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)
