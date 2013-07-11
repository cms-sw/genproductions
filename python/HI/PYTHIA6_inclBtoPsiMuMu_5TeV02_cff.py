import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.1 $'),
        name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/HI/PYTHIA6_inclBtoPsiMuMu_5TeV02_cff.py,v $'),
        annotation = cms.untracked.string('Winter13: Pythia6+EvtGen generation of B0->Jpsi->MuMu, 5.023TeV, Z2star tune')
)

#from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

# this is Z2star --Matt
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
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(5023.0),
#   BF(B0->JPsiX) * BF(JPsi->mumu) = 0.0185 * 0.0593
    crossSection = cms.untracked.double(54402709.5),
    filterEfficiency = cms.untracked.double(0.0003114),
    maxEventsToPrint = cms.untracked.int32(0),
    ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
             operates_on_particles = cms.vint32( 0 ), # 0 (zero) means default list (hardcoded)
                                                      # you can put here the list of particles (PDG IDs)
                                                      # that you want decayed by EvtGen
             use_default_decay = cms.untracked.bool(False),
             decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
             #decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY.DEC'),
             particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
             #user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Validation.dec'),
             user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/incl_BtoJpsi_mumu.dec'),
             list_forced_decays = cms.vstring('MyB0',
                                              'Myanti-B0',
                                              'MyB+',
                                              'MyB-',
                                              'MyB_s0',
                                              'Myanti-B_s0',
                                              'MyLambda_b0',
                                              'Myanti-Lambda_b0'),
        ),
        parameterSets = cms.vstring('EvtGen')
    ),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        bbbarSettings = cms.vstring('MSEL = 1'), 
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring(
             'pythiaUESettings',
             'bbbarSettings')
    )
)

bfilter = cms.EDFilter("PythiaFilter",
    ParticleID = cms.untracked.int32(5)
)

## oniafilter = cms.EDFilter("PythiaFilter",
##    Status = cms.untracked.int32(2),
##    MaxEta = cms.untracked.double(1000.0),
##    MinEta = cms.untracked.double(-1000.0),
##    MinPt = cms.untracked.double(0.0),
##    ParticleID = cms.untracked.int32(443)
##)

oniafilter = cms.EDFilter("MCSingleParticleFilter",
    Status = cms.untracked.vint32(     2,    2),
    ParticleID = cms.untracked.vint32(443, 100443),
    MinPt = cms.untracked.vdouble(    0.0, 0.0),
    MaxEta = cms.untracked.vdouble(   1000.0, 1000.0),
    MinEta = cms.untracked.vdouble(   -1000.0, -1000.0),
)

mumugenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinP = cms.untracked.vdouble(2.5, 2.5),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)


ProductionFilterSequence = cms.Sequence(generator*bfilter*oniafilter*mumugenfilter)


