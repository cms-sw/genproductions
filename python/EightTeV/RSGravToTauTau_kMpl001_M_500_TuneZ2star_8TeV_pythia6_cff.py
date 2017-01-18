import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *


generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.04288),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .054         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
            'MSEL=0                    !(D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative', 
            'MSUB(391)=1               ! q qbar -> G* ', 
            'MSUB(392)=1               ! g g -> G*', 
            'MDME(4084,1)=0            ! d dbar', 
            'MDME(4085,1)=0            ! u ubar', 
            'MDME(4086,1)=0            ! s sbar', 
            'MDME(4087,1)=0            ! c cbar', 
            'MDME(4088,1)=0            ! b bbar', 
            'MDME(4089,1)=0            ! t tbar', 
            'MDME(4090,1)=-1            ! bprime bprimebar', 
            'MDME(4091,1)=-1            ! tprime tprimebar', 
            'MDME(4092,1)=0            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=1            ! tau- tau+', 
            'MDME(4097,1)=0            ! nu_tau  nu_taubar', 
            'MDME(4098,1)=-1            ! tauprime- tauprime+ ', 
            'MDME(4099,1)=-1            ! nuprime_tau nuprime_taubar ', 
            'MDME(4100,1)=0            ! g  g  ', 
            'MDME(4101,1)=0            ! gamma gamma ', 
            'MDME(4102,1)=0            ! Z Z', 
            'MDME(4103,1)=0            ! W W', 
            'CKIN(3)=-1.          ! minimum pt hat for hard interactions', 
            'CKIN(4)=-1.          ! maximum pt hat for hard interactions'),

        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

#configurationMetadata = cms.untracked.PSet(
#    version = cms.untracked.string('$Revision: 1.1 $'),
#    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/RSGravEE_kMpl001_M_1000_TuneZ2star_8TeV_pythia6_cff.py,v $'),
#    annotation = cms.untracked.string('PYTHIA6 RS Graviton to gammagamma, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
#)

ProductionFilterSequence = cms.Sequence(generator)


