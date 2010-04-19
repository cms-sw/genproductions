import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000515),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000120),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000346),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.708),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0245),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00281),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000515),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000120),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000346),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.708),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0245),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00281),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000515),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000120),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000346),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.708),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0245),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00281),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000515),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000120),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000346),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.708),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0245),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00281),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000515),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000120),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000346),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.708),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0245),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00281),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000515),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000120),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000346),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.708),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0245),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00281),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000515),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000120),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000346),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.708),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0245),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00281),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .05         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000579),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000395),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.829),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0284),
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0031),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00349),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00103),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000311),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.715),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.079),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0556),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0138),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00418),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00124),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.309),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000579),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000395),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.829),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0284),
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0031),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0146),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00349),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00103),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000311),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.715),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.079),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0556),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0138),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00418),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00124),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.309),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000579),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000395),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.829),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0284),
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0031),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0146),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00349),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00103),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000311),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.715),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.079),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0556),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0138),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00418),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00124),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.309),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000455),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000179),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000493),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.856),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0305),
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00248),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000579),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000395),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.829),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0284),
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0031),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0146),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00349),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00103),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000311),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.715),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.079),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0556),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0138),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00418),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00124),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.309),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000579),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000395),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.829),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0284),
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0031),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0146),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00349),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00103),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000311),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.715),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.079),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0556),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0138),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00418),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00124),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.309),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000585),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00014),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000404),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.830),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0288),
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00316),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.014),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00349),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00103),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000311),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.715),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.079),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0556),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0138),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00418),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00124),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.309),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000589),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000398),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.823),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0291),
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00318),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000589),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000398),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.823),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0291),
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00318),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0147),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00352),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000994),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000317),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.723),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0828),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0575),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0141),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00122),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000427),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.331),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4092,1)=1            ! e+ e-', 
            'MDME(4093,1)=0            ! nu_e nu_ebar', 
            'MDME(4094,1)=0            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravEE_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000585),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00014),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0000404),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.830),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 250.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0288),
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00316),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.014),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0035),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00101),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000318),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.724),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 500.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0804),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .27         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl005_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.05, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0582),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.0139),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1250.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1250_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1250 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00400),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1500.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1500_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1500 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.00125),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M1750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 1750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000425),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 2000.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M2000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 2000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.318),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 750.         !mass of RS Graviton', 
            'PARP(50) = .54         ! 0.054 == c=0.01 (k/M_PL=0.01)', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl01_M750_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.1, mass = 750 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",   
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    crossSection  = cms.untracked.double(0.000585),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(347,1)= 1000.         !mass of RS Graviton', 
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
            'MDME(4094,1)=1            ! mu- mu+', 
            'MDME(4095,1)=0            ! nu_mu nu_mubar', 
            'MDME(4096,1)=0            ! tau- tau+', 
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

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EXOTICA_RSGravMuMu_kMpl001_M1000_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 RS Graviton to ee, k/Mpl = 0.01, mass = 1000 at sqrt(s) = 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
