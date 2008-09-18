import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(0.1609),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('PMAS(6,1)=172.3 ! t quark mass', 
            'PMAS(347,1)= 1250.0 ! graviton mass', 
            'PARP(50)=0.54  ! c(k/Mpl) * 5.4', 
            'MSEL=0         ! User defined processes', 
            'MSUB(391)=1    ! ffbar->G*', 
            'MSUB(392)=1    ! gg->G*', 
            '5000039:ALLOFF ! Turn off graviton decays', 
            '5000039:ONIFANY 24 ! graviton decays into W', 
            '24:ALLOFF ! Turn off W decays', 
            '24:ONIFANY 1 2 3 4 5 6 14 ! W decays to q and nu_mu', 
            'CKIN(3)=25.    ! Pt hat lower cut', 
            'CKIN(4)=-1.    ! Pt hat upper cut', 
            'CKIN(13)=-10.  ! etamin', 
            'CKIN(14)=10.   ! etamax', 
            'CKIN(15)=-10.  ! -etamax', 
            'CKIN(16)=10.   ! -etamin'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    annotation = cms.untracked.string('PYTHIA6-RS1250 to WW to jet+MET at 10TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Exotica_RS1250_WW_jetMET_10TeV_cff_py_GEN_IDEAL.py,v $')

)

