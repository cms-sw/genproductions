import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
from GeneratorInterface.Pythia6Interface.TauolaSettings_cff import *
#################################################
source = cms.Source("PythiaSource",					       
    pythiaHepMCVerbosity = cms.untracked.bool(False),			       
    maxEventsToPrint = cms.untracked.int32(0),				       
    pythiaPylistVerbosity = cms.untracked.int32(0),			       
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),	
    crossSection = cms.untracked.double(55000000000.),
    ##### add Tauola to Pythia
    ExternalGenerators = cms.PSet(
           Tauola = cms.untracked.PSet(
                       TauolaPolar,
                       InputCards = cms.vstring('TAUOLA = 0 0 120 ! TAUOLA ')
           ),
           parameterSets = cms.vstring('Tauola')
    ),
    UseExternalGenerators = cms.untracked.bool(True),
    #################################################
    PythiaParameters = cms.PSet(					       
	pythiaUESettingsBlock,
	processParameters = cms.vstring(
            'MSEL=0            ! User defined processes',
            'MSUB(102)=0       ! gg->H',
            'MSUB(123)=1       ! qq-ZZqq->Hqq',
            'MSUB(124)=1       ! qq-WWqq->Hqq',
	    'PMAS(23,1)=91.188 ! Z mass',
	    'PMAS(24,1)=80.450 ! W mass',
	    'PMAS(25,1)=115.0  ! mass of Higgs',
# Higgs boson decays
            'MDME(210,1)=0    ! Higgs decay into dd', 		       
            'MDME(211,1)=0    ! Higgs decay into uu', 		       
            'MDME(212,1)=0    ! Higgs decay into ss', 		       
            'MDME(213,1)=0    ! Higgs decay into cc', 		       
            'MDME(214,1)=0    ! Higgs decay into bb', 		       
            'MDME(215,1)=0    ! Higgs decay into tt', 		       
            'MDME(216,1)=0    ! Higgs decay into', 		       
            'MDME(217,1)=0    ! Higgs decay into Higgs decay', 	       
            'MDME(218,1)=0    ! Higgs decay into e e', 	       
            'MDME(219,1)=0    ! Higgs decay into mu mu', 
# Higgs->tau tau	       
            'MDME(220,1)=1    ! Higgs decay into tau tau', 	       
            'MDME(221,1)=0    ! Higgs decay into Higgs decay', 	       
            'MDME(222,1)=0    ! Higgs decay into g g', 		       
            'MDME(223,1)=0    ! Higgs decay into gam gam', 	       
            'MDME(224,1)=0    ! Higgs decay into gam Z', 	       
            'MDME(225,1)=0    ! Higgs decay into Z Z', 		       
            'MDME(226,1)=0    ! Higgs decay into W W'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 'processParameters')
    )									       
)									       
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Tauola_SM_VBF_H_tautau_2l_mH115_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 Tauola SM VBF H->tautau->2l at 10TeV with mH=115 GeV')
)
