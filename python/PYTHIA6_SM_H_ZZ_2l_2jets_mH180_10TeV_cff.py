import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",					       
    pythiaHepMCVerbosity = cms.untracked.bool(False),			       
    maxEventsToPrint = cms.untracked.int32(0),				       
    pythiaPylistVerbosity = cms.untracked.int32(0),			       
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),				       
    PythiaParameters = cms.PSet(					       
	pythiaUESettingsBlock,
	processParameters = cms.vstring(
            'MSEL=0            ! User defined processes',
            'MSUB(102)=0       ! ggH', 				       
            'MSUB(123)=1       ! ZZ fusion to H', 		       
            'MSUB(124)=1       ! WW fusion to H', 		       
	    'PMAS(23,1)=91.188 ! Z mass',
	    'PMAS(24,1)=80.450 ! W mass',
	    'PMAS(25,1)=180.0  ! mass of Higgs',

            'MSTJ(41)=1       ! Switch off Pythia QED bremsshtrahlung', 
            
            'CKIN(45)=5.      ! high mass cut on m2 in 2 to 2 process',   
            'CKIN(46)=150.    ! high mass cut on secondary resonance    
                              ! m1 in 2->1->2 process',
            'CKIN(47)=5.      ! low mass cut on secondary resonance     
                              ! m2 in 2->1->2 process',
            'CKIN(48)=150.    ! high mass cut on secondary resonance    
	                      ! m2 in 2->1->2 process',
# Higgs boson decays
            'MDME(210,1)=0    ! Higgs decay into dd', 		       
            'MDME(211,1)=0    ! Higgs decay into uu', 		       
            'MDME(212,1)=0    ! Higgs decay into ss', 		       
            'MDME(213,1)=0    ! Higgs decay into cc', 		       
            'MDME(214,1)=0    ! Higgs decay into bb', 		       
            'MDME(215,1)=0    ! Higgs decay into tt', 		       
            'MDME(216,1)=0    ! Higgs decay into', 		       
            'MDME(217,1)=0    ! Higgs decay into Higgs decay', 	       
            'MDME(218,1)=0    ! Higgs decay into e nu e', 	       
            'MDME(219,1)=0    ! Higgs decay into mu nu mu', 	       
            'MDME(220,1)=0    ! Higgs decay into tau nu tau', 	       
            'MDME(221,1)=0    ! Higgs decay into Higgs decay', 	       
            'MDME(222,1)=0    ! Higgs decay into g g', 		       
            'MDME(223,1)=0    ! Higgs decay into gam gam', 	       
            'MDME(224,1)=0    ! Higgs decay into gam Z', 	       
            'MDME(225,1)=1    ! Higgs decay into Z Z', 		       
            'MDME(226,1)=0    ! Higgs decay into W W', 		       
            'MSTP(128)=2      ! dec.prods out of doc section,	       
	                      ! point at parents in the main section', 
# Z boson decays: Z->q qbar
            'MDME(174,1)=4           ! Z decay into d dbar', 		       
            'MDME(175,1)=4           ! Z decay into u ubar', 		       
            'MDME(176,1)=4           ! Z decay into s sbar', 		       
            'MDME(177,1)=4           ! Z decay into c cbar', 		       
            'MDME(178,1)=4           ! Z decay into b bbar', 		       
            'MDME(179,1)=0           ! Z decay into t tbar',
# Z boson decays: Z->e e      
            'MDME(182,1)=5           ! Z decay into e- e+', 		       
            'MDME(183,1)=0           ! Z decay into nu_e nu_ebar',
# Z boson decays: Z->mu mu                  
            'MDME(184,1)=5           ! Z decay into mu- mu+', 		       
            'MDME(185,1)=0           ! Z decay into nu_mu nu_mubar', 	       
# Z boson decays: Z->tau tau
            'MDME(186,1)=5           ! Z decay into tau- tau+', 	       
            'MDME(187,1)=0           ! Z decay into nu_tau nu_taubar'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 'processParameters')
    )									       
)									       
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_SM_H_ZZ_2l_2jets_mH180_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 SM H->ZZ->2l2jets at 10TeV with mH=180 GeV')
)
