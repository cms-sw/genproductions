import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUESettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         maxEventsToPrint = cms.untracked.int32(0),       
                         pythiaPylistVerbosity = cms.untracked.int32(0),  
                         displayPythiaCards = cms.untracked.bool(False),  
                         filterEfficiency = cms.untracked.double(1.0),
                         comEnergy = cms.double(7000.0),
                         crossSection = cms.untracked.double(1.0),
                         PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        HZToLLInvParameters = cms.vstring(
            'PMAS(25,1)=150.0        !Higgs mass', 
            'MSEL=0                  !(D=1) to select between full user control (0, then use MSUB) and some preprogrammed alternative', 
            'MSUB(102)=0             !ggH', 
            'MSUB(123)=0             !ZZ fusion to H', 
            'MSUB(124)=0             !WW fusion to H', 
            'MSUB(24)=1              !ZH Higgsstrahlung', 
            'MDME(174,1)=0           !Z decay into d dbar', 
            'MDME(175,1)=0           !Z decay into u ubar', 
            'MDME(176,1)=0           !Z decay into s sbar', 
            'MDME(177,1)=0           !Z decay into c cbar', 
            'MDME(178,1)=0           !Z decay into b bbar', 
            'MDME(179,1)=0           !Z decay into t tbar', 
            'MDME(180,1)=-1          !Z decay into b* b*bar',
            'MDME(181,1)=-1          !Z decay into t* t*bar',
            'MDME(182,1)=1           !Z decay into e- e+', 
            'MDME(183,1)=0           !Z decay into nu_e nu_ebar', 
            'MDME(184,1)=1           !Z decay into mu- mu+', 
            'MDME(185,1)=0           !Z decay into nu_mu nu_mubar', 
            'MDME(186,1)=1           !Z decay into tau- tau+', 
            'MDME(187,1)=0           !Z decay into nu_tau nu_taubar', 
            'MDME(188,1)=-1          !Z decay into tau* tau*bar', 
            'MDME(189,1)=-1          !Z decay into nu_tau* nu_tau*bar', 
            'MDCY(25,1)=0            !All Higgs decays switched off',
            ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring( 
	    'pythiaUESettings', 
            'HZToLLInvParameters'
            )
        )
                                 )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/ZHToLLInv_M150_7TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('default documentation string for ZHToLLInv_M150_7TeV_pythia6_cff.py')
    )

