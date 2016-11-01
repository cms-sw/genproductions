# General Import
import FWCore.ParameterSet.Config as cms

# Import settings for modules
from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

# Define the generator module
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(1.1),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar, # tau polarisation switch on
            TauolaDefaultInputCards #set default TAUOLA input card
        ),
        parameterSets = cms.vstring('Tauola')
    ),
    UseExternalGenerators = cms.untracked.bool(True),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0         ! User defined processes',
            'MSUB(31)  = 1      ! qg --> qW',
            'MSTP(9)   = 1      ! to include top in allowed flavour remnants in  q` -> W + q'
            'CKIN(1)    = 303.462 ! 1.2*(mtop+mW)=1.2*(172.5+80.385) M HAT LOW CUT',
	    'CKIN(3)    = 20    ! ',
            # Only allow b and g as incoming partons
            'KFIN(1,1)   = 0 ! d',
            'KFIN(1,-1)   = 0 ! dbar',
            'KFIN(1,2)   = 0 ! u',
            'KFIN(1,-2)   = 0 ! ubar',    
            'KFIN(1,3)   = 0 ! s',    
            'KFIN(1,-3)   = 0 ! sbar',
            'KFIN(1,4)   = 0 ! c',    
            'KFIN(1,-4)   = 0 ! cbar',
            'KFIN(1,6)   = 0 ! t',    
            'KFIN(1,-6)   = 0 ! tbar',
            'KFIN(2,1)   = 0 ! d',
            'KFIN(2,-1)   = 0 ! dbar',
            'KFIN(2,2)   = 0 ! u',    
            'KFIN(2,-2)   = 0 ! ubar',
            'KFIN(2,3)   = 0 ! s',
            'KFIN(2,-3)   = 0 ! sbar',
            'KFIN(2,4)   = 0 ! c',
            'KFIN(2,-4)   = 0 ! cbar',
            'KFIN(2,6)   = 0 ! t',
            'KFIN(2,-6)   = 0 ! tbar',
            # Force b->W+t branchings
            #'MDME(33,1) = 0   ! b decay into g b',
            #'MDME(34,1) = 0   ! b decay into gamma b',     
            #'MDME(35,1) = 0   ! b decay into Z0 b',     
            'MDME(36,1) = 0   ! b decay into W- u',     
            'MDME(37,1) = 0   ! b decay into W- c',     
            'MDME(38,1) = 1   ! b decay into W- t',     
            'MDME(39,1) = 0   ! b decay into W- t`',     
            #'MDME(40,1) = 0   ! b decay into h0 b',     

            # Set the masses of the higgs
            'PMAS(6,1)  = 172.5  ! top quark mass',                                        
            'PMAS(37,1) = 80  ! charged Higgs mass ',
            # Switch off / on desirable channels for top quark

            'MDME(41,1) = 0   ! t decay into g t', 
            'MDME(42,1) = 0   ! t decay into gamma t', 
            'MDME(43,1) = 0   ! t decay into Z0 t', 
            'MDME(44,1) = 0   ! t decay into W d', 
            'MDME(45,1) = 0   ! t decay into W s', 
            'MDME(46,1) = 0   ! t decay into W and b', 
            'MDME(47,1) = 0   ! t decay into W b` ', 
            'MDME(48,1) = 0   ! t decay into h0 t', 
            'MDME(49,1) = 1   ! t decay into H and b', 
            'MDME(50,1) = 0   ! t decay into ~chi_10 ~t_1', 
            'MDME(51,1) = 0   ! t decay into ~chi_20 ~t_1', 
            'MDME(52,1) = 0   ! t decay into ~chi_30 ~t_1', 
            'MDME(53,1) = 0   ! t decay into ~chi_40 ~t_1', 
            'MDME(54,1) = 0   ! t decay into ~g ~t_1', 
            'MDME(55,1) = 0   ! t decay into ~Gravitino ~t_1', 
            # Switch off / on desirable channels for H+
            'MDME(503,1) = 0   ! H+ decay into dbar u', 
            'MDME(504,1) = 0   ! H+ decay into sbar c', 
            'MDME(505,1) = 0   ! H+ decay into bbar t', 
            'MDME(506,1) = 0   ! H+ decay into b`bar t`', 
            'MDME(507,1) = 0   ! H+ decay into e+ nu_e', 
            'MDME(508,1) = 0   ! H+ decay into mu+ nu_mu', 
            'MDME(509,1) = 1   ! H+ decay into tau+ nu_tau (channel is switched on)', 
            'MDME(510,1) = 0   ! H+ decay into tau`+ nu`_tau', 
            'MDME(511,1) = 0   ! H+ decay into W+ h0', 
            'MDME(512,1) = 0   ! H+ decay into ~chi_10 ~chi_1+', 
            'MDME(513,1) = 0   ! H+ decay into ~chi_10 ~chi_2+', 
            'MDME(514,1) = 0   ! H+ decay into ~chi_20 ~chi_1+', 
            'MDME(515,1) = 0   ! H+ decay into ~chi_20 ~chi_2+', 
            'MDME(516,1) = 0   ! H+ decay into ~chi_30 ~chi_1+', 
            'MDME(517,1) = 0   ! H+ decay into ~chi_30 ~chi_2+', 
            'MDME(518,1) = 0   ! H+ decay into ~chi_40 ~chi_1+', 
            'MDME(519,1) = 0   ! H+ decay into ~chi_40 ~chi_2+', 
            'MDME(520,1) = 0   ! H+ decay into ~t_1 ~b_1bar   ', 
            'MDME(521,1) = 0   ! H+ decay into ~t_2 ~b_1bar   ', 
            'MDME(522,1) = 0   ! H+ decay into ~t_1 ~b_2bar   ', 
            'MDME(523,1) = 0   ! H+ decay into ~t_2 ~b_2bar   ', 
            'MDME(524,1) = 0   ! H+ decay into ~d_Lbar ~u_L   ', 
            'MDME(525,1) = 0   ! H+ decay into ~s_Lbar ~c_L   ', 
            'MDME(526,1) = 0   ! H+ decay into ~e_L+ ~nu_muL  ', 
            'MDME(527,1) = 0   ! H+ decay into ~mu_L+ ~u_L    ', 
            'MDME(528,1) = 0   ! H+ decay into ~tau_1+ ~nu_tauL', 
            'MDME(529,1) = 0   ! H+ decay into ~tau_2+ ~nu_tauL'),                            
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.4 $'),
    name = cms.untracked.string('$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/PYTHIA6_Tauola_TTbar_H80_taunu_8TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-TW Hplus80 To TauNu, with Tauola at 8TeV')
    )

