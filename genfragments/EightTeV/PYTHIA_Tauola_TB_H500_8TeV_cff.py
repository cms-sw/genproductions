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
            'MSUB(401)  = 1    ! qqbar',
            'MSUB(402)  = 1    ! gg',
            'MSTP(129)  = 1000',
            # Set the masses of the higgs and tan beta
            'PMAS(6,1)  = 172.5  ! top quark mass',                                        
            'PMAS(37,1) = 500.   ! charged Higgs mass ',
            'PARU(141)  = 30.    ! tan beta = 30', 
            # Switch off / on desirable channels for top quark
            'MDME(41,1) = 0   ! t decay into g t', 
            'MDME(42,1) = 0   ! t decay into gamma t', 
            'MDME(43,1) = 0   ! t decay into Z0 t', 
            'MDME(44,1) = 0   ! t decay into W d', 
            'MDME(45,1) = 0   ! t decay into W s', 
            'MDME(46,1) = 1   ! t decay into W and b (channel is ON for tbar, OFF for t)', 
            'MDME(47,1) = 0   ! t decay into W b` ', 
            'MDME(48,1) = 0   ! t decay into h0 t', 
            'MDME(49,1) = 0   ! t decay into H and b (channel is ON for t, OFF for tbar)', 
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
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/PYTHIA_Tauola_TB_H500_8TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-TB Hplus500, with Tauola at 8TeV')
    )

