import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import * 
from GeneratorInterface.Pythia6Interface.TauolaSettings_cff import *
#################################################
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(55000000000.),
    ##### add Tauola to Pythia
    ExternalGenerators = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
        ),
        parameterSets = cms.vstring('Tauola')
    ),
    UseExternalGenerators = cms.untracked.bool(True),
    #################################################
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
            'MSEL      = 0     ! User defined processes',
            'MSUB(401) = 1     ! gg->tbH+',
            'MSUB(402) = 1     ! qq->tbH+',
            'IMSS(1)   = 1     ! MSSM',
            'RMSS(5)   = 30.   ! TANBETA',
            'RMSS(19)  = 161.  ! (D=850.) m_A',
            'MDME(503,1)=0           !Higgs(H+) decay into dbar            u',
            'MDME(504,1)=0           !Higgs(H+) decay into sbar            c',
            'MDME(505,1)=0           !Higgs(H+) decay into bbar            t',
            'MDME(506,1)=0           !Higgs(H+) decay into b bar           t',
            'MDME(507,1)=0           !Higgs(H+) decay into e+              nu_e',
            'MDME(508,1)=0           !Higgs(H+) decay into mu+             nu_mu',
            'MDME(509,1)=1           !Higgs(H+) decay into tau+            nu_tau',
            'MDME(510,1)=0           !Higgs(H+) decay into tau prime+      nu_tau',
            'MDME(511,1)=0           !Higgs(H+) decay into W+              h0',
            'MDME(512,1)=0           !Higgs(H+) decay into ~chi_10         ~chi_1+',
            'MDME(513,1)=0           !Higgs(H+) decay into ~chi_10         ~chi_2+',
            'MDME(514,1)=0           !Higgs(H+) decay into ~chi_20         ~chi_1+',
            'MDME(515,1)=0           !Higgs(H+) decay into ~chi_20         ~chi_2+',
            'MDME(516,1)=0           !Higgs(H+) decay into ~chi_30         ~chi_1+',
            'MDME(517,1)=0           !Higgs(H+) decay into ~chi_30         ~chi_2+',
            'MDME(518,1)=0           !Higgs(H+) decay into ~chi_40         ~chi_1+',
            'MDME(519,1)=0           !Higgs(H+) decay into ~chi_40         ~chi_2+',
            'MDME(520,1)=0           !Higgs(H+) decay into ~t_1            ~b_1bar',
            'MDME(521,1)=0           !Higgs(H+) decay into ~t_2            ~b_1bar',
            'MDME(522,1)=0           !Higgs(H+) decay into ~t_1            ~b_2bar',
            'MDME(523,1)=0           !Higgs(H+) decay into ~t_2            ~b_2bar',
            'MDME(524,1)=0           !Higgs(H+) decay into ~d_Lbar         ~u_L',
            'MDME(525,1)=0           !Higgs(H+) decay into ~s_Lbar         ~c_L',
            'MDME(526,1)=0           !Higgs(H+) decay into ~e_L+           ~nu_eL',
            'MDME(527,1)=0           !Higgs(H+) decay into ~mu_L+          ~nu_muL',
            'MDME(528,1)=0           !Higgs(H+) decay into ~tau_1+         ~nu_tauL',
            'MDME(529,1)=0           !Higgs(H+) decay into ~tau_2+         ~nu_tauL'
        ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Tauola_MSSM_Hch_tau_mA200_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-Hch->taunu mA=200GeV with TAUOLA at 10TeV')
)

