import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.GenProduction.PythiaUESettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(3),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(10000.0),
    crossSection = cms.untracked.double(242.8),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
        ),
        parameterSets = cms.vstring('Tauola')
    ),
    UseExternalGenerators = cms.untracked.bool(True),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0         ! User defined processes',
            'MSUB(81)  = 1     ! qqbar to QQbar',
            'MSUB(82)  = 1     ! gg to QQbar',
            'MSTP(7)   = 6     ! flavor = top',
            'PMAS(6,1) = 172.5  ! top quark mass',
            'PMAS(37,1)=160.0       !mass of charged Higgs', 
            'MDME(44,1)=4           !top decay into Wd', 
            'MDME(45,1)=4           !top decay into Ws', 
            'MDME(46,1)=4           !top decay into Wb', 
            'MDME(49,1)=5           !top decay into H+b', 
            'MDME(503,1)=0 !H+ decay into dbar u', 
            'MDME(504,1)=0 !H+ decay into sbar c', 
            'MDME(505,1)=0 !H+ decay into bbar t', 
            'MDME(507,1)=0 !H+ decay into e+   nu_e', 
            'MDME(508,1)=0 !H+ decay into mu+  nu_mu', 
            'MDME(509,1)=1 !H+ decay into tau+ nu_tau', 
            'MDME(511,1)=0 !H+ decay into W+   h0' 
	    ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.8 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Tauola_TTbar_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('tt->H+(160)bt at 10TeV')
)
