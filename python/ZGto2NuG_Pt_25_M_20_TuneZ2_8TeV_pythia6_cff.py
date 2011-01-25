import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.double(8000.0),
    crossSection = cms.untracked.double(3.132),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL        = 0    !User defined processes',
                'MSUB(19)=1 !Zgamma production',
                'CKIN(3)=25.          ! minimum pt hat for hard interactions',
                'MSTJ(41)=2        !Switch on Pythia QCD, QED Brem',
                'CKIN(43)=20.      !Minimum sqrt(s_hat) value (=Z mass)',
    'MDME(174,1)=0            !Z decay into d dbar',
                'MDME(175,1)=0            !Z decay into u ubar',
                'MDME(176,1)=0            !Z decay into s sbar',
                'MDME(177,1)=0            !Z decay into c cbar',
                'MDME(178,1)=0            !Z decay into b bbar',
                'MDME(179,1)=0            !Z decay into t tbar',
                'MDME(182,1)=0            !Z decay into e- e+',
                'MDME(183,1)=1            !Z decay into nu_e nu_ebar',
                'MDME(184,1)=0            !Z decay into mu- mu+',
                'MDME(185,1)=1            !Z decay into nu_mu nu_mubar',
                'MDME(186,1)=0            !Z decay into tau- tau+',
                'MDME(187,1)=1            !Z decay into nu_tau nu_taubar'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/ZGto2NuG_Pt_25_M_20_TuneZ2_7TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-EWK Zgamma, Z invisible decay, at 8TeV')
)


