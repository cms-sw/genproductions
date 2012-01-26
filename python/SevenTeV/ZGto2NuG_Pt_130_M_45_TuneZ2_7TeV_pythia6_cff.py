import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(3.802e-02),
    PythiaParameters = cms.PSet( 
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL = 0    !User defined processes',
                'MSUB(19)=1 !Zgamma production',
                'CKIN(3)=130.          ! minimum pt hat for hard interactions',
                'MSTJ(41)=2        !Switch on Pythia QCD, QED Brem',
                'CKIN(41)=45.      !Minimum sqrt(s_hat) value (=Z mass)',
                'MSTP(32)= 4        !Qsqr = shat',
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
        parameterSets = cms.vstring('pythiaUESettings',
                'processParameters')
        )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    name = cms.untracked.string('$Maj_GenFragment_cff.py,v $'),
    annotation = cms.untracked.string('Majorana neutrino 7TeV')
            )

