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
    crossSection = cms.untracked.double(0.040),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
                               "PMAS(25,1)=120.0      !mass of Higgs",
                               "MSEL=0                  ! user selection for process",
                               "MSUB(102)=1             !ggH",
                               "MSUB(123)=1             !ZZ fusion to H",
                               "MSUB(124)=1             !WW fusion to H",
                               "MSUB(24)=1              !ZH production",
                               "MSUB(26)=1              !WH production",
                               "MSUB(121)=1             !gg to ttH",
                               "MSUB(122)=1             !qq to ttH",
                               # Higgs decays
                               "MDME(210,1)=0           !Higgs decay into dd",
                               "MDME(211,1)=0           !Higgs decay into uu",
                               "MDME(212,1)=0           !Higgs decay into ss",
                               "MDME(213,1)=0           !Higgs decay into cc",
                               "MDME(214,1)=0           !Higgs decay into bb",
                               "MDME(215,1)=0           !Higgs decay into tt",
                               "MDME(216,1)=0           !Higgs decay into",
                               "MDME(217,1)=0           !Higgs decay into Higgs decay",
                               "MDME(218,1)=0           !Higgs decay into e nu e",
                               "MDME(219,1)=0           !Higgs decay into mu nu mu",
                               "MDME(220,1)=0           !Higgs decay into tau nu tau",
                               "MDME(221,1)=0           !Higgs decay into Higgs decay",
                               "MDME(222,1)=0           !Higgs decay into g g",
                               "MDME(223,1)=1           !Higgs decay into gam gam",
                               "MDME(224,1)=0           !Higgs decay into gam Z",
                               "MDME(225,1)=0           !Higgs decay into Z Z",
                               "MDME(226,1)=0           !Higgs decay into W W"
                               ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_SM_H_VBfusion_gamgam_mH120_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 SM H vector boson fusion to gammagamma mH 120 GeV at 10TeV')
)

#this_filter = cms.EDFilter("PythiaFilterGammaJetWithBg",
#                           MaxEvents = cms.untracked.int32(2),
#                           MaxPhotonEta = cms.untracked.double(2.8),
#                           MaxPhotonPt = cms.untracked.double(22.0),
#                           MinPhotonEtaForwardJet = cms.untracked.double(1.3),
#                           moduleLabel = cms.untracked.string('source'),
#                           MinDeltaPhi = cms.untracked.double(170.0),
#                           MinPhotonPt = cms.untracked.double(18.0),
#                           MaxDeltaEta = cms.untracked.double(1.3),
#                           PhotonSeedPt = cms.untracked.double(5.0)
#)
#ProductionFilterSequence = cms.Sequence(this_filter)

