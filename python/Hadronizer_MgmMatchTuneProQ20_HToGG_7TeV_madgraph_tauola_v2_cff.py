

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEProQ20Settings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(7000.0),
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
                                        'PMAS(5,1)=4.7   ! b quark mass',
                                        'PMAS(6,1)=174.3 ! t quark mass',
                                        'MSTJ(1)=1       ! Fragmentation/hadronization on or off',
                                        'MSTP(61)=1      ! Parton showering on or off',
                                        'MDME(210,1)=0    ! Higgs decay into dd',
                                        'MDME(211,1)=0    ! Higgs decay into uu',
                                        'MDME(212,1)=0    ! Higgs decay into ss',
                                        'MDME(213,1)=0    ! Higgs decay into cc',
                                        'MDME(214,1)=0    ! Higgs decay into bb',
                                        'MDME(215,1)=0    ! Higgs decay into tt',
                                        'MDME(216,1)=0    ! Higgs decay into bbbar prime',
                                        'MDME(217,1)=0    ! Higgs decay into ttbar prime',
                                        'MDME(218,1)=0    ! Higgs decay into e e',
                                        'MDME(219,1)=0    ! Higgs decay into mu mu',
                                        'MDME(220,1)=0    ! Higgs decay into tau tau',
                                        'MDME(221,1)=0    ! Higgs decay into tau tau prime',
                                        'MDME(222,1)=0    ! Higgs decay into g g',
                                        'MDME(223,1)=1    ! Higgs decay into gam gam',
                                        'MDME(224,1)=0    ! Higgs decay into gam Z',
                                        'MDME(225,1)=0    ! Higgs decay into Z Z',
                                        'MDME(226,1)=0    ! Higgs decay into W W'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
                                    'processParameters')
        ),
                         jetMatching = cms.untracked.PSet(
    MEMAIN_showerkt = cms.double(0),
    MEMAIN_maxjets = cms.int32(2),
    MEMAIN_minjets = cms.int32(0),
    MEMAIN_qcut = cms.double(20),
    MEMAIN_excres = cms.string(''),
    MEMAIN_etaclmax = cms.double(5.0),
    MEMAIN_nqmatch = cms.int32(5),
    outTree_flag = cms.int32(0),
    scheme = cms.string('Madgraph'),
    mode = cms.string('auto')
    ),
)
