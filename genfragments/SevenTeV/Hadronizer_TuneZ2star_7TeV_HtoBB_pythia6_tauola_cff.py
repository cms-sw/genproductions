import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
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
        processParameters = cms.vstring('MSEL=0          ! User defined processes',
                                        'PMAS(5,1)=4.75  ! b quark mass',
                                        'PMAS(6,1)=172.5 ! t quark mass',
                                        
                                        'MDME(210,1)=0 ! Higgs decay into dd',
                                        'MDME(211,1)=0 ! Higgs decay into uu',
                                        'MDME(212,1)=0 ! Higgs decay into ss',
                                        'MDME(213,1)=0 ! Higgs decay into cc',
                                        'MDME(214,1)=1 ! Higgs decay into bb',
                                        'MDME(215,1)=0 ! Higgs decay into tt',
                                        'MDME(216,1)=0 ! Higgs decay into bbbar prime',
                                        'MDME(217,1)=0 ! Higgs decay into ttbar prime',
                                        'MDME(218,1)=0 ! Higgs decay into e e',
                                        'MDME(219,1)=0 ! Higgs decay into mu mu',
                                        'MDME(220,1)=0 ! Higgs decay into tau tau',
                                        'MDME(221,1)=0 ! Higgs decay into tau tau prime',
                                        'MDME(222,1)=0 ! Higgs decay into g g',
                                        'MDME(223,1)=0 ! Higgs decay into gam gam',
                                        'MDME(224,1)=0 ! Higgs decay into gam Z',
                                        'MDME(225,1)=0 ! Higgs decay into Z Z',
                                        'MDME(226,1)=0 ! Higgs decay into W W'
                                        ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
                                    'processParameters'
                                    )
        )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.0 $'),
    name = cms.untracked.string ('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/Hadronizer_TuneZ2star_7TeV_HtoBB_pythia6_tauola_cff.py,v $'),
    annotation = cms.untracked.string('runs Z2star Pythia6 with Higgs -> bb  at 7 TeV')
)

