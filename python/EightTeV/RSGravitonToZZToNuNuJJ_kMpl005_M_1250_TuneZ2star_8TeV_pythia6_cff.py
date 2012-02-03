import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         filterEfficiency = cms.untracked.double(0.28),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(8000.0),
                         maxEventsToPrint = cms.untracked.int32(0),
                         crossSection  = cms.untracked.double(3.752E-03),
                         PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                                     processParameters = cms.vstring('PMAS(5,1)= 4.8         ! b quark mass',
                                                                                     'PMAS(6,1)= 175.0       ! t quark mass',
                                                                                     'PMAS(347,1)= 1250.0     ! mass of RS Graviton',
                                                                                     'PARP(50) = 0.27        ! 0.054 == c=0.01 (k/M_PL=0.01)',
                                                                                     'MSEL=0                 ! (D=1) 0 to select full user control',
                                                                                     'MSUB(391)=1               ! q qbar -> G* ',
                                                                                     'MSUB(392)=1               ! g g -> G*',
                                                                                     '5000039:ALLOFF            ! Turn off all decays of G*',
                                                                                     '5000039:ONIFANY 23        ! Turn on the decays ZZ'),
                                                     parameterSets = cms.vstring('pythiaUESettings',
                                                                                 'processParameters')
                                                     )
                         )

nunujjFilter  = cms.EDFilter("TwoVBGenFilter",
    src      = cms.untracked.InputTag("generator"),
    eejj     = cms.bool(False),
    enujj    = cms.bool(False),
    mumujj   = cms.bool(False),
    munujj   = cms.bool(False),
    tautaujj = cms.bool(False),
    taunujj  = cms.bool(False),
    nunujj   = cms.bool(True)
)

configurationMetadata = cms.untracked.PSet(
        version = cms.untracked.string('$Revision: 1.1 $'),
        name = cms.untracked.string('$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/RSGravitonToZZToNuNuJJ_kMpl005_M_1250_pythia6_cff.py,v $'),
        annotation = cms.untracked.string('default documentation string for PYTHIA6_EXOTICA_RSGravZZ_kMpl005_M1250_7TeV_nunujj_cff.py')
)

ProductionFilterSequence = cms.Sequence(generator*nunujjFilter)
