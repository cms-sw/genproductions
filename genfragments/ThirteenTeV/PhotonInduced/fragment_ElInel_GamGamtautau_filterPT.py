import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/afs/cern.ch/user/m/mpitt/public/exclusive_tautau/gridpacks/CEP_tautau_reweight_slc7_amd64_gcc700_CMSSW_10_6_19_tarball.tar.xz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    HepMCFilter = cms.PSet(
        filterName = cms.string('EmbeddingHepMCFilter'),
        filterParameters = cms.PSet(
            ElElCut = cms.string('El1.Pt > 22 && El2.Pt > 10 && El1.Eta < 2.6 && El2.Eta < 2.6'),
            ElHadCut = cms.string('El.Pt > 22 && Had.Pt > 16 && El.Eta < 2.6 && Had.Eta < 2.7'),
            ElMuCut = cms.string('Mu.Pt > 7 && El.Pt > 11 && El.Eta < 2.6 && Mu.Eta < 2.5'),
            HadHadCut = cms.string('Had1.Pt > 35 && Had2.Pt > 35 && Had1.Eta < 2.5 && Had2.Eta < 2.5'),
            MuHadCut = cms.string('Mu.Pt > 19 && Had.Pt > 16 && Mu.Eta < 2.5 && Had.Eta < 2.7'),
            MuMuCut = cms.string('Mu1.Pt > 17 && Mu2.Pt > 8 && Mu1.Eta < 2.5 && Mu2.Eta < 2.5'),
            Final_States = cms.vstring(             
                'ElHad',
                'ElEl',
                'MuMu',
                'ElMu',
                'HadHad',
                'MuHad'
            ),                                            
            BosonPDGID = cms.int32(22)
        )
    ),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        semiexclusive_process = cms.vstring( # recomendation from https://superchic.hepforge.org/superchic4.pdf
          'LesHouches:matchInOut = off',
          'BeamRemnants:primordialKT = off',
          'PartonLevel:MPI = off',
          'PartonLevel:FSR = on',
          'SpaceShower:dipoleRecoil = on',
          'SpaceShower:pTmaxMatch = 2',
          'SpaceShower:QEDshowerByQ = off',
          'SpaceShower:pTdampMatch = 1',
          'BeamRemnants:unresolvedHadron = 1',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings','pythia8CP5Settings','pythia8PSweightsSettings','semiexclusive_process')      
    ),
    comEnergy = cms.double(13000.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1)
)

ProductionFilterSequence = cms.Sequence(generator)

