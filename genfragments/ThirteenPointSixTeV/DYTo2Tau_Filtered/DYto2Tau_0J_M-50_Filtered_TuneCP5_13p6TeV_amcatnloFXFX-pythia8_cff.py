import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/el8_amd64_gcc10/13p6TeV/amcatnlo/V5_2.9.18/DYToTauTau_NLO/DY0jToTauTau_NLO_5f_el8_amd64_gcc10_CMSSW_12_4_8_tarball.tar.xz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh'),
    generateConcurrently = cms.untracked.bool(False),
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8aMCatNLOSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentHadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13600.),
    nAttempts = cms.uint32(1),
    HepMCFilter = cms.PSet(
        filterName = cms.string('EmbeddingHepMCFilter'),
        filterParameters = cms.PSet(
            ElElCut = cms.string('El1.Pt > 22 && El2.Pt > 10 && El1.Eta < 3.0 && El2.Eta < 3.0'),
            ElHadCut = cms.string('El.Pt > 22 && Had.Pt > 16 && El.Eta < 3.0 && Had.Eta < 3.0'),
            ElMuCut = cms.string('Mu.Pt > 8 && El.Pt > 11 && El.Eta < 3.0 && Mu.Eta < 3.0'),
            HadHadCut = cms.string('Had1.Pt > 20 && Had2.Pt > 20 && Had1.Eta < 3.0 && Had2.Eta < 3.0'),
            MuHadCut = cms.string('Mu.Pt > 19 && Had.Pt > 16 && Mu.Eta < 3.0 && Had.Eta < 3.0'),
            MuMuCut = cms.string('Mu1.Pt > 16 && Mu2.Pt > 8 && Mu1.Eta < 3.0 && Mu2.Eta < 3.0'),
            Final_States = cms.vstring(
                'ElHad',
                'ElMu',
                'HadHad',
                'MuHad'
            ),
            BosonPDGID = cms.int32(23),
            IncludeDY = cms.bool(True) # allow virtual bosons
                             )
                         ),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8aMCatNLOSettingsBlock,
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            'JetMatching:setMad = off',
            'JetMatching:scheme = 1',
            'JetMatching:merge = on',
            'JetMatching:jetAlgorithm = 2',
            'JetMatching:etaJetMax = 999.',
            'JetMatching:coneRadius = 1.',
            'JetMatching:slowJetPower = 1',
            'JetMatching:doFxFx = on',
            'JetMatching:qCut = 30.',
            'JetMatching:qCutME = 10',
            'JetMatching:nJetMax = 2',
            'TimeShower:mMaxGamma = 4.0',
            'JetMatching:nQmatch = 5',
            'BeamRemnants:primordialKThard=2.48',
		),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CP5Settings',
            'pythia8aMCatNLOSettings',
            'processParameters',
            'pythia8PSweightsSettings',
        )
    )
)

