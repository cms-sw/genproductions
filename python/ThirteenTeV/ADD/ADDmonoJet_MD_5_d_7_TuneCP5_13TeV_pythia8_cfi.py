import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter('Pythia8GeneratorFilter',
    comEnergy = cms.double(13000.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    #SLHATableForPythia8 = cms.string(SLHA_TABLE),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        pdfSettings = cms.vstring(
            'PDF:pSet = LHAPDF6:NNPDF31_lo_as_0130',
            ),
        processParameters = cms.vstring(
            'ExtraDimensionsLED:monojet = on',
            'ExtraDimensionsLED:CutOffmode = 1',
            'ExtraDimensionsLED:t = 0.5',
            'ExtraDimensionsLED:n = 7',
            'ExtraDimensionsLED:MD = 5000.',
            '5000039:m0 = 1200.',
            '5000039:mWidth = 1000.',
            '5000039:mMin = 1.',
            '5000039:mMax = 13990.',
            'PhaseSpace:pTHatMin = 80.',
            'PartonLevel:ISR = on',
            'PartonLevel:FSR = on',
            'ParticleDecays:limitTau0 = on',
            'ParticleDecays:tauMax = 10'
            ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CP5Settings',
            'pdfSettings',
            'pythia8PSweightsSettings',
            'processParameters',
            ),
        ),
    )
