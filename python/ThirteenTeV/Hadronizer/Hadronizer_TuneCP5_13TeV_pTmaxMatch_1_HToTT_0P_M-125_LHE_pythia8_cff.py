import math
import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    UseExternalGenerators = cms.untracked.bool(True),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            UseTauolaPolarization = cms.bool(True),
            InputCards = cms.PSet(
                mdtau = cms.int32(0),
                pjak2 = cms.int32(0),
                pjak1 = cms.int32(0)
            ),
            parameterSets = cms.vstring('setHiggsScalarPseudoscalarPDG', 'setHiggsScalarPseudoscalarMixingAngle'),
            setHiggsScalarPseudoscalarPDG = cms.int32(25),
            setHiggsScalarPseudoscalarMixingAngle = cms.double(0), #this is for SM decay
                                                                   #change to math.pi/2 for pure pseudoscalar
        ),
        parameterSets = cms.vstring('Tauola'),
    ),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        processParameters = cms.vstring(
            'SpaceShower:pTmaxMatch = 1',
            'TimeShower:pTmaxMatch = 1',
            'SpaceShower:pTmaxFudge = 1',
            'TimeShower:pTmaxFudge = 1',
            '25:onMode = off',       # turn OFF all H decays
            '25:onIfMatch = 15 -15', # turn ON H->tautau
            '25:m0 = 125.0',

            #PSweights
            'UncertaintyBands:doVariations = on',
            # 3 sets of variations for ISR&FSR up/down
            # Reduced sqrt(2)/(1/sqrt(2)), Default 2/0.5 and Conservative 4/0.25 variations
            'UncertaintyBands:List = {' +
            'isrRedHi isr:muRfac=0.707,fsrRedHi fsr:muRfac=0.707,isrRedLo isr:muRfac=1.414,fsrRedLo fsr:muRfac=1.414,' +
            'isrDefHi isr:muRfac=0.5, fsrDefHi fsr:muRfac=0.5,isrDefLo isr:muRfac=2.0,fsrDefLo fsr:muRfac=2.0,' +
            'isrConHi isr:muRfac=0.25, fsrConHi fsr:muRfac=0.25,isrConLo isr:muRfac=4.0,fsrConLo fsr:muRfac=4.0}',

            'UncertaintyBands:MPIshowers = on',
            'UncertaintyBands:overSampleFSR = 10.0',
            'UncertaintyBands:overSampleISR = 10.0',
            'UncertaintyBands:FSRpTmin2Fac = 20',
            'UncertaintyBands:ISRpTmin2Fac = 1',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'processParameters',
                                    )
    )
)

