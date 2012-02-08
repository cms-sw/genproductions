import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(8000.),
                         crossSection = cms.untracked.double(2.05e-3),
                         PythiaParameters = cms.PSet(
    pythia8_monophoton = cms.vstring( ## see details on http://home.thep.lu.se/~torbjorn/php8135/ExtraDimensionalProcesses.php?filepath=files/
    'Tune:pp 5',
    'Tune:ee 3',
    'ExtraDimensionsLED:ffbar2Ggamma = on',
    'ExtraDimensionsLED:CutOffmode = 1',
    'ExtraDimensionsLED:t = 0.5',
    'ExtraDimensionsLED:n = 3',
    'ExtraDimensionsLED:MD = 3000.',
    'ExtraDimensionsLED:LambdaT = 3000.',
    '5000039:m0 = 1200.',
    '5000039:mWidth = 1000.',
    '5000039:mMin = 1.',
    '5000039:mMax = 13990.',
    'PhaseSpace:pTHatMin = 125.',
    'PartonLevel:MI = on',
    'PartonLevel:ISR = on',
    'PartonLevel:FSR = on'					       
    ), 
    parameterSets = cms.vstring('pythia8_monophoton')
    ) 
                         )



        
