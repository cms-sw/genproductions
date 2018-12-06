import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
   	pythia8CP5SettingsBlock,
        processParameters = cms.vstring('HardQCD:all = on', 
                                        'PhaseSpace:pTHatMin = 15.', 
                                        'PhaseSpace:pTHatMax = 500.',#Upper bound since we will do weighted pthat event-by-event
                                        'PhaseSpace:bias2Selection = on',#Bias to selection, standard as JERC
                                        'PhaseSpace:bias2SelectionPow = 4.5',
                                        'PhaseSpace:bias2SelectionRef = 15.'),
        parameterSets = cms.vstring('pythia8CommonSettings', 
                                    'pythia8CP5Settings', 
                                    'processParameters'),
        
        ),
        comEnergy = cms.double(5020.0),
        filterEfficiency = cms.untracked.double(1.0),
        maxEventsToPrint = cms.untracked.int32(0),
        pythiaHepMCVerbosity = cms.untracked.bool(False),
        pythiaPylistVerbosity = cms.untracked.int32(0)                         
)

bfilter = cms.EDFilter("MCSingleParticleFilter",
                       MaxEta     = cms.untracked.vdouble(3.0, 3.0),
                       MinEta     = cms.untracked.vdouble(-3.0, -3.0),
                       MinPt      = cms.untracked.vdouble(0.0, 0.0),
                       ParticleID = cms.untracked.vint32(511,513,521,523,531,533,535,541,543,-511,-513,-521,-523,-531,-533,-535,-541,-543,551,553,555,557,-551,-553,-555,-557, 5122,5112,5212,5212,5222,5114,5214,5224,5132,5232,5312,5322,5314,5324,5332,5334,5142,5242,5412,5422,5414,5424,5342,5432,5434,5442,5444,5512,5522,5514,5524,5532,5534,5542,5544,5554,-5122,-5112,-5212,-5212,-5222,-5114,-5214,-5224,-5132,-5232,-5312,-5322,-5314,-5324,-5332,-5334,-5142,-5242,-5412,-5422,-5414,-5424,-5342,-5432,-5434,-5442,-5444,-5512,-5522,-5514,-5524,-5532,-5534,-5542,-5544,-5554, 100533,200533,-100533,-200533)
)

ProductionFilterSequence = cms.Sequence(generator*bfilter)
