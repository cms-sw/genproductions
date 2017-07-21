import FWCore.ParameterSet.Config as cms 
from Configuration.Generator.Pythia8CommonSettings_cfi import *                                                                  
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *    

generator = cms.EDFilter("Pythia8GeneratorFilter",                                                                       
    pythiaPylistVerbosity = cms.untracked.int32(1),                                                                              
    filterEfficiency = cms.untracked.double(1.0),                                                                                
    pythiaHepMCVerbosity = cms.untracked.bool(False),                                                                            
    comEnergy = cms.double(13000.0),                                                                                             
    crossSection = cms.untracked.double(518.3),                                                                                  
    maxEventsToPrint = cms.untracked.int32(1),                                                                                   
    PythiaParameters = cms.PSet(                                                                                                 
            pythia8CommonSettingsBlock,                                                                                          
            pythia8CUEP8M1SettingsBlock, 
                    processParameters = cms.vstring('Higgs:useBSM = on',                                                                     
                                        'HiggsBSM:gg2A3bbbar = on',                                                                   
                                       '36:m0 = 45',                                                                           
                                        '36:onMode = off',                                                                       
                                        '36:onIfMatch = 15 -15'                                                                        
                                                                                                                                 
            ),                                                                                                                   
                                                                                                                                 
        parameterSets = cms.vstring('pythia8CommonSettings',                                                                     
            'pythia8CUEP8M1Settings',                                                                                            
            'processParameters')                                                                                                 
    )                                                                                                                            
) 


muelegenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
                           MinPt = cms.untracked.vdouble(15.,15.,15.,15.),
                           MinEta = cms.untracked.vdouble(-10,-10,-10,-10),
                           MaxEta = cms.untracked.vdouble(10,10,10,10),
                           ParticleID = cms.untracked.vint32(13,-13,11,-11),
                           Status = cms.untracked.vint32(1,1,1,1),
                           # Decay cuts are in mm
                           MaxDecayRadius = cms.untracked.vdouble(2000.,2000.,2000.,2000.),
                           MinDecayZ = cms.untracked.vdouble(-4000.,-4000.,-4000.,-4000.),
                           MaxDecayZ = cms.untracked.vdouble(4000.,4000.,4000.,4000.)
)

ProductionFilterSequence = cms.Sequence(generator + muelegenfilter)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('\$Revision$'),
    name = cms.untracked.string('\$Source$'),
    annotation = cms.untracked.string('low mass gg->bba1, 13TeV, mA = 45GeV, a1->tautau, filtered. TuneCUETP8M1')
)

