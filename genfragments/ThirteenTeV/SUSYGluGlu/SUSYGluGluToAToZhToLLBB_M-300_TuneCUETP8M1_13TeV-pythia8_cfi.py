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
                                        'HiggsBSM:gg2A3 = on', 
                                        '36:m0 = 300',                                                                                                                
                                        '25:m0 = 125',                                                                                                                
                                        '36:onMode = off',                                                                                                                     
                                        '36:onIfMatch = 23 25',                                                                                                                
                                        '25:onMode = off',                                                                                                                     
                                        '25:onIfMatch = 5 -5',                                                                                                               
                                        '23:onMode = off',                                                                                                                     
                                        '23:onIfAny = 13 11',                                                                                                                  
                                           
            ),                                                                                                                   
                                                                                                                                 
        parameterSets = cms.vstring('pythia8CommonSettings',                                                                     
            'pythia8CUEP8M1Settings',                                                                                            
            'processParameters')                                                                                                 
    )                                                                                                                            
) 


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('\$Revision$'),
    name = cms.untracked.string('\$Source$'),
    annotation = cms.untracked.string('ggA, A->Zh->llbb 13TeV, mA = 300GeV, TuneCUETP8M1')
)
