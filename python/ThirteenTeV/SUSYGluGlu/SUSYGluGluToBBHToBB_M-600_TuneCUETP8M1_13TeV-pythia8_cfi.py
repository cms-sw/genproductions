import FWCore.ParameterSet.Config as cms 
from Configuration.Generator.Pythia8CommonSettings_cfi import *                                                                  
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *    

generator = cms.EDFilter("Pythia8GeneratorFilter",                                                                       
    pythiaPylistVerbosity = cms.untracked.int32(1),                                                                              
    filterEfficiency = cms.untracked.double(1.0),                                                                                
    pythiaHepMCVerbosity = cms.untracked.bool(False),                                                                            
    comEnergy = cms.double(13000.0),                                                                                             
    crossSection = cms.untracked.double(1.0),                                                                                  
    maxEventsToPrint = cms.untracked.int32(1),                                                                                   
    PythiaParameters = cms.PSet(                                                                                                 
            pythia8CommonSettingsBlock,                                                                                          
            pythia8CUEP8M1SettingsBlock, 
                    processParameters = cms.vstring(                                                                     
                                'Higgs:useBSM = on',
                                'HiggsBSM:gg2A3bbbar = on',
                                'HiggsA3:coup2d  = 20.0',
            			'HiggsA3:coup2l  = 20.0',
            			'HiggsA3:coup2u  = 0.05',
            			'HiggsA3:coup2H1Z = 0.0',
            			'HiggsA3:coup2H2Z = 1.0',
                                '36:m0 = 600',                                                                           
                                '36:onMode = off',                                                    
                                '36:onIfMatch = 5 -5',                                                                                            	         ),                                                                                                                    
        parameterSets = cms.vstring('pythia8CommonSettings',                                                                     
            'pythia8CUEP8M1Settings',                                                                                            
            'processParameters')                                                                                                 
    )                                                                                                                            
) 

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('\$Revision$'),
    name = cms.untracked.string('\$Source$'),
    annotation = cms.untracked.string('gg->bbA, 13TeV, mA = 600GeV, A->bb, TuneCUETP8M1')
)


