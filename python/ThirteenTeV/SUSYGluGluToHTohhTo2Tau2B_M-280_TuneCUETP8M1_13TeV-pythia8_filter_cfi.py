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
              'HiggsBSM:gg2H2 = on',                                                                                                                                             
            '35:m0 = 280',                                                                                                                                                     
            '25:m0 = 125',                                                                                                                                                     
            '35:onMode = off',                                                                                                                                                 
            '35:onIfMatch = 25 25',                                                                                                                                            
            '25:onMode = off',                                                                                                                                                 
            '25:onIfMatch = 5 -5',                                                                                                                                             
            '25:onIfMatch = 15 -15'
            ),                                                                                                                                          
                                                                                                                               
        parameterSets = cms.vstring('pythia8CommonSettings',                                                                     
            'pythia8CUEP8M1Settings',                                                                                            
            'processParameters')                                                                                                 
    )                                                                                                                            
)


bbgenfilter = cms.EDFilter("MCMultiParticleFilter",
                           Status = cms.vint32(23, 23),
                           src = cms.InputTag('generator'),
                           ParticleID = cms.vint32(5, -5),
                           PtMin = cms.vdouble(0, 0),
                           NumRequired = cms.int32(1),
                           EtaMax = cms.vdouble(9999, 9999),
                           AcceptMore = cms.bool(True)
)

tautaugenfilter = cms.EDFilter("MCMultiParticleFilter",
                               Status = cms.vint32(23, 23),
                               src = cms.InputTag('generator'),
                               ParticleID = cms.vint32(15, -15),
                               PtMin = cms.vdouble(0,0),
                               NumRequired = cms.int32(1),
                               EtaMax = cms.vdouble(9999, 9999),
                               AcceptMore = cms.bool(True)
)

ProductionFilterSequence = cms.Sequence(generator + bbgenfilter + tautaugenfilter)


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('\$Revision$'),
    name = cms.untracked.string('\$Source$'),
    annotation = cms.untracked.string('ggH (H->hh->tautaubb), 13TeV, mH = 280GeV, filtered. TuneCUETP8M1')
)
