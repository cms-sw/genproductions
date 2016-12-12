import FWCore.ParameterSet.Config as cms 


from GeneratorInterface.ExternalDecays.TauolaSettings_cff import * 
from Configuration.Generator.Pythia8CommonSettings_cfi import * 
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *  


generator = cms.EDFilter("Pythia8GeneratorFilter", 
                         comEnergy = cms.double(13000.0), 
                          filterEfficiency = cms.untracked.double(1.), 
                          maxEventsToPrint = cms.untracked.int32(1), 
                          pythiaHepMCVerbosity = cms.untracked.bool(False), 
                          pythiaPylistVerbosity = cms.untracked.int32(1), 
                          SLHAFileForPythia8 = cms.string('./RPVStop_M_500_LDQ333.slha'), 
                          ExternalDecays = cms.PSet(Tauola = cms.untracked.PSet(TauolaPolar, TauolaDefaultInputCards ), 
                                                    parameterSets = cms.vstring('Tauola') 
                                                    ), 
                          PythiaParameters = cms.PSet(         
         pythia8CommonSettingsBlock, 
         pythia8CUEP8M1SettingsBlock, 
         processParameters = cms.vstring( 
             'SUSY:all off',
             'SUSY:gg2squarkantisquark = on',
             'SUSY:qqbar2squarkantisquark = on',
             'SUSY:idA = 1000006',
             'SUSY:idB = 1000006',
             ), 
         parameterSets = cms.vstring( 
             'pythia8CommonSettings', 
             'pythia8CUEP8M1Settings', 
             'processParameters')        
         ) 
                          ) 
