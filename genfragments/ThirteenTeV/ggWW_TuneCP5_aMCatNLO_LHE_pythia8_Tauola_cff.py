import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
                                     args = cms.vstring('/afs/cern.ch/work/s/sblancof/public/WW_helicity-Gridpacks/SMP_Polarization/Run2-Gridpacks/ggWW_LL_slc7_amd64_gcc700_CMSSW_10_6_19_tarball.tar.xz'),
                                     generateConcurrently = cms.untracked.bool(True),
                                     inputFile = cms.string('unweight_events.lhe'),
                                     nEvents = cms.untracked.uint32(500000),
                                     numberOfParameters = cms.uint32(1),
                                     outputFile = cms.string('cmsgrid_final.lhe'),
                                     scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)


from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         ExternalDecays = cms.PSet(
                             Tauola = cms.untracked.PSet(
                                 TauolaPolar,
                                 TauolaDefaultInputCards
                             ),
                             parameterSets = cms.vstring('Tauola')
                         ),
                         UseExternalGenerators = cms.untracked.bool(True),
                    
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
                             pythia8CommonSettingsBlock,
                             pythia8CP5SettingsBlock,
                             pythia8PSweightsSettingsBlock,
                             
                             processParameters = cms.vstring(
                                 'Main:timesAllowErrors    = 10000',
                                 'SLHA:useDecayTable = on',
                                 '24:mMin = 0.05',                  #                                                                                                                                                                                                          
                                 '24:onMode = off',                 # turn OFF all W decays                                                                                                                                                                                    
                                 '24:onIfAny = 11 13 15 12 14 16',   # turn ON W->lnu   
                                 'ParticleDecays:limitTau0 = on',
                                 'ParticleDecays:tauMax = 10'),

                             parameterSets = cms.vstring('pythia8CommonSettings',
                                                         'pythia8CP5Settings',
                                                         'pythia8aMCatNLOSettings',
                                                         'processParameters',
                                                     )
                        )
)

ProductionFilterSequence = cms.Sequence(generator)

