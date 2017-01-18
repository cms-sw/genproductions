import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

source = cms.Source("LHESource",
                    fileNames = cms.untracked.vstring('file:ADPS.lhe')
                    )

generator = cms.EDFilter("Pythia6HadronizerFilter",
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         maxEventsToPrint = cms.untracked.int32(0),
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         filterEfficiency = cms.untracked.double(1),
                         comEnergy = cms.double(8000.0),
                         crossSection = cms.untracked.double(1),
                         
                         PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                                     processParameters = cms.vstring('MSEL = 0        ! User defined processes',
                                                                                     'MSTJ(1)=1       ! Fragmentation/hadronization on or off',
                                                                                     'MSTP(61)=1      ! Parton showering on or off'),
                                                     parameterSets = cms.vstring('pythiaUESettings',
                                                                                 'processParameters'),
                                                     ),
                         )

ProductionFilterSequence = cms.Sequence(generator)
