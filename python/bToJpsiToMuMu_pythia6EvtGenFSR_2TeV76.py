import FWCore.ParameterSet.Config as cms
from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                        pythiaPylistVerbosity = cms.untracked.int32(0),
                        pythiaHepMCVerbosity  = cms.untracked.bool(False),
                        comEnergy             = cms.double(2760.0),
                        crossSection          = cms.untracked.double(29800000.0),
                        filterEfficiency      = cms.untracked.double(0.000298),
                        maxEventsToPrint      = cms.untracked.int32(0),
                        ExternalDecays        = cms.PSet(
        EvtGen = cms.untracked.PSet(operates_on_particles  = cms.vint32( 0 ), # 0 (zero) means default list (hardcoded)
                                    use_default_decay      = cms.untracked.bool(False),
                                    decay_table            = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY_NOLONGLIFE.DEC'),
                                    particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
                                    user_decay_file        = cms.FileInPath('GeneratorInterface/ExternalDecays/data/incl_BtoJpsi_mumu.dec'),
                                    list_forced_decays     = cms.vstring('MyB0', 
                                                                         'Myanti-B0',
                                                                         'MyB+',
                                                                         'MyB-',
                                                                         'MyB_s0', 
                                                                         'Myanti-B_s0'),
                                    ),
        parameterSets = cms.vstring('EvtGen')
        ),
                         PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                                     bbbarSettings = cms.vstring('MSEL = 1'), # dijet process
                                                     # This is a vector of ParameterSet names to be read, in this order
                                                     parameterSets = cms.vstring('pythiaUESettings',
                                                                                 'bbbarSettings')
                                                     )
                         )

ProductionFilterSequence = cms.Sequence(generator)
