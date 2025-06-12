import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from GeneratorInterface.EvtGenInterface.EvtGenSetting_cff import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

_generator = cms.EDFilter("Pythia8GeneratorFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13600.),
    ExternalDecays = cms.PSet(
        EvtGen130 = cms.untracked.PSet(
            decay_table = cms.string('GeneratorInterface/EvtGenInterface/data/DECAY_2014_NOLONGLIFE.DEC'),
            particle_property_file = cms.FileInPath('GeneratorInterface/EvtGenInterface/data/evt_2014_bsmass_3p6gev.pdl'),
            list_forced_decays = cms.vstring('MyB_s0', 'Myanti-B_s0'),        # will force one at the time, notice just the parent
            operates_on_particles = cms.vint32(531,-531),               # we care just about our signal particles
            convertPythiaCodes = cms.untracked.bool(False),
            user_decay_embedded= cms.vstring(
                'Alias      MyB_s0   B_s0',
                'Alias      Myanti-B_s0   anti-B_s0',
                'ChargeConj      Myanti-B_s0      MyB_s0',
                'Decay MyB_s0',
                '1.000      tau+  tau-               PHOTOS SLL;',
                'Enddecay',
                'CDecay Myanti-B_s0',
                'End',
            ),
        ),
        parameterSets = cms.vstring('EvtGen130'),
    ),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            'Top:all = off',
            'Top:gg2ttbar = on',
            'Top:qqbar2ttbar = on',
            '531:m0 = 3.6',  # override Bs mass in Pythia
            '533:m0 = 3.6326', # override Bs* mass in Pythia
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'pythia8PSweightsSettings',
                                    'processParameters',
                                    )
    )
)
from GeneratorInterface.Core.ExternalGeneratorFilter import ExternalGeneratorFilter
generator = ExternalGeneratorFilter(_generator)
generator.PythiaParameters.processParameters.extend(EvtGenExtraParticles)

bFilter = cms.EDFilter("PythiaFilter",
    MaxEta = cms.untracked.double(9999.),
    MinEta = cms.untracked.double(-9999.),
    ParticleID = cms.untracked.int32(531)  #Bs
)

maindecayfilter = cms.EDFilter(
    "PythiaDauVFilter",
    verbose         = cms.untracked.int32(1),
    NumberDaughters = cms.untracked.int32(2),
    ParticleID      = cms.untracked.int32(531),
    DaughterIDs     = cms.untracked.vint32(15,-15),
    MinPt           = cms.untracked.vdouble(2.5, 2.5),
    MaxEta          = cms.untracked.vdouble(2.5, 2.5),
    MinEta          = cms.untracked.vdouble(-2.5, -2.5)
)

ProductionFilterSequence = cms.Sequence(generator * bFilter * maindecayfilter)
