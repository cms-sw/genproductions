import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        parameterSets = cms.vstring('pythia8CommonSettings',
            'pythia8CP5Settings',
            'processParameters'),
        processParameters = cms.vstring('HardQCD:all = on',
                                        'PhaseSpace:pTHatMin = 50.',
                                        'PhaseSpace:pTHatMax = 9999.'),
    ),
    comEnergy = cms.double(5020.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0)
)

configurationMetadata = cms.untracked.PSet(
    annotation = cms.untracked.string('PYTHIA 8, Tune CP5, (unquenched) EM-enriched Dijets in NN (pt-hat > 50 GeV) at sqrt(s) = 5.02 TeV')
    )

partonFilter = cms.EDFilter("MCSingleParticleFilter",
                            ParticleID = cms.untracked.vint32(1, 2, 3, 4, 5, 6, #quarks
                                                              21, 22), #gluon, photon
                            Status = cms.untracked.vint32(2, 2, 2, 2, 2, 2,
                                                          2, 1)
                            )
neutralMesonFilter = cms.EDFilter("MCSingleParticleFilter",
                                  ParticleID = cms.untracked.vint32(111, #pi0
                                                                    221, #eta
                                                                    331, #eta'
                                                                    223), #omega
                                  Status = cms.untracked.vint32(2, #pi0
                                                                2, #eta
                                                                2, #eta'
                                                                2), #omega
                                  MinEta = cms.untracked.vdouble(-3,
                                                                 -3,
                                                                 -3,
                                                                 -3),
                                  MaxEta = cms.untracked.vdouble(3,
                                                                 3,
                                                                 3,
                                                                 3),
                                  MinPt = cms.untracked.vdouble(35,
                                                                35,
                                                                35,
                                                                35)

                                  )


ProductionFilterSequence = cms.Sequence(generator*partonFilter*neutralMesonFilter)
