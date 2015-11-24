import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        parameterSets = cms.vstring('pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
            'processParameters'),
        processParameters = cms.vstring('HardQCD:all = on',
                                        'PhaseSpace:pTHatMin = 30.',
                                        'PhaseSpace:pTHatMax = 9999.'),
    ),
    comEnergy = cms.double(5020.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0)
)

configurationMetadata = cms.untracked.PSet(
    annotation = cms.untracked.string('PYTHIA 8 (unquenched) EM-enriched Dijets in NN (pt-hat > 30 GeV) at sqrt(s) = 5.02 TeV')
    )

photonFilter = cms.EDFilter("HINEcalGenEvtSelector",
                            partons = cms.vint32(1, 2, 3, 4, 5, 6, #quarks
                                                 21, 22), #gluon, photon
                            partonPt = cms.vdouble(0, 0, 0, 0, 0, 0,
                                                   0, 0),
                            partonStatus = cms.vint32(2, 2, 2, 2, 2, 2,
                                                      2, 1),
                            particles = cms.vint32(221, #eta
                                                   331, #eta'
                                                   223, #omega
                                                   111), #pi0
                            particlePt = cms.vdouble(35, 35, 35, 35),
                            particleStatus = cms.vint32(2, #eta
                                                        2, #eta'
                                                        2, #omega
                                                        2), #pi0
                            etaMax = cms.double(3.0),   # Photon eta cut
)


ProductionFilterSequence = cms.Sequence(generator*photonFilter)
