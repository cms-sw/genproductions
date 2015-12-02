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
            'PhaseSpace:pTHatMin = 460.', 
            'PhaseSpace:pTHatMax = 9999.'),
    ),
    comEnergy = cms.double(5020.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0)
)

configurationMetadata = cms.untracked.PSet(
    annotation = cms.untracked.string('PYTHIA 8 (unquenched) dijets in NN (pt-hat > 460 GeV) at sqrt(s) = 5.02 TeV')
    )

bfilter = cms.EDFilter("MCSingleParticleFilter",
                       MaxEta     = cms.untracked.vdouble(3.0, 3.0),
                       MinEta     = cms.untracked.vdouble(-3.0, -3.0),
                       MinPt      = cms.untracked.vdouble(0.0, 0.0),
                       ParticleID = cms.untracked.vint32(5, -5)
                       )

ProductionFilterSequence = cms.Sequence(generator*bfilter)


