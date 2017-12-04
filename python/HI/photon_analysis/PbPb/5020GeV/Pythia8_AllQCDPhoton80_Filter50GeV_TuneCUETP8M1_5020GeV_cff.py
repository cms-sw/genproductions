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
                                        'PromptPhoton:all = on',
                                        'PhaseSpace:pTHatMin = 80.',
                                        'PhaseSpace:pTHatMax = 9999.'),
    ),
    comEnergy = cms.double(5020.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0)
)

configurationMetadata = cms.untracked.PSet(
    annotation = cms.untracked.string('PYTHIA 8 (unquenched) photons in NN (pt-hat > 80 GeV) at sqrt(s) = 5.02 TeV')
    )

photonFilter = cms.EDFilter("PythiaFilterMultiMother",
                            Status = cms.untracked.int32(1),
                            MinPt = cms.untracked.double(50.0),
                            ParticleID = cms.untracked.int32(22),
                            MotherIDs = cms.untracked.vint32(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,-22,-21,-20,-19,-18,-17,-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1)
)


ProductionFilterSequence = cms.Sequence(generator*photonFilter)
