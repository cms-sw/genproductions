
import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
   comEnergy = cms.double(13000.0),
   crossSection = cms.untracked.double(0.015),
   filterEfficiency = cms.untracked.double(1),
   maxEventsToPrint = cms.untracked.int32(0),
   pythiaHepMCVerbosity = cms.untracked.bool(False),
   pythiaPylistVerbosity = cms.untracked.int32(1),
   PythiaParameters = cms.PSet(
      pythia8CommonSettingsBlock,
      pythia8CUEP8M1SettingsBlock,
      processParameters = cms.vstring(
         'PhotonCollision:gmgm2ee = on',
         'PhaseSpace:mHatMin = 500.',
         'PhaseSpace:mHatMax = 6500.',
         'PDF:useHard on',
         'PDF:pHardSet = LHAPDF6:MRST2004qed.LHgrid',
         'PDF:pSet = LHAPDF6:NNPDF23_lo_as_0130_qed'
      ),
      parameterSets = cms.vstring('pythia8CommonSettings',
                                  'pythia8CUEP8M1Settings',
                                  'processParameters',
                                 )
   )
)

ProductionFilterSequence = cms.Sequence(generator)
