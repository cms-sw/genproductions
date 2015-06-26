import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        pythia8PowhegEmissionVetoSettingsBlock,
        processParameters = cms.vstring(
            'POWHEG:nFinal = 2',   ## Number of final state particles
                                   ## (BEFORE THE DECAYS) in the LHE
                                   ## other than emitted extra parton
          ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'pythia8PowhegEmissionVetoSettings',
                                    'processParameters'
                                    )
        )
                         )


llgenfilter = cms.EDFilter("MCParticlePairFilter",
                           Status = cms.untracked.vint32(1, 1),
                           MinPt = cms.untracked.vdouble(0., 0.),
                           #ParticleCharge = cms.untracked.int32(0),
                           ParticleID1 = cms.untracked.vint32(11),
                           ParticleID2 = cms.untracked.vint32(11),
                           MinInvMass =  cms.untracked.double(800.),
                           MaxInvMass =  cms.untracked.double(1200.)
                           )

ProductionFilterSequence = cms.Sequence(generator*llgenfilter)
