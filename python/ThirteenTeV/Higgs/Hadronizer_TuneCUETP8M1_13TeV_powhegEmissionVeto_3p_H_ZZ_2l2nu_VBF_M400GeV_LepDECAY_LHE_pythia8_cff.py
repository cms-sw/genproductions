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
            'POWHEG:nFinal = 3',              ## Number of final state particles
                                              ## (BEFORE THE DECAYS) in the LHE
                                              ## other than emitted extra parton
            '23:mMin = 0.05',                 # Solve problem with mZ cut
            '25:m0 = 400.0',                  # Higgs Mass
            '25:onMode = off',                # turn OFF all H decays
            '25:onIfMatch = 23 23',           # turn ON H->ZZ
            '23:onMode = off',                # turn OFF all Z decays
            '23:onIfAny = 11 13 15 12 14 16', # turn ON Z->ll
          ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'pythia8PowhegEmissionVetoSettings',
                                    'processParameters'
                                    )
        )
                         )
#Filters to have exactly H To ZZ_2l2nu events
#Filter to select 2 leptons
VisLep = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(0, 0),
    MinDeltaPhi = cms.untracked.double(0.0),
    MaxDeltaPhi = cms.untracked.double(6.29),
    MinPt = cms.untracked.vdouble(5.0, 5.0),
    MinP = cms.untracked.vdouble(0.0, 0.0),
    MaxEta = cms.untracked.vdouble(1000, 1000),
    MinEta = cms.untracked.vdouble(-1000, -1000),
    ParticleCharge = cms.untracked.int32(-1),
    MaxInvMass = cms.untracked.double(1000.0),
    MinInvMass = cms.untracked.double(40.0),
    ParticleID1 = cms.untracked.vint32(11, 13, 15),
    ParticleID2 = cms.untracked.vint32(11, 13, 15)
)

#Filter to select 2 neutrinos
InVisLep = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinDeltaPhi = cms.untracked.double(0.0),
    MaxDeltaPhi = cms.untracked.double(6.29),
    MinPt = cms.untracked.vdouble(5.0, 5.0),
    MinP = cms.untracked.vdouble(0.0, 0.0),
    MaxEta = cms.untracked.vdouble(1000, 1000),
    MinEta = cms.untracked.vdouble(-1000, -1000),
    ParticleCharge = cms.untracked.int32(-1),
    MaxInvMass = cms.untracked.double(1000.0),
    MinInvMass = cms.untracked.double(40.0),
    ParticleID1 = cms.untracked.vint32(12, 14, 16),
    ParticleID2 = cms.untracked.vint32(12, 14, 16)
)

ProductionFilterSequence = cms.Sequence(generator*VisLep*InVisLep)
