FLAVOR = 'stau'
COM_ENERGY = 13000. # GeV
MASS_POINT = 1800   # GeV
CHARGE = 15   # electron charge/3
ZMASS_POINT = 91.18 # GeV
PROCESS_FILE = 'SimG4Core/CustomPhysics/data/RhadronProcessList.txt'
PARTICLE_FILE = 'Configuration/Generator/data/newStau/particles_HIP%d_%s_%d_GeV.txt' % (CHARGE, FLAVOR, MASS_POINT)
SLHA_FILE = 'None'
PDT_FILE = 'Configuration/Generator/data/newStau/hscppythiapdtHIP%d%s%d.tbl'  % (CHARGE, FLAVOR, MASS_POINT)
USE_REGGE = False

hipMass = float (MASS_POINT)
vectCoupling = float ((CHARGE/3)*0.92) # -2Qsin^2(thetaW)

import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP2Settings_cfi import *


generator = cms.EDFilter("Pythia8ConcurrentGeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(COM_ENERGY),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP2SettingsBlock,
        processParameters = cms.vstring(
          'NewGaugeBoson:ffbar2gmZZprime = on',
          'Zprime:gmZmode = 5', # full gamma*/Zprime interference
          '32:m0 = %f' % ZMASS_POINT, # faking the Zprime to be the Z
          '32:onMode = off',
          '32:onIfAny = 17',
          'Zprime:coup2gen4 = on',
          'Zprime:universality = off',
          'Zprime:atauPrime = 0',
          'Zprime:vtauPrime = %f' % vectCoupling,
          '17:m0 = %f' % hipMass,
          '17:mWidth = 0',
          '17:doForceWidth = true',
          '17:mayDecay = off',
          '17:chargeType  = %f' % CHARGE,
          'PhaseSpace:mHatMin = %f' % (2.0*hipMass),
        ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CP2Settings',
            'processParameters'
            )
        )
)

generator.hscpFlavor = cms.untracked.string(FLAVOR)
generator.massPoint = cms.untracked.int32(MASS_POINT)
generator.slhaFile = cms.untracked.string(SLHA_FILE)
generator.processFile = cms.untracked.string(PROCESS_FILE)
generator.particleFile = cms.untracked.string(PARTICLE_FILE)
generator.pdtFile = cms.FileInPath(PDT_FILE)
generator.useregge = cms.bool(USE_REGGE)

genfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(0., 0.),
    MinP = cms.untracked.vdouble(0., 0.),
    MaxEta = cms.untracked.vdouble(100., 100.),
    MinEta = cms.untracked.vdouble(-100, -100),
    ParticleCharge = cms.untracked.int32(0),
    ParticleID1 = cms.untracked.vint32(17),
    ParticleID2 = cms.untracked.vint32(17)
)

ProductionFilterSequence = cms.Sequence(generator*genfilter)
