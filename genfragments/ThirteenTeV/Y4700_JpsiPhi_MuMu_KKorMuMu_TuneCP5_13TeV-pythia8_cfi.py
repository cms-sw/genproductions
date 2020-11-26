import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *


generator = cms.EDFilter("Pythia8GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.0),
    crossSection = cms.untracked.double(0.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        processParameters = cms.vstring(
            'Charmonium:states(3PJ) = 10441', # generating only Chi_c1 particle             
            'Charmonium:O(3PJ)[3P0(1)] = 0.05', # The color-singlet long-distance matrix elements
            'Charmonium:O(3PJ)[3S1(8)] = 0.0031', # The color-singlet long-distance matrix elements
            'Charmonium:gg2ccbar(3PJ)[3PJ(1)]g = on', # Colour-singlet production of 3PJ charmonium states via g g -> ccbar[3PJ(1)] g
            'Charmonium:qg2ccbar(3PJ)[3PJ(1)]q = on', # Colour-singlet production of 3PJ charmonium states via q g -> ccbar[3PJ(1)] q
            'Charmonium:qqbar2ccbar(3PJ)[3PJ(1)]g = on', # Colour-singlet production of 3PJ charmonium states via q qbar -> ccbar[3PJ(1)] g
            'Charmonium:gg2ccbar(3PJ)[3S1(8)]g = on', # Colour-octet production of 3PJ charmonium states via g g -> ccbar[3S1(8)] g
            'Charmonium:qg2ccbar(3PJ)[3S1(8)]q = on', # Colour-octet production of 3PJ charmonium states via q g -> ccbar[3S1(8)] q
            'Charmonium:qqbar2ccbar(3PJ)[3S1(8)]g = on',# Colour-octet production of 3PJ charmonium states via q qbar -> ccbar[3S1(8)] g
            'ParticleDecays:allowPhotonRadiation = on', 
			      '10441:m0 = 4.704', 
            '10441:mWidth = 0.120', 
            '10441:mMin = 4.116',
            '10441:mMax = 5.886',
            '10441:addChannel = on 1 0 443 333',
            '10441:onMode = off',
            '10441:onIfMatch = 443 333',
            '443:onMode = off',
            '443:onIfMatch = 13 -13',
            '333:onMode = off',
            '333:0:onMode = on', # channel_0 = 321 -321
            '333:0:bRatio = 0.5',
            '333:9:onMode = on', # channel_9 = 13 -13
            '333:9:bRatio = 0.5',
            ),
parameterSets = cms.vstring('pythia8CommonSettings',
                                 'pythia8CP5Settings',
                                 'processParameters',
                                 )
      )
 )

motherFilter = cms.EDFilter("PythiaFilter",
    MaxEta = cms.untracked.double(9999.0),
    MinEta = cms.untracked.double(-9999.0),
    ParticleID = cms.untracked.int32(10441) # Chi_c1 as Y4140
)

# verbose threshold for "PythiaDauVFilter" are 2,5,10

decayfilter = cms.EDFilter("PythiaDauVFilter",
    ParticleID = cms.untracked.int32(10441),
    NumberDaughters = cms.untracked.int32(2),
    DaughterIDs = cms.untracked.vint32(443, 333),
    MaxEta = cms.untracked.vdouble(9999.0, 9999.0),
    MinEta = cms.untracked.vdouble(-9999.0, -9999.0),
    MinPt = cms.untracked.vdouble(-1.0, -1.0),
    verbose = cms.untracked.int32(4*3)
)

psifilter = cms.EDFilter("PythiaDauVFilter",
    MotherID = cms.untracked.int32(10441),
    ParticleID = cms.untracked.int32(443),
    NumberDaughters = cms.untracked.int32(2),
    DaughterIDs = cms.untracked.vint32(13, -13),
    MinPt = cms.untracked.vdouble(0.5, 0.5),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    verbose = cms.untracked.int32(4*1)
)

phipositivelegfilter = cms.EDFilter("PythiaDauVFilter",
    MotherID        = cms.untracked.int32(10441),
    ParticleID      = cms.untracked.int32(333),
    NumberDaughters = cms.untracked.int32(1),
    DaughterIDs     = cms.untracked.vint32(321, 13), # either K+ or mu+
    MinPt           = cms.untracked.vdouble(0.5, 0.5),
    MinEta          = cms.untracked.vdouble(-2.5, 2.5),
    MaxEta          = cms.untracked.vdouble( 2.5, 2.5),
    verbose         = cms.untracked.int32(4*1),
    )

phinegativelegfilter = cms.EDFilter("PythiaDauVFilter",
    MotherID        = cms.untracked.int32(10441),
    ParticleID      = cms.untracked.int32(333),
    NumberDaughters = cms.untracked.int32(1),
    DaughterIDs     = cms.untracked.vint32(-321, -13), # either K- or mu-
    MinPt           = cms.untracked.vdouble(0.5, 0.5),
    MinEta          = cms.untracked.vdouble(-2.5, 2.5),
    MaxEta          = cms.untracked.vdouble( 2.5, 2.5),
    verbose         = cms.untracked.int32(4*1),
)

ProductionFilterSequence = cms.Sequence(
                         generator
                        *motherFilter
                        *decayfilter
                        *psifilter
                        *phipositivelegfilter
                        *phinegativelegfilter
)
