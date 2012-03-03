#
# This produces an H0, which decays to a pair of long-lived exotic particles
# (PDG code=6000111, 6000112 or 6000113) which then each decay to difermions.
#
# Author: Ian Tomalin
#

COM_ENERGY = 8000 # GeV (centre-of-mass energy)
MASS_HIGGS = '120' # GeV (mass of resonance that decays to long-lived particle pair - GeV)
MASS_X     = '50' # (mass of long-lived particle - GeV)
CTAU_X     = '50To5000' # (c*tau with units of long-lived particle - mm - central c*tau if more than one value being generated)
FERMIONS   = '4B' # type of fermions produced when long-lived exotic decays
PYUPDA_CARDS = 'Configuration/Generator/data/HTo2LongLivedTo%s_MH_%s_MFF_%s_CTau%s_pythia6.pyupda' % (FERMIONS, MASS_HIGGS, MASS_X, CTAU_X) # PYUPDA card file

import FWCore.ParameterSet.Config as cms

#
#=== This sets up most of the Pythia parameters for this production
#

from Configuration.GenProduction.EightTeV.HTo2LongLivedTo4F_Block_pythia6_cff import *

#
#== However, some Pythia parameters are tailored below for the specific generation wanted here.
#

# Centre of mass energy
generator.comEnergy = cms.double(COM_ENERGY)

# Filter efficiency (must be overridden later, since varies from sample to sample)
generator.filterEfficiency = cms.untracked.double(0.034)

generator.PythiaParameters.pythiaParameters.extend(
    cms.vstring('PMAS(35,1)=%s ! Set Higgs mass. This probably has no effect ...'  %MASS_HIGGS)
)

# This block controls how Pythia interacts with the PYUPDA cards.
# The PYUPDA cards specify the masses of the Higgs and long-lived particles,
# the c*tau (mm) of the long-lived particles, and their branching ratios.
generator.PythiaParameters.PYUPDAParameters.extend(
    cms.vstring("PYUPDAFILE = \'%s\'"    %PYUPDA_CARDS)
)

# Specify desired exotics (should be unnecessary, since specified in PYUPDA)
XtoFFbarFilter.idMotherX = cms.vint32(6000112, 6001112, 6002112, 6003112)
XtoFFbarFilter.idMotherY = cms.vint32(6000112, 6001112, 6002112, 6003112)
# Specify how they must decay (should be unnecessary, since specified in PYPDA)
XtoFFbarFilter.idDaughterF = cms.vint32(5)
XtoFFbarFilter.idDaughterG = cms.vint32(5)

mumugenFilter = cms.EDFilter("MCParticlePairFilter",
                             Status = cms.untracked.vint32(1, 1),
                             MinPt = cms.untracked.vdouble(3.0, 3.0),
                             MaxEta = cms.untracked.vdouble(2.1, 2.1),
                             MinEta = cms.untracked.vdouble(-2.1, -2.1),
                             ParticleCharge = cms.untracked.int32(0),
                             ParticleID1 = cms.untracked.vint32(13),
                             ParticleID2 = cms.untracked.vint32(13)
                             )

ProductionFilterSequence = cms.Sequence(generator*genParticlesForFilter*XtoFFbarFilter*mumugenFilter)
