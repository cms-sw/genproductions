#
# This produces an H0, which decays to a pair of long-lived exotic particles
# (PDG code=6000111, 6000112 or 6000113) which then each decay to difermions.
#
# Author: Ian Tomalin
#

COM_ENERGY = 8000 # GeV (centre-of-mass energy)
MASS_HIGGS = '1000' # GeV (mass of resonance that decays to long-lived particle pair - GeV)
MASS_X     = '50' # (mass of long-lived particle - GeV)
CTAU_X     = '4To400' # (c*tau with units of long-lived particle - mm - central c*tau if more than one value being generated)
FERMIONS   = '4F' # type of fermions produced when long-lived exotic decays
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
generator.filterEfficiency = cms.untracked.double(0.99)

generator.PythiaParameters.pythiaParameters.extend(
    cms.vstring('PMAS(35,1)=%s ! Set Higgs mass. This probably has no effect ...'  %MASS_HIGGS)
)

# This block controls how Pythia interacts with the PYUPDA cards.
# The PYUPDA cards specify the masses of the Higgs and long-lived particles,
# the c*tau (mm) of the long-lived particles, and their branching ratios.
generator.PythiaParameters.PYUPDAParameters.extend(
    cms.vstring("PYUPDAFILE = \'%s\'"    %PYUPDA_CARDS)
)

ProductionFilterSequence = cms.Sequence(generator*genParticlesForFilter*XtoFFbarFilter)
