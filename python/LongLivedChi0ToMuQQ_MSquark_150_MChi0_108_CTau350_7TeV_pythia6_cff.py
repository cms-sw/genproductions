#
# This produces a pair of squarks or squark anti-squark pair that each decay to neutralino and quark.
# The neutralino decays to a muon and virtual smuon.  The smuon then decays to two quarks
# (This is SUSY RPV, which makes the neutralino long-lived)
#
# Author: Emyr Clement
#

E_BEAM      = 7000 # GeV (beam energy)
MASS_SQUARK = 150 # GeV mass of squark
LAMBDA      = 0.000003 # RPV coupling lambda'(2,1,1) ## (2nd generation lepton, 1st generation quark, 1st generation quark) coupling - controls lifetime.
M1          = 110 # Value of M1 controls neutralino mass

import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from SimGeneral.HepPDTESSource.pythiapdt_cfi import *

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(10),
    # Print event
    pythiaPylistVerbosity = cms.untracked.int32(3),
    # Print decay tables
#    pythiaPylistVerbosity = cms.untracked.int32(12),                         
    filterEfficiency = cms.untracked.double(1.00),
    comEnergy = cms.double(E_BEAM),   
#    crossSection = cms.untracked.double(55000000000.),
    UseExternalGenerators = cms.untracked.bool(False),
#
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        pythiaParameters = cms.vstring(
# This is needed if you are introducing long-lived exotic particles.
#         'MSTJ(22)=1    ! Decay ALL unstable particles',
# Using MSTJ(22)=4 instead of 1 prevents GEANT crashing, but will never generate events
# with one exotic decaying inside CMS and the other outside, so is not wonderful ...
          'MSTJ(22)=4    ! Require both long-lived exotics to decay inside the CMS volume',
          'PARJ(73)=7000. ! radius (mm) of CMS cylinder',
          'PARJ(74)=9000. ! half (mm) length of CMS cylinder',

# Disable colour reconnection, since it can put colour strings between widely separated partons.
          'MSTP(95)=0',          

# Turn everything off
          'MSEL=0',

# Request squark pair production
        'MSUB(271)=1',
        'MSUB(272)=1',
        'MSUB(273)=1',
        'MSUB(274)=1',
        'MSUB(275)=1',
        'MSUB(276)=1',
        'MSUB(277)=1',
        'MSUB(278)=1',
        'MSUB(279)=1',
        'MSUB(280)=1',
        
# Turn on a general MSSM simulations
        'IMSS(1)=1',
        
# Turn on Lepton number violation i.e. allow RPV LQD couplings
# But all couplings set to zero (channels still on though)
        'IMSS(52)=3',
        
# Set lambda' (2,1,1)
        'RVLAMP(2,1,1)=%f' % LAMBDA,
# Turn off decays to neutrino d dbar and to antineutrino d dbar
        'MDME(2241,1)=0',
        'MDME(2242,1)=0',
        
# Set masses (5000 is approximately infinity)
        'RMSS(1)=%i' % M1,
        'RMSS(2)=5000.',
        'RMSS(3)=5000.',
        'RMSS(4)=800.',
        'RMSS(5)=2.',
        'RMSS(6)=5000.',
        'RMSS(7)=5000.',
        'RMSS(8)=%i' % MASS_SQUARK,
        'RMSS(9)=%i' % MASS_SQUARK,
        'RMSS(10)=5000.',
        'RMSS(11)=5000.',
        'RMSS(12)=5000.',
        'RMSS(13)=5000.',
        'RMSS(14)=5000.'
        ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
                                    'pythiaParameters')
    )
)

# N.B. If your PYUPDA tables introduces new exotic particles, you will need
# to include:
#
from PhysicsTools.HepMCCandAlgos.genParticles_cfi import *
genParticles.abortOnUnknownPDGCode = cms.untracked.bool(False)

# The filter is only used to print statistics. It does not filter.
genParticlesForFilter = genParticles.clone()
from GeneratorInterface.GenFilters.XtoFFbarFilter_cfi import *
XtoFFbarFilter.src = cms.InputTag("genParticlesForFilter")
XtoFFbarFilter.idMotherX = cms.vint32(1000022)
XtoFFbarFilter.idMotherY = cms.vint32(1000022)

ProductionFilterSequence = cms.Sequence(generator*genParticlesForFilter*XtoFFbarFilter)
