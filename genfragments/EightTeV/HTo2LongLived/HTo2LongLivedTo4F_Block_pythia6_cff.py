#
# This is used to produce an H0, which decays to a pair of long-lived exotic particles
# (PDG code=6000111, 6000112 or 6000113) which then each decay to difermions.
#
# It should not be used directly by itself, but instead imported into another file, such as
# HTo2LongLivedTo4F_MH_1000_MFF_150_CTau100_7TeV_pythia6_cff.py
# where the dummy parameters in this file should be overridden.
#
# Author: Ian Tomalin
#

# Dummy parameters
E_BEAM_DUMMY     = -1 # GeV (beam energy)
MASS_HIGGS_DUMMY = -1 # GeV (mass of resonance that decays to long-lived particle pair)
MASS_LL_DUMMY    = 'dummy' # (mass with units of long-lived particle)
CTAU_LL_DUMMY    = 'dummy' # (c*tau with units of long-lived particle)
FERMION_DUMMY    = 'dummy' # type of fermions produced when long-lived exotic decays
PYUPDA_CARDS_DUMMY = 'dummy' # (PYUPDA card file)

import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from SimGeneral.HepPDTESSource.pythiapdt_cfi import *

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(10),
    # Print event
    pythiaPylistVerbosity = cms.untracked.int32(3),
    # Print decay tables
#    pythiaPylistVerbosity = cms.untracked.int32(12),                         
    filterEfficiency = cms.untracked.double(0.99),
    comEnergy = cms.double(E_BEAM_DUMMY),   
    crossSection = cms.untracked.double(-1.),
    UseExternalGenerators = cms.untracked.bool(False),
#
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        pythiaParameters = cms.vstring(
# This is needed if you are introducing long-lived exotic particles.
          'MSTJ(22)=1    ! Decay ALL unstable particles',
# Using MSTJ(22)=4 instead of 1 prevented GEANT crashing, but will never generate events
# with one exotic decaying inside CMS and the other outside, so is not wonderful ...
# No longer seems necessary in CMSSW 5.x
#          'MSTJ(22)=4    ! Require both long-lived exotics to decay inside the CMS volume',
#          'PARJ(73)=7000. ! radius (mm) of CMS cylinder',
#          'PARJ(74)=9000. ! half (mm) length of CMS cylinder',

# Disable colour reconnection, since it can put colour strings between widely separated partons.
          'MSTP(95)=0',

          'MSEL=0',
          
# Request gg -> H0 production
          'MSUB(152)=1',
          'MWID(35)=2 ! Let me set H0 properties'
# Don't set this here. Set instead in file that imports this one.           
#          'PMAS(35,1)=%f ! This probably has no effect ...'  %MASS_HIGGS_DUMMY ,
          'PMAS(35,2)=1.'

# Request gg -> h0 production
#          'IMSS(1)=1              ! MSSM',
#          'MSUB(102)=1',
#          'MWID(25)=2 ! Let me set h0 properties',
#          'PMAS(25,1)=450.'
#          'PMAS(25,2)=20.'

# Request Z' production
#          'MSUB(141)=1',
#          'MSTP(44)=7 ! Control Z-prime - Z interference ?',
#          'MWID(32)=2 ! Let me set Z-prime properties',
#          'PMAS(32,1)=700.',
#          'PMAS(32,2)=5.'
        ),
#
# This block controls how Pythia interacts with the PYUPDA cards.
# The PYUPDA cards specify the masses of the Higgs and long-lived particles,
# the c*tau (mm) of the long-lived particles, and their branching ratios.
#        
        PYUPDAParameters = cms.vstring(
# Either:
#     1) Specify the location of the PYUPDA table to be read in. 
# Don't set this here. Set instead in file that imports this one.
         "PYUPDAFILE = \'%s\'"    %PYUPDA_CARDS_DUMMY
#        Optionally ask to call PYUPDA after PYINIT. Don't do this unless you have to.
#        "PYUPDApostPYINIT"
# Or:
#     2) Write current PYUPDA parameters to file (so you can edit them and read back in).
#         "PYUPDAFILE = \'pyupda.out\'",
#         "PYUPDAWRITE"
        ),
#
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
                                    'pythiaParameters',
                                    'PYUPDAParameters')
    )
)

# N.B. If your PYUPDA tables introduces new exotic particles, you will need
# to include:
#
from PhysicsTools.HepMCCandAlgos.genParticles_cfi import *
genParticles.abortOnUnknownPDGCode = cms.untracked.bool(False)

# The H0 must have a small coupling to SM particles in order to be produced.
# This means that it occassionally decays to them. The following filter
# rejects these decays (and prints statistics).

genParticlesForFilter = genParticles.clone()
from GeneratorInterface.GenFilters.XtoFFbarFilter_cfi import *
XtoFFbarFilter.src = cms.InputTag("genParticlesForFilter")

# Specify PDG codes of exotics to be accepted by filter.
XtoFFbarFilter.idMotherX = cms.vint32(6000111, 6000112, 6000113, 6000114,
                                      6001111, 6001112, 6001113, 6001114,
                                      6002111, 6002112, 6002113, 6002114,
                                      6003111, 6003112, 6003113, 6003114)
XtoFFbarFilter.idMotherY = cms.vint32(6000111, 6000112, 6000113, 6000114,
                                      6001111, 6001112, 6001113, 6001114,
                                      6002111, 6002112, 6002113, 6002114,
                                      6003111, 6003112, 6003113, 6003114)

# To be defined in file that imports this one.
#ProductionFilterSequence = cms.Sequence(generator*genParticlesForFilter*XtoFFbarFilter)
