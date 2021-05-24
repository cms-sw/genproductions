MNEU = 400.0
CTAU = 1.0

MGLU = MNEU + 5.0
WIDTH = 0.0197e-11 / CTAU

SLHA_TABLE = '''
BLOCK SPINFO      # Spectrum calculator information
    1    Minimal  # spectrum calculator
    2    1.0.0    # version number

BLOCK MODSEL      # Model selection
    1    1        #

BLOCK MASS        # Mass Spectrum
#   PDG code   mass  particle
    1000021    %E    # ~g
    1000022    %E    # ~chi_10

DECAY    1000021    0.01E+0.00       # gluino decays
#   BR          NDA  ID1        ID2
    1.0E00      2    1000022    21   # BR(~g -> ~chi_10 g)

DECAY    1000022    %E               # neutralino decays
#   BR          NDA  ID1   ID2   ID3
    5.00E-01    3     3     5     6  # BR(~chi_10 -> s    b    t)
    5.00E-01    3    -3    -5    -6  # BR(~chi_10 -> sbar bbar tbar)
''' % (MGLU, MNEU, WIDTH)

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter('Pythia8GeneratorFilter',
    comEnergy = cms.double(13000.0),
    filterEfficiency = cms.untracked.double(1.0),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    SLHATableForPythia8 = cms.string(SLHA_TABLE),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'SUSY:gg2gluinogluino = on',
            'SUSY:qqbar2gluinogluino = on',
            'SUSY:idA = 1000021',
            'SUSY:idB = 1000021',
            '1000022:tau0 = %f' % CTAU,
            ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
            'processParameters',
            ),
        ),
    )
