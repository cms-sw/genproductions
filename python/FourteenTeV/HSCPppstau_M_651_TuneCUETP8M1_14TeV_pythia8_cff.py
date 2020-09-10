FLAVOR = 'stau'
COM_ENERGY = 14000. 
MASS_POINT = 651   # GeV
PROCESS_FILE = 'SimG4Core/CustomPhysics/data/RhadronProcessList.txt'
PARTICLE_FILE = 'Configuration/Generator/data/particles_%s_%d_GeV.txt' % (FLAVOR, MASS_POINT)
SLHA_FILE = 'Configuration/Generator/data/isa-slha%dGeV.out' % MASS_POINT
PDT_FILE = 'Configuration/Generator/data/hscppythiapdt%s%d.tbl'  % (FLAVOR, MASS_POINT)
USE_REGGE = False

import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(COM_ENERGY),
    crossSection = cms.untracked.double(-1),
    maxEventsToPrint = cms.untracked.int32(0),
    SLHAFileForPythia8 = cms.string('%s' % SLHA_FILE),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'SUSY:all = on',
            'SUSY:idA = 1000015',
            'SUSY:idB = 1000015',
            '1000006:mayDecay = off'
        ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
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

ProductionFilterSequence = cms.Sequence(generator)
