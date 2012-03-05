FLAVOR = 'gluino'
COM_ENERGY = 8000. # GeV
MASS_POINT = 1200   # GeV
PROCESS_FILE = 'SimG4Core/CustomPhysics/data/RhadronProcessList.txt'
PARTICLE_FILE = 'Configuration/Generator/data/particles_%s_%d_GeV.txt'  % (FLAVOR, MASS_POINT)
SLHA_FILE ='Configuration/Generator/data/HSCP_%s_%d_SLHA.spc' % (FLAVOR, MASS_POINT)
PDT_FILE = 'Configuration/Generator/data/hscppythiapdt%s%d.tbl'  % (FLAVOR, MASS_POINT)
USE_REGGE = False

import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         filterEfficiency = cms.untracked.double(-1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(COM_ENERGY),
                         crossSection = cms.untracked.double(-1),
                         maxEventsToPrint = cms.untracked.int32(0),
						 PythiaParameters = cms.PSet(
    processParameters = cms.vstring(
	          'SLHA:file = %s' % SLHA_FILE,
			  'Tune:pp  = 5',
			  'SUSY:all = off',
			  'SUSY:gg2gluinogluino = on',
			  'SUSY:qqbar2gluinogluino = on',
			  'RHadrons:allow  = on',
			  'RHadrons:allowDecay = off',
			  'RHadrons:setMasses = on',
			  'RHadrons:probGluinoball = 0.1',
			  ),
    parameterSets = cms.vstring(
    'processParameters'
    )
    
    )
                         )
generator.hscpFlavor = cms.untracked.string(FLAVOR)
generator.massPoint = cms.untracked.int32(MASS_POINT)
generator.particleFile = cms.untracked.string(PARTICLE_FILE)
generator.slhaFile = cms.untracked.string(SLHA_FILE)
generator.processFile = cms.untracked.string(PROCESS_FILE)
generator.pdtFile = cms.FileInPath(PDT_FILE)
generator.useregge = cms.bool(USE_REGGE)

dirhadrongenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(0., 0.),
    MinP = cms.untracked.vdouble(0., 0.),
    MaxEta = cms.untracked.vdouble(100., 100.),
    MinEta = cms.untracked.vdouble(-100, -100),
    ParticleCharge = cms.untracked.int32(0),
    ParticleID1 = cms.untracked.vint32(1000993,1009213,1009313,1009323,1009113,1009223,1009333,1091114,1092114,1092214,1092224,1093114,1093214,1093224,1093314,1093324,1093334),
    ParticleID2 = cms.untracked.vint32(1000993,1009213,1009313,1009323,1009113,1009223,1009333,1091114,1092114,1092214,1092224,1093114,1093214,1093224,1093314,1093324,1093334)
)

ProductionFilterSequence = cms.Sequence(generator*dirhadrongenfilter)
