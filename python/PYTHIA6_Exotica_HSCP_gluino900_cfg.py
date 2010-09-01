FLAVOR = 'gluino'
COM_ENERGY = 7000. # GeV
MASS_POINT = 900   # GeV
PROCESS_FILE = 'SimG4Core/CustomPhysics/data/RhadronProcessList.txt'
PARTICLE_FILE = 'SimG4Core/CustomPhysics/data/particles_%s_%d_GeV.txt'  % (FLAVOR, MASS_POINT)
SLHA_FILE =' None'

gluinoMass = float (MASS_POINT)

import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from PhysicsTools.HepMCCandAlgos.genParticles_cfi import *
#genParticles.abortOnUnknownPDGCode = cms.untracked.bool(False)

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         filterEfficiency = cms.untracked.double(1.),
                         pythiaHepMCVerbosity = cms.untracked.bool(True),
                         comEnergy = cms.double(COM_ENERGY),
                         crossSection = cms.untracked.double(-1),
                         maxEventsToPrint = cms.untracked.int32(0),
                         stopHadrons = cms.bool(False),
                         gluinoHadrons =  cms.bool(True),  
						 PythiaParameters = cms.PSet(
    pythiaUESettingsBlock,
    processParameters = cms.vstring(
    'MSEL=0          ! User defined processes',
    'IMSS(1)=1       !  brute force',
    'MSUB(243)=1     !  subprocess',
    'MSUB(244)=1     !  subprocess',    
    'IMSS(3)=1',
    'RMSS(1)=%f !should be gluino mass/2' % (gluinoMass/2.),
    'RMSS(2)=%f !should be gluino mass/2' % (gluinoMass/2.),
    'RMSS(3)=%f !should be gluino mass' % gluinoMass,
    'RMSS(10)=%f !RMSS(10)=2.2*mass gluino' % (gluinoMass*2.2),
    'RMSS(11)=%f !RMSS(10)=2.1*mass gluino' % (gluinoMass*2.1),
    'RMSS(12)=%f !should be gluino mass X2' % (gluinoMass*2.),
    'MDCY(309,1)=0                   ! set gluino stable',
    'IMSS(5)=1',
	'RMSS(8)=100000 !alway 1E5 for rmss(8,9)',
	'RMSS(9)=100000 !RMSS(8-9) make squark/lepton heavy enough to not decay',
    'MSTJ(14)=-1',
    'MSTP(111)=0 ! no hadronization'
    ),
    parameterSets = cms.vstring(
    'pythiaUESettings', 
    'processParameters'
    )
    
    )
                         )
generator.hscpFlavor = cms.untracked.string(FLAVOR)
generator.massPoint = cms.untracked.int32(MASS_POINT)
generator.particleFile = cms.untracked.string(PARTICLE_FILE)
generator.slhaFile = cms.untracked.string(SLHA_FILE)
generator.processFile = cms.untracked.string(PROCESS_FILE)

ProductionFilterSequence = cms.Sequence(generator)
