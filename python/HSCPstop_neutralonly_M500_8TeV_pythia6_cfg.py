FLAVOR = 'stop'
COM_ENERGY = 8000. # GeV
MASS_POINT = 500   # GeV
PROCESS_FILE = 'SimG4Core/CustomPhysics/data/stophadronProcessList_onlyneutral.txt'
PARTICLE_FILE = 'Configuration/GenProduction/python/particles_%s_%d_GeV.txt'  % (FLAVOR, MASS_POINT)
SLHA_FILE =' None'

stopMass = float (MASS_POINT)

import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from PhysicsTools.HepMCCandAlgos.genParticles_cfi import *
genParticles.abortOnUnknownPDGCode = cms.untracked.bool(False)

from Configuration.Generator.PythiaUESettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         filterEfficiency = cms.untracked.double(1.),
                         pythiaHepMCVerbosity = cms.untracked.bool(True),
                         comEnergy = cms.double(COM_ENERGY),
                         crossSection = cms.untracked.double(-1),
                         maxEventsToPrint = cms.untracked.int32(0),
                         stopHadrons = cms.bool(True),
                         gluinoHadrons =  cms.bool(False),  
						 PythiaParameters = cms.PSet(
    pythiaUESettingsBlock,
    processParameters = cms.vstring(
    'MSEL=0          ! User defined processes',
    'IMSS(1)=1       !  brute force',
	'MSUB(261)=1      !  subprocess ffbar-t1t1bar',
	'MSUB(264)=1      !  subprocess gg-t1t1bar',
	'IMSS(3)=1',
	'RMSS(3)=1000.    ! gluino mass',  
	'RMSS(1)=1000.    ! gluino mass',
	'RMSS(2)=1000.    ! gluino mass',
	'RMSS(4)=5000.    ',
	'MDCY(302,1)=0    ! set stop stable',
	'MWID(302)=0      ! set stop width',
	'IMSS(5)=1',
	'RMSS(12)=%f    ! stop mass' % stopMass,
	'RMSS(10)=%f    ! 1.5*stopmass' % (stopMass*1.5),
	### 'MSTJ(14)=-1', # this is actually hardcoded in Py6Had class
	### 'MSTP(111)=0'  # this is a MANDATORY, thus it's hardcoded in Py6ad class
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
