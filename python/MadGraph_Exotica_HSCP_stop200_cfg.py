FLAVOR = 'stop'
COM_ENERGY = 7000. # GeV
MASS_POINT = 200   # GeV
PROCESS_FILE = 'SimG4Core/CustomPhysics/data/stophadronProcessList.txt'
PARTICLE_FILE = 'SimG4Core/CustomPhysics/data/particles_%s_%d_GeV.txt'  % (FLAVOR, MASS_POINT)
SLHA_FILE ='file:/uscms_data/d1/jchen/stop-stop-%d-xqcut20-7TeV.lhe' % MASS_POINT
QCUT = 30.

import FWCore.ParameterSet.Config as cms

#source = cms.Source("LHESource",
#					fileNames = cms.untracked.vstring('%s' % SLHA_FILE)
#					)

from PhysicsTools.HepMCCandAlgos.genParticles_cfi import *
genParticles.abortOnUnknownPDGCode = cms.untracked.bool(False)

from Configuration.Generator.PythiaUESettings_cfi import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
                         filterEfficiency = cms.untracked.double(0.334),
                         pythiaHepMCVerbosity = cms.untracked.bool(True),
                         comEnergy = cms.double(COM_ENERGY),
                         crossSection = cms.untracked.double(-1),
                         maxEventsToPrint = cms.untracked.int32(0),
                         stopHadrons = cms.bool(True),
                         gluinoHadrons =  cms.bool(False),  
						 PythiaParameters = cms.PSet(
    pythiaUESettingsBlock,
    processParameters = cms.vstring(
	'MSEL=0         ! Full user control',
	'MDCY(302,1)=0    ! set stop stable',
	'MWID(302)=0',
	'IMSS(1)=1        !  brute force',
    ),
    parameterSets = cms.vstring(
    'pythiaUESettings', 'processParameters'),
	),
	jetMatching = cms.untracked.PSet(
	scheme = cms.string("Madgraph"),
	mode = cms.string("auto"),        # soup, or "inclusive" / "exclusive"
	MEMAIN_etaclmax = cms.double(5.0),
	MEMAIN_qcut = cms.double(QCUT),
	MEMAIN_minjets = cms.int32(0),
	MEMAIN_maxjets = cms.int32(2),
	MEMAIN_iexcfile = cms.uint32(0), ## only set to 1 if need to perfo
	)
	)
generator.hscpFlavor = cms.untracked.string(FLAVOR)
generator.massPoint = cms.untracked.int32(MASS_POINT)
generator.particleFile = cms.untracked.string(PARTICLE_FILE)
generator.slhaFile = cms.untracked.string(SLHA_FILE)
generator.processFile = cms.untracked.string(PROCESS_FILE)

ProductionFilterSequence = cms.Sequence(generator)
