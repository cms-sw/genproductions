FLAVOR = 'stau'
COM_ENERGY = 8000. 
MASS_POINT = 730   # GeV
PROCESS_FILE = 'SimG4Core/CustomPhysics/data/RhadronProcessList.txt'
PARTICLE_FILE = 'Configuration/Generator/data/particles_%s_%d_GeV.txt' % (FLAVOR, MASS_POINT)
SLHA_FILE = 'file:/uscms_data/d2/jchen/dichamp/8TeV/modelrho1600/Dichamp_8TeV_Dichamp%dGeV.lhe' % MASS_POINT
PDT_FILE = 'Configuration/Generator/data/hscppythiapdt%s%d.tbl'  % (FLAVOR, MASS_POINT)

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

#source = cms.Source("LHESource",
#                                       fileNames = cms.untracked.vstring('%s' %
# SLHA_FILE)
#                                       )
generator = cms.EDFilter("Pythia6HadronizerFilter",
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.double(COM_ENERGY),
    crossSection = cms.untracked.double(-1),
    maxEventsToPrint = cms.untracked.int32(0),
    stopHadrons = cms.bool(False),
    gluinoHadrons =  cms.bool(False),  
                         
	PythiaParameters = cms.PSet(
	    pythiaUESettingsBlock,
	 	processParameters = cms.vstring(
 	      'MSEL=0          ! User defined processes',
		  'MDCY(C1000015,1)=0       ! set the stau stable.',
		  'MDCY(C2000015,1)=0       ! set the stau stable.'
		  ),
    parameterSets = cms.vstring(

    'pythiaUESettings', 'processParameters'),
    
    )
 )
                         
generator.hscpFlavor = cms.untracked.string(FLAVOR)
generator.massPoint = cms.untracked.int32(MASS_POINT)
generator.slhaFile = cms.untracked.string(SLHA_FILE)
generator.processFile = cms.untracked.string(PROCESS_FILE)
generator.particleFile = cms.untracked.string(PARTICLE_FILE)
generator.pdtFile = cms.FileInPath(PDT_FILE)

ProductionFilterSequence = cms.Sequence(generator)
