################
# Gen fragment #
################


import FWCore.ParameterSet.Config as cms


configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.3 $'),
	name = cms.untracked.string('PYTHIA6_Xibstar_8TeV_cff.py'),
	annotation = cms.untracked.string('Data2012: Pythia6+EvtGen generation of Xib*0 decay chain, 8TeV'))


#from Configuration.Generator.PythiaUEZ2Settings_cfi import *
from Configuration.Generator.PythiaUEZ2starSettings_cfi import *


generator = cms.EDFilter("Pythia6GeneratorFilter",
			 pythiaPylistVerbosity = cms.untracked.int32(0),
			 pythiaHepMCVerbosity = cms.untracked.bool(False),
			 comEnergy = cms.double(8000.0),
			 crossSection = cms.untracked.double(0.0),     # Given by PYTHIA after running 
			 filterEfficiency = cms.untracked.double(1.0), # Given by PYTHIA after running
			 maxEventsToPrint = cms.untracked.int32(0),
			 
			 ExternalDecays = cms.PSet(
        EvtGen = cms.untracked.PSet(
	operates_on_particles = cms.vint32(0),
	use_default_decay = cms.untracked.bool(False),
	decay_table = cms.FileInPath('GeneratorInterface/ExternalDecays/data/DECAY.DEC'),
	particle_property_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/evt.pdl'),
	user_decay_file = cms.FileInPath('GeneratorInterface/ExternalDecays/data/Xib.dec'),
	list_forced_decays = cms.vstring('MyXi_b0','Myanti-Xi_b0')),
        parameterSets = cms.vstring('EvtGen')),
			 
			 PythiaParameters = cms.PSet(
	pythiaUESettingsBlock,
	bbbarSettings = cms.vstring('MSEL = 1'),
	parameterSets = cms.vstring('pythiaUESettings','bbbarSettings')))


###########
# Filters #
###########
# Filter only pp events which produce a  Xi*_b0 : efficiency ~6e-3
b0filter = cms.EDFilter("PythiaFilter", ParticleID = cms.untracked.int32(5324))

# Filter on final state muons
mumugenfilter = cms.EDFilter("MCParticlePairFilter",
			     Status = cms.untracked.vint32(1, 1),
			    # MinPt = cms.untracked.vdouble(3.3, 3.3), # we don't use pt range for Xib*0 decay chain,pt>0 
			     MaxEta = cms.untracked.vdouble(2.3, 2.3),
			     MinEta = cms.untracked.vdouble(-2.3, -2.3),
			     ParticleCharge = cms.untracked.int32(-1),
			     #ParticleID1 = cms.untracked.vint32(13),
			     #ParticleID2 = cms.untracked.vint32(13)
                             )

ProductionFilterSequence = cms.Sequence(generator*b0filter*mumugenfilter)
