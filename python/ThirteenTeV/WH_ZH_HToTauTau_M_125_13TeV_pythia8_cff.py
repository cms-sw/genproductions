import FWCore.ParameterSet.Config as cms

# Z2 star tune with pT-ordered showers
#from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
#from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(10),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    comEnergy = cms.double(13000.),
 	PythiaParameters = cms.PSet(
	py8HDecaySettings = cms.vstring(  
			 		  '25:m0 = 125.0',   # Mass of The Higgs 
			       	   	  'HiggsSM:all = off',      # turn OFF all H production mode
			       		  'HiggsSM:ffbar2HW = on',  # Turn ON WH production mode
			       		  'HiggsSM:ffbar2HZ = on',  # Turn ON ZH production mode
					  '25:onMode = off', # turn OFF all H decays
			#		  '25:onIfAny = 5'  # turn ON H->bbbar
			                  '25:onIfAny = 15 15', #turn ON H->tautau
					  'ParticleDecays:sophisticatedTau = 1', 	# interface to Tauola for the tau decay  
			       		  'Tune:pp 5'   # UE tune
			 ),
        parameterSets = cms.vstring('py8HDecaySettings')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/ThirteenTeV/WH_ZH_HToTauTau_M_125_13TeV_pythia8_cff.py,v $'),
    annotation = cms.untracked.string(' PYTHIA8 - WH / ZH  -> tautau  at 13TeV')
    )
