# NMSSM Hto2Ato4mu 13TeV pythia8 configuration file
import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaPylistVerbosity = cms.untracked.int32(1),
	filterEfficiency = cms.untracked.double(1.0),
	crossSection = cms.untracked.double(0.0),
	comEnergy = cms.double(13000.0),
	PythiaParameters = cms.PSet(
		pythia8CommonSettingsBlock,
		pythia8CUEP8M1SettingsBlock,
		pythiaUESettings = cms.vstring(),
		processParameters = cms.vstring(
			# This section should be entirely in Pythia 8. See details in
		        #   - http://home.thep.lu.se/~torbjorn/pythia82html/HiggsProcesses.html
		        #   - http://home.thep.lu.se/~torbjorn/pythia82html/ParticleDataScheme.html
			'Higgs:useBSM = on',     # Initialize and use the two-Higgs-doublet BSM states
			'HiggsBSM:all = off',    # Switch off all BSM Higgs production
			'HiggsBSM:gg2H2 = on',   # Switch on gg->H^0(H_2^0) scattering via loop contributions primarily from top. Code 1022. 
			'35:m0 = 90.0',         #  mass in GeV of H0 (PDG ID = 35)
			'36:m0 = 4.0',           #  mass in GeV of A0 (PDG ID = 36)
			# decays of H0 (PDG ID = 35)
			'35:onMode = off',       # Turn off all H0 decay modes 
			'35:onIfMatch = 36 36',  # Allow H0 decays to A0: H0 ->A0A0
			# decays of A0 (PDG ID = 36)
			'36:onMode = off',       # Turn off all A0 decay modes
			'36:onIfMatch = 13 -13', # Allow A0 decays to muons: A0 ->mu+mu-
			# Useful debug printouts
			'Init:showProcesses = on',        # Print a list of all processes that will be simulated, with their estimated cross section maxima
			'Init:showChangedSettings = on',  # Print a list of the changed flag/mode/parameter/word setting
			#'Init:showAllParticleData = on', # Print a list of all particle and decay data. Warning: this will be a long list
	    ),
		parameterSets = cms.vstring(
            'pythiaUESettings', 
		    'pythia8CommonSettings',
		    'pythia8CUEP8M1Settings',
		    'processParameters'
	    )
    )
)
