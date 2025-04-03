from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

process.generator = cms.EDFilter("Pythia8ConcurrentGeneratorFilter",
	comEnergy = cms.double(13000),
	crossSection = cms.untracked.double(1),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),
	
	PythiaParameters = cms.PSet(
                pythia8CommonSettingsBlock,
                pythia8CP5SettingsBlock,
		processParameters = cms.vstring(
			'PhaseSpace:mHatMin = 6000 ',
			'PhaseSpace:mHatMax = 13000 ',
			'PhaseSpace:pTHatMin = 600.0 ',
			'HardQCD:all = off ',
	                'ExtraDimensionsLED:dijets = on',
			'ExtraDimensionsLED:CutOffmode = 0',
			'ExtraDimensionsLED:LambdaT = 10000',
			'ExtraDimensionsLED:negInt = 0', # Change sign of interferecen term if ==1
			'ExtraDimensionsLED:nQuarkNew = 5', # outgoing mass-less quark flavours
			'ExtraDimensionsLED:opMode = 1', # 0=Franceshini paper 1=Giudice paper

		),
		parameterSets = cms.vstring('pythia8CommonSettings',
                                            'pythia8CP5Settings',
                                            'processParameters')
	)
)
