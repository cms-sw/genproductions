from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
from Configuration.Generator.Pythia8aMCatNLOSettings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentHadronizerFilter",
	maxEventsToPrint = cms.untracked.int32(1),
	pythiaPylistVerbosity = cms.untracked.int32(1),
	filterEfficiency = cms.untracked.double(1.0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	comEnergy = cms.double(13600.),
	PythiaParameters = cms.PSet(
	pythia8CommonSettingsBlock,
	pythia8CP5SettingsBlock,
	pythia8PSweightsSettingsBlock,
	pythia8aMCatNLOSettingsBlock,
  	processParameters = cms.vstring(
  		'JetMatching:setMad = off',
  		'JetMatching:scheme = 1',
  		'JetMatching:merge = on',
  		'JetMatching:jetAlgorithm = 2',
  		'JetMatching:etaJetMax = 999.',
  		'JetMatching:coneRadius = 1.',
  		'JetMatching:slowJetPower = 1',
  		'JetMatching:qCut = 40.', #this is the actual merging scale
  		'JetMatching:doFxFx = on',
  		'JetMatching:qCutME = 20.',#this must match the ptj cut in the lhe generation step
  		'JetMatching:nQmatch = 5', #4 corresponds to 4-flavour scheme (no matching of b-quarks), 5 for 5-flavour scheme
  		'JetMatching:nJetMax = 1', #number of partons in born matrix element for highest multiplicity
  		'SLHA:useDecayTable = off',  # Use pythia8s own decay mode instead of decays defined in LH accord
  		'25:m0 = <MASS>',
	  	'23:mMin = 0.05',       # Solve problem with mZ cut
  		'24:mMin = 0.05',       # Solve problem with mW cut
  		'25:onMode = off',
  		'25:onIfMatch = 22 22',    # Decay only higgs to gamma gamma, note onIfMatch is used instead of onIfAny since otherwise H->Gamma Z, Z-> Gamma is also possible.
  ),
	parameterSets = cms.vstring('pythia8CommonSettings',
		'pythia8CP5Settings',
		'pythia8PSweightsSettings',
		'pythia8aMCatNLOSettings',
		'processParameters',
	)
  )
)