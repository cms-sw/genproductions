import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
	maxEventsToPrint = cms.untracked.int32(1),
	pythiaPylistVerbosity = cms.untracked.int32(1),
	filterEfficiency = cms.untracked.double(1.0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	comEnergy = cms.double(13000.0),

	crossSection = cms.untracked.double(7820.25),

	PythiaParameters = cms.PSet(
            pythia8CommonSettingsBlock,
            pythia8CUEP8M1SettingsBlock,
	    processParameters = cms.vstring(
            		'ParticleDecays:limitTau0 = off',
			'ParticleDecays:limitCylinder = on',
            		'ParticleDecays:xyMax = 2000',
            		'ParticleDecays:zMax = 4000',
			'HardQCD:all = on',
			'PhaseSpace:pTHatMin = 300',
			'PhaseSpace:pTHatMax = 470',
           		'130:mayDecay = on',
            		'211:mayDecay = on',
            		'321:mayDecay = on'
	    ),
            parameterSets = cms.vstring('pythia8CommonSettings',
                                        'pythia8CUEP8M1Settings',
                                        'processParameters',
                                        )
	)
)


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('\$Revision$'),
    name = cms.untracked.string('\$Source$'),
    annotation = cms.untracked.string('QCD dijet production, pThat 300to470 GeV, inclusive with long lived decays allowed, 13 TeV, TuneCUETP8M1')
)
