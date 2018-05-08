import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *

generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,
	crossSection = cms.untracked.double(1.85e-4),
	filterEfficiency = cms.untracked.double(1),

	configFiles = cms.vstring('RS.model'),
	parameterSets = cms.vstring(
		'cm8TeV',
		'pdfMRST2001',
		'productionParameters',
		'basicSetup',
		'setParticlesStableForDetector',
	),
	productionParameters = cms.vstring(
                'cd /Herwig/NewPhysics',
                'insert ResConstructor:Incoming 0 /Herwig/Particles/g',
                'insert ResConstructor:Incoming 1 /Herwig/Particles/u',
                'insert ResConstructor:Incoming 2 /Herwig/Particles/ubar',
                'insert ResConstructor:Incoming 3 /Herwig/Particles/d',
                'insert ResConstructor:Incoming 4 /Herwig/Particles/dbar',
                'insert ResConstructor:Intermediates 0 /Herwig/Particles/Graviton',
                'insert ResConstructor:Outgoing 0 /Herwig/Particles/Z0',
                'set RS/Model:Lambda_pi 7833*GeV',
                'set /Herwig/Particles/Graviton:NominalMass 3000*GeV',
	),
)


ProductionFilterSequence = cms.Sequence(generator)
