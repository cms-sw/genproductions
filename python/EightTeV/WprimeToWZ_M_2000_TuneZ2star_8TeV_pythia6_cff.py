import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaPylistVerbosity = cms.untracked.int32(0),
	filterEfficiency = cms.untracked.double(1),
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(3.129e-3),
	
	PythiaParameters = cms.PSet(
	        pythiaUESettingsBlock,
                processParameters = cms.vstring(
        'MSEL=0 ! (D=1) 0 to select full user control',
        'PMAS(34,1)=2000 ! mass of Wprime',
        'MSUB(142)=1 ! qq->Wprime',
        'MDME(311,1)=0 ! Wprime->dubar',        'MDME(312,1)=0 ! Wprime->dcbar',
        'MDME(313,1)=0 ! Wprime->dtbar',
        'MDME(315,1)=0 ! Wprime->subar',        'MDME(316,1)=0 ! Wprime->scbar',
        'MDME(317,1)=0 ! Wprime->stbar',
        'MDME(319,1)=0 ! Wprime->bubar',        'MDME(320,1)=0 ! Wprime->bcbar',
        'MDME(321,1)=0 ! Wprime->btbar',
        'MDME(327,1)=0 ! Wprime->enu',        'MDME(328,1)=0 ! Wprime->munu',
        'MDME(329,1)=0 ! Wprime->taunu',
        'MDME(331,1)=1 ! Wprime->WZ',
        'MDME(332,1)=0 ! Wprime->Wgamma',
        'MDME(333,1)=0 ! Wprime->Wh0',
        ),
		parameterSets = cms.vstring(
		        'pythiaUESettings',
			'processParameters')
	)
)


ProductionFilterSequence = cms.Sequence(generator)
