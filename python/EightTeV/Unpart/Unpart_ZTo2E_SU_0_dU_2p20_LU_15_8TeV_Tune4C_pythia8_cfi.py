import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
                        pythiaPylistVerbosity = cms.untracked.int32(1),
                        filterEfficiency = cms.untracked.double(1.0),
                        pythiaHepMCVerbosity = cms.untracked.bool(False),
                        comEnergy = cms.double(8000.0),
                        maxEventsToPrint = cms.untracked.int32(0),
                        PythiaParameters = cms.PSet(
                            pythia8_unparticle = cms.vstring('Main:timesAllowErrors = 10000', 
                                'ExtraDimensionsUnpart:ffbar2UZ = on', 
                                'ParticleDecays:limitTau0 = on', 
                                'ParticleDecays:tau0Max = 10.', 
                                'ExtraDimensionsUnpart:spinU = 0', 
                                'ExtraDimensionsUnpart:dU = 2.200000', 
                                'ExtraDimensionsUnpart:LambdaU = 15000.000000', 
                                'ExtraDimensionsUnpart:lambda = 1.0', 
                                'ExtraDimensionsUnpart:CutOffmode = 0', 
                                '5000039:m0 = 500.', 
                                '5000039:mWidth = 1000.', 
                                '5000039:mMin = 1.', 
                                '5000039:mMax = 13990.', 
                                'PartonLevel:MI = on', 
                                'SecondHard:generate = off', 
                                'SecondHard:TwoJets = off', 
                                'PhaseSpace:sameForSecond = off',
                                'PhaseSpace:pTHatMin = 30.', 
                                '23:onMode=off',
				'23:mMin=50', 
                                '23:onIfAny=11', 
                                'Tune:pp 5'),
                            parameterSets = cms.vstring('pythia8_unparticle')
                                 ) 
                         )
#ProductionFilterSequence = cms.Sequence(generator)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 0.1 $'),
    name = cms.untracked.string('$Source: Unpart_ZTo2E_SU_0_dU_2p20_LU_15_8TeV_Tune4C_pythia8_cfi.py,v $'),
    annotation = cms.untracked.string('PYTHIA8 Unpart Z/gamma* to 2E')
)

