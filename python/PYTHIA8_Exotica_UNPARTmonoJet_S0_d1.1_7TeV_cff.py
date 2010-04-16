import FWCore.ParameterSet.Config as cms


generator = cms.EDFilter("Pythia8GeneratorFilter",
                                        maxEventsToPrint = cms.untracked.int32(1),
                                        pythiaPylistVerbosity = cms.untracked.int32(1),
                                        filterEfficiency = cms.untracked.double(1.0),
                                        pythiaHepMCVerbosity = cms.untracked.bool(False),
                                        comEnergy = cms.double(7000.),
                                        PythiaParameters = cms.PSet(
                                        pythia8_unparticle = cms.vstring( ## see details on http://home.thep.lu.se/~torbjorn/php8135/ExtraDimensionalProcesses.php?filepath=files/
                                               'ExtraDimensionsUnpart:monojet = on',
                                               'ExtraDimensionsUnpart:gg2Ug = on',
                                               'ExtraDimensionsUnpart:qg2Uq = on',
                                               'ExtraDimensionsUnpart:qqbar2Ug = on',
					       'ExtraDimensionsUnpart:spinU = 0',
					       'ExtraDimensionsUnpart:dU = 1.1',
					       'ExtraDimensionsUnpart:LambdaU = 1000.',
					       'ExtraDimensionsUnpart:lambda = 1.0',
					       'ExtraDimensionsUnpart:CutOffmode = 1',
					       '5000039:m0 = 300.',
					       '5000039:mWidth = 300.',
					       '5000039:mMin = 1.',
					       '5000039:mMax = 13990.',
					       'PhaseSpace:pTHatMin = 50.',
                                               'PartonLevel:MI = off',
                                               'PartonLevel:ISR = on',
                                               'PartonLevel:FSR = on'
                                        ), 
                                        parameterSets = cms.vstring('pythia8_unparticle')
                                 ) 
                         )


genParticles = cms.EDProducer(
    "GenParticleProducer",
                  src = cms.InputTag("generator"),
    abortOnUnknownPDGCode = cms.untracked.bool(False)
 )










        
