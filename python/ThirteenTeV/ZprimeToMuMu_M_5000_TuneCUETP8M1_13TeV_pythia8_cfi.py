import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

generator = cms.EDFilter("Pythia8GeneratorFilter",
        comEnergy = cms.double(13000.0),
        crossSection = cms.untracked.double(5.511e-5),
        filterEfficiency = cms.untracked.double(1),
        maxEventsToPrint = cms.untracked.int32(0),
        pythiaHepMCVerbosity = cms.untracked.bool(False),
        pythiaPylistVerbosity = cms.untracked.int32(1),
        PythiaParameters = cms.PSet(
              pythia8CommonSettingsBlock,
              pythia8CUEP8M1SettingsBlock,
              
              processParameters = cms.vstring(
                        'NewGaugeBoson:ffbar2gmZZprime = on',
                        'Zprime:gmZmode = 3', # only pure Z' contribution
                        '32:m0 = 5000',
                        '32:onMode = off', # switch off all of the Z' decay
                        '32:onIfAny = 13', # switch on the Z'->mu-mu+
                ),
                parameterSets = cms.vstring('pythia8CommonSettingsBlock',
                                            'pythia8CUEP8M1SettingsBlock',
                                            'processParameters')
              )
)

ProductionFilterSequence = cms.Sequence(generator)
