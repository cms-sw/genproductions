from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
                         maxEventsToPrint = cms.untracked.int32(10),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         filterEfficiency = cms.untracked.double(1.),
                         comEnergy = cms.double(7000.0),
                         crossSection = cms.untracked.double(1.178e-3),
                         PythiaParameters = cms.PSet(
    pythiaUESettingsBlock,
    processParameters = cms.vstring(
    'MSEL          =0    !User defined processes',
    'MSUB(169)     =1    !qq~ -> e*e',
    'PMAS(345,1)   =600  !m*',
    'RTCM(41)      =10000 !lambda',
    'MDME( 4079,1) =1    !e* decay into e+gamma',
    'MDME( 4080,1) =0    !e* decay into z+gamma',
    'MDME( 4081,1) =0    !e* decay into w+nu'
    ),
    # This is a vector of ParameterSet names to be read, in this order
    parameterSets = cms.vstring('pythiaUESettings',
                                'processParameters')
    )
                         )

ProductionFilterSequence = cms.Sequence(generator)
