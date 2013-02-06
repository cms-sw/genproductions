import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8HadronizerFilter",
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    emissionVeto = cms.untracked.PSet(

    ),
    maxEventsToPrint = cms.untracked.int32(1),
    PythiaParameters = cms.PSet(
        pythia8_example07 = cms.vstring('SpaceShower:pTmaxMatch = 2',
            'TimeShower:pTmaxMatch  = 2',
            'TimeShower:MEcorrections = off',
            'SpaceShower:MEcorrections = off'),
        parameterSets = cms.vstring('pythia8_example07')
    )
)
