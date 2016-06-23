import FWCore.ParameterSet.Config as cms
import sys

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *
from Configuration.Generator.Pythia8aMCatNLOSettings_cfi import *

process.generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
    pythia8CommonSettings = cms.vstring('Main:timesAllowErrors = 10000', 
        'Check:epTolErr = 0.01', 
        'Beams:setProductionScalesFromLHEF = off', 
        'SLHA:keepSM = on', 
        'SLHA:minMassSM = 1000.', 
        'ParticleDecays:limitTau0 = on', 
        'ParticleDecays:tau0Max = 10', 
        'ParticleDecays:allowPhotonRadiation = on'),
    pythia8CUEP8M1Settings = cms.vstring('Tune:pp 14', 
        'Tune:ee 7', 
        'MultipartonInteractions:pT0Ref=2.4024', 
        'MultipartonInteractions:ecmPow=0.25208', 
        'MultipartonInteractions:expPow=1.6'),
    pythia8aMCatNLOSettings = cms.vstring('SpaceShower:pTmaxMatch = 1', 
        'SpaceShower:pTmaxFudge = 1', 
        'SpaceShower:MEcorrections = off', 
        'TimeShower:pTmaxMatch = 1', 
        'TimeShower:pTmaxFudge = 1', 
        'TimeShower:MEcorrections = off', 
        'TimeShower:globalRecoil = on', 
        'TimeShower:limitPTmaxGlobal = on', 
        'TimeShower:nMaxGlobalRecoil = 1', 
        'TimeShower:globalRecoilMode = 2', 
        'TimeShower:nMaxGlobalBranch = 1'),
        processParameters = cms.vstring(
            'TimeShower:nPartonsInBorn = 1', #number of coloured particles (before resonance decays) in highest multiplicity born matrix element
            ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'pythia8aMCatNLOSettings',
                                    'processParameters',
                                    )
        )
)

