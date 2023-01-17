#this is based on: https://raw.githubusercontent.com/jieunyoo/genproductions/master/genfragments/ThirteenTeV/Hadronizer/Hadronizer_TuneCP5Up_13TeV_powhegEmissionVeto_3p_LHE_pythia8_cff.py
#it just adds a new filter at the end

import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5TuneUpSettings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentHadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5TuneUpSettingsBlock,
        pythia8PowhegEmissionVetoSettingsBlock,
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            'POWHEG:nFinal = 3',   ## Number of final state particles
                                   ## (BEFORE THE DECAYS) in the LHE
                                   ## other than emitted extra parton
          ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5TuneUpSettings',
                                    'pythia8PowhegEmissionVetoSettings',
                                    'pythia8PSweightsSettings',
                                    'processParameters'
                                    )
        )
                         )

#filter Higgs pt
ProductionFilterSequence = cms.Sequence(generator)
LHEHiggsPtFilter = cms.EDFilter("LHEPtFilter",
  selectedPdgIds = cms.vint32(25),
  ptMin=cms.double(135.),
  ptMax=cms.double(1e10),
  src=cms.InputTag("externalLHEProducer")
)

ProductionFilterSequence = cms.Sequence(LHEHiggsPtFilter+generator)
