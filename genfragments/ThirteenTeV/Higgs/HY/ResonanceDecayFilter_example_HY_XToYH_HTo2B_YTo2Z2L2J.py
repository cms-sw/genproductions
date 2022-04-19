import FWCore.ParameterSet.Config as cms

# Fragment example from central X->HH->WWgg request: https://github.com/cms-sw/genproductions/blob/dace43b7b79c6c7d01d80bb17b96db6d84326830/genfragments/ThirteenTeV/Higgs/HH/ResonanceDecayFilter_example_HHTo2G2WTo2G2Q1L1Nu_madgraph_pythia8_CP5_cff.py

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/afs/cern.ch/work/v/vinguyen/HHWWgg_Tools/NMSSM/NMSSM_XToYH_MX_500_MY_300_slc7_amd64_gcc700_CMSSW_10_6_19_tarball.tar.xz'), ##-- Locally produced gridpack
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            '15:onMode = off',
            '15:onIfAny = 11 13', # only leptonic tau decays
            '23:mMin = 0.05',
            '23:onMode = off',
            '23:onIfAny = 1 2 3 4 5 11 13 15', # Z->jets decay and a leptonic charged Z decay, including taus
            '35:onMode = off',
            '35:oneChannel = 1 1 100 23 23',  # Y->ZZ
            '35:onIfMatch = 23 23',
            '24:mMin = 0.05',
            '24:onMode = off',
            '25:m0 = 125.0',
            '25:onMode = off',
            '25:oneChannel = 1 1 100 5 -5', # H->bb
            '25:onIfMatch = 5 -5',
            'ResonanceDecayFilter:filter = on',
            'ResonanceDecayFilter:exclusive = on', #off: require at least the specified number of daughters, on: require exactly the specified number of daughters
            'ResonanceDecayFilter:eMuAsEquivalent = off', #on: treat electrons and muons as equivalent
            'ResonanceDecayFilter:eMuTauAsEquivalent = on', #on: treat electrons, muons , and taus as equivalent
            'ResonanceDecayFilter:allNuAsEquivalent  = off', #on: treat all three neutrino flavours as equivalent
            'ResonanceDecayFilter:udscAsEquivalent   = off', #on: treat udsc quarks as equivalent
            'ResonanceDecayFilter:udscbAsEquivalent  = on',  #on: treat udscb quarks as equivalent
            'ResonanceDecayFilter:mothers = 35,25,23', #list of mothers not specified -> count all particles in hard process+resonance decays (better to avoid specifying mothers when including leptons from the lhe in counting, since intermediate resonances are not gauranteed to appear in general
            'ResonanceDecayFilter:daughters = 5,5,23,23,1,1,11,11',
          ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'pythia8PSweightsSettings',
                                    'processParameters'
                                    )
        )
                         )


ProductionFilterSequence = cms.Sequence(generator)
