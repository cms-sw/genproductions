import FWCore.ParameterSet.Config as cms 
externalLHEProducer = cms.EDProducer('ExternalLHEProducer', 
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc6_amd64_gcc481/13TeV/powheg/V2/VBF_H_NNPDF30_13TeV/v1/VBFH_NNPDF30_M-125_13TeV_tarball.tar.gz'), 
    nEvents = cms.untracked.uint32(60000), 
    numberOfParameters = cms.uint32(1), 
    outputFile = cms.string('cmsgrid_final.lhe'), 
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh') 
) 

# Link to cards:
# https://github.com/cms-sw/genproductions/blob/6a68fe7bcc051fcc33f55dac960431fb3ffb1da7/bin/Powheg/examples/V2/VBF_H_NNPDF30_13TeV/VBF_H_NNPDF30_13TeV.input
 
import FWCore.ParameterSet.Config as cms 
from Configuration.Generator.Pythia8CommonSettings_cfi import * 
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import * 
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import * 
 
generator = cms.EDFilter('Pythia8HadronizerFilter', 
                         maxEventsToPrint = cms.untracked.int32(1), 
                         pythiaPylistVerbosity = cms.untracked.int32(1), 
                         filterEfficiency = cms.untracked.double(1.0), 
                         pythiaHepMCVerbosity = cms.untracked.bool(False), 
                         comEnergy = cms.double(13000.), 
                         PythiaParameters = cms.PSet( 
        pythia8CommonSettingsBlock, 
        pythia8CUEP8M1SettingsBlock, 
        pythia8PowhegEmissionVetoSettingsBlock, 
        processParameters = cms.vstring( 
            'POWHEG:nFinal = 3',   ## Number of final state particles 
                                   ## (BEFORE THE DECAYS) in the LHE 
                                   ## other than emitted extra parton 
            '9000006:all = sk   skbar    0        0          0       50  1.9732e-14  1.0  75.0 100', 
            '9000006:oneChannel = 1  1.0 101  5 -5', 
            '9000006:mayDecay = on', 
            '9000006:isResonance = on', 
            '25:m0 = 125.0', 
            '25:onMode = off', 
            '25:addChannel = 1 0.000000001 101 9000006 -9000006', 
            '25:onIfMatch = 9000006 -9000006', 
            '9000006:onMode = off', 
            '9000006:onIfAny = 5', 
        ), 
        parameterSets = cms.vstring('pythia8CommonSettings', 
                                    'pythia8CUEP8M1Settings', 
                                    'pythia8PowhegEmissionVetoSettings', 
                                    'processParameters' 
                                    ) 
        ) 
                         ) 
 
ProductionFilterSequence = cms.Sequence(generator) 

# Link to example generator fragment:
# https://github.com/lbenato/genproductions/blob/master/python/ThirteenTeV/HToSS/NLO_HToSSTobbbb_MH125_MS40_ctauS1_13TeV.py
