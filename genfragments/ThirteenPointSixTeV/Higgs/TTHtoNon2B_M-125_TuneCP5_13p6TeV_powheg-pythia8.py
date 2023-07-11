import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    #args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc7_amd64_gcc700/13TeV/powhegV2/ttH_inclusive_hdamp_NNPDF31_13TeV_M125/ttH_inclusive_hdamp_NNPDF31_13TeV_M125.tgz'),
    args = cms.vstring('/afs/cern.ch/user/m/moameen/public/gridpacks/ttH_PowhegP8/ttH_slc7_amd64_gcc10_CMSSW_12_4_8_tt_H.tgz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

#Link to datacards:
# For Run-2 https://github.com/cms-sw/genproductions/blob/0a0040eb78ea5cafe8287837d333869e2a676d3d/bin/Powheg/production/2017/13TeV/Higgs/ttH_inclusive_hdamp_NNPDF31_13TeV_M125/ttH_inclusive_hdamp_NNPDF31_13TeV_M125.input
# For Run-3: https://github.com/cms-sw/genproductions/blob/master/bin/Powheg/production/Run3/13p6TeV/Higgs/ttH_inclusive_hdamp_NNPDF31_13p6TeV_M125/ttH_inclusive_hdamp_NNPDF31_13p6TeV_M125.input

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentHadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13600.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PowhegEmissionVetoSettingsBlock,
        pythia8PSweightsSettingsBlock,
        processParameters = cms.vstring(
            'POWHEG:nFinal = 3',   ## Number of final state particles
                                   ## (BEFORE THE DECAYS) in the LHE
                                   ## other than emitted extra parton
            '23:mMin = 0.05',      
	    	'24:mMin = 0.05',      
            '25:m0 = 125.0',
            '25:onMode = on',	   
			'25:offIfAny = 5 -5' #with previous line: decays to all, except bb pair
          ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
				    'pythia8PowhegEmissionVetoSettings',
                    		    'pythia8PSweightsSettings',
                                    'processParameters'
                                    )
        )
 )

ProductionFilterSequence = cms.Sequence(generator)

