import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    crossSection = cms.untracked.double(1508.),
    comEnergy = cms.double(14000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=6            !User defined processes', 
                                        'MSUB(81)=1         !qq to QQ',
					'MSUB(82)=1         !gg to QQ', 
#					'MSTP(7)=6',
					'MSUB(96)=0',
					'MDME(41,1)=0',
					'MDME(42,1)=0',
					'MDME(43,1)=0',
					'MDME(44,1)=0',
					'MDME(45,1)=0',
					'MDME(46,1)=1',
					'MDME(47,1)=0',
					'MDME(48,1)=0',
					'MDME(49,1)=0',
                                        ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/FourteenTeV/DYToLL_M_20_TuneZ2star_14TeV_pythia6_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 TT inclusive Tune Z2*')
)
