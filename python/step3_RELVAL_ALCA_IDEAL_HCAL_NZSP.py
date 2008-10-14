# Auto generated configuration file
# using: 
# Revision: 1.77.2.1 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: step3_RELVAL -s ALCA:Configuration/StandardSequences/AlCaRecoStream_HcalCalMinBias_cff:HcalCalMinBias+DQM -n 1000 --filein file:/tmp/safronov/CMSSW_2_1_9/work/AlCaRawHcalPhiSymStream.root --conditions FrontierConditions_GlobalTag,IDEAL_V9::All --no_exec
import FWCore.ParameterSet.Config as cms

process = cms.Process('ALCA')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryPilot2_cff')
process.load('Configuration/StandardSequences/MagneticField_38T_cff')
process.load('Configuration/StandardSequences/AlCaRecoStream_HcalCalMinBias_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.77.2.1 $'),
    annotation = cms.untracked.string('step3_RELVAL nevts:1000'),
    name = cms.untracked.string('PyReleaseValidation')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PoolSource",
    fileNames = cms.untracked.vstring(
'file:PYTHIA6_MinBias_10TeV_cff_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root'
)
)

# Additional output definition
process.ALCARECOStreamHcalCalMinBias = cms.OutputModule("PoolOutputModule",
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('pathALCARECOHcalCalMinBias')
    ),
    outputCommands = cms.untracked.vstring('drop *', 
        'keep HBHERecHitsSorted_hbherecoMB_*_*', 
        'keep HORecHitsSorted_horecoMB_*_*', 
        'keep HFRecHitsSorted_hfrecoMB_*_*', 
        'keep HBHERecHitsSorted_hbherecoNoise_*_*', 
        'keep HORecHitsSorted_horecoNoise_*_*', 
        'keep HFRecHitsSorted_hfrecoNoise_*_*'),
    fileName = cms.untracked.string('ALCARECOHcalCalMinBias.root'),
    dataset = cms.untracked.PSet(
        filterName = cms.untracked.string('StreamALCARECOHcalCalMinBias'),
        dataTier = cms.untracked.string('ALCARECO')
    )
)

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V9::All'

# Path and EndPath definitions
process.pathALCARECODQM = cms.Path(process.MEtoEDMConverter)
process.pathALCARECOHcalCalMinBias = cms.Path(process.seqALCARECOHcalCalMinBias)
process.ALCARECOStreamHcalCalMinBiasOutPath = cms.EndPath(process.ALCARECOStreamHcalCalMinBias)

# Schedule definition
process.schedule = cms.Schedule(process.pathALCARECODQM,process.pathALCARECOHcalCalMinBias,process.ALCARECOStreamHcalCalMinBiasOutPath)
