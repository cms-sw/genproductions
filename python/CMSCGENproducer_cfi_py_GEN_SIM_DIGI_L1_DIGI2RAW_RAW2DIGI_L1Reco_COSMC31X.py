# Auto generated configuration file
# using: 
# Revision: 1.168 
# Last update 09.04.2010 K.Hoepfner
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: GeneratorInterface/CosmicMuonGenerator/python/CMSCGENproducer_cfi.py -s GEN,SIM,DIGI,L1,DIGI2RAW,HLT,RAW2DIGI,L1Reco -n 10 --geometry DB --conditions auto:startup --eventcontent RECOSIM --scenario cosmics --no_exec
import FWCore.ParameterSet.Config as cms

process = cms.Process('HLT')

#--- import of standard configurations

process.load('Configuration.StandardSequences.Services_cff')
process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
process.load('FWCore.MessageService.MessageLogger_cfi')
process.load('Configuration.StandardSequences.MixingNoPileUp_cff')
process.load('Configuration.StandardSequences.GeometryDB_cff')
process.load('Configuration.StandardSequences.MagneticField_38T_cff')
process.load('Configuration.StandardSequences.Generator_cff')
process.load('Configuration.StandardSequences.VtxSmearedNoSmear_cff')
process.load('Configuration.StandardSequences.SimNOBEAM_cff')
process.load('Configuration.StandardSequences.DigiCosmics_cff')
process.load('Configuration.StandardSequences.SimL1Emulator_cff')
process.load('Configuration.StandardSequences.DigiToRaw_cff')
###process.load('HLTrigger.Configuration.HLT_8E29_cff')
process.load('Configuration.StandardSequences.RawToDigi_cff')
process.load('Configuration.StandardSequences.L1Reco_cff')
process.load('Configuration.StandardSequences.EndOfProcess_cff')
process.load('Configuration.StandardSequences.FrontierConditions_GlobalTag_cff')
process.load('Configuration.EventContent.EventContentCosmics_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.168 $'),
    annotation = cms.untracked.string('GeneratorInterface/CosmicMuonGenerator/python/CMSCGENproducer_cfi.py nevts:10'),
    name = cms.untracked.string('PyReleaseValidation')
)

#--- Events

process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(200)
)
process.options = cms.untracked.PSet(

)
#--- Input source

process.source = cms.Source("EmptySource")

#--- Output definition

process.output = cms.OutputModule("PoolOutputModule",
    splitLevel = cms.untracked.int32(0),
    outputCommands = process.RECOSIMEventContent.outputCommands,
    fileName = cms.untracked.string('CosmicMC_Underground_10GeV_COSMC31X.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string(''),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

#--- Additional output definition

# Other statements
process.GlobalTag.globaltag = 'COSMC3X_V1::All'


process.generator = cms.EDProducer("CosMuoGenProducer",
    MinTheta = cms.double(0.0),
    RhoPlug = cms.double(2.5),
    MinP = cms.double(10.0),
    MTCCHalf = cms.bool(False),
    MinPhi = cms.double(0.0),
    TIFOnly_constant = cms.bool(False),
    MaxP = cms.double(3000.0),
    NuProdAlt = cms.double(7500000.0),
    RhoAir = cms.double(0.001214),
    ZDistOfTarget = cms.double(15000.0),
    PlugVx = cms.double(0.0),
    PlugVz = cms.double(-14000.0),
    MultiMuonFileFirstEvent = cms.int32(1),
    MinT0 = cms.double(-12.5),
    RadiusOfTarget = cms.double(8000.0),
    AcptAllMu = cms.bool(False),
    TIFOnly_linear = cms.bool(False),
    MultiMuonNmin = cms.int32(2),
    MaxPhi = cms.double(360.0),
    Verbosity = cms.bool(False),
    MaxT0 = cms.double(12.5),
    ZCentrOfTarget = cms.double(0.0),
    ElossScaleFactor = cms.double(1.0),
    ClayWidth = cms.double(50000.0),
    MinP_CMS = cms.double(-1.0),
    RhoRock = cms.double(2.5),
    RhoClay = cms.double(2.3),
    TrackerOnly = cms.bool(False),
    MaxTheta = cms.double(84.26),
    MinEnu = cms.double(10.0),
    MaxEnu = cms.double(10000.0),
    MultiMuon = cms.bool(False),
    MultiMuonFileName = cms.string('CORSIKAmultiMuon.root'),
    RhoWall = cms.double(2.5)
)

#--- L1 trigger

process.load('Configuration.StandardSequences.SimL1Emulator_cff')
process.simDttfDigis.Open_LUTs = True
process.simCsctfTrackDigis.SectorProcessor.trigger_on_ME1a = True
process.simCsctfTrackDigis.SectorProcessor.trigger_on_ME1b = True
process.simCsctfTrackDigis.SectorProcessor.trigger_on_ME2  = True
process.simCsctfTrackDigis.SectorProcessor.trigger_on_ME3  = True
process.simCsctfTrackDigis.SectorProcessor.trigger_on_ME4  = True
process.simCsctfTrackDigis.SectorProcessor.trigger_on_MB1a = False
process.simCsctfTrackDigis.SectorProcessor.trigger_on_MB1d = False
process.simCsctfTrackDigis.SectorProcessor.singlesTrackPt  = 255
process.simCsctfTrackDigis.SectorProcessor.singlesTrackOutput = 1
process.simCsctfTrackDigis.useDT = False

#--- Path and EndPath definitions

process.generation_step = cms.Path(process.pgen)
process.simulation_step = cms.Path(process.psim)
process.digitisation_step = cms.Path(process.pdigi)
process.L1simulation_step = cms.Path(process.SimL1Emulator)
process.digi2raw_step = cms.Path(process.DigiToRaw)
process.raw2digi_step = cms.Path(process.RawToDigi)
process.L1Reco_step = cms.Path(process.L1Reco)
process.endjob_step = cms.Path(process.endOfProcess)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.simulation_step,process.digitisation_step,process.L1simulation_step,process.digi2raw_step)
###process.schedule.extend(process.HLTSchedule)
process.schedule.extend([process.raw2digi_step,process.L1Reco_step,process.endjob_step,process.out_step])

#--- special treatment in case of production filter sequence  
for path in process.paths: 
    getattr(process,path)._seq = process.generator*getattr(process,path)._seq
