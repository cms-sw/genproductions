# Auto generated configuration file
# using: 
# $Revision: 1.38 $
# $Source: /cvs/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v $
import FWCore.ParameterSet.Config as cms

process = cms.Process('HLT')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('Configuration/StandardSequences/Geometry_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/MagneticField_cff')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/VtxSmearedEarly10TeVCollision_cff')
process.load('Configuration/StandardSequences/Sim_cff')
process.load('Configuration/StandardSequences/Digi_cff')
process.load('Configuration/StandardSequences/SimL1Emulator_cff')
process.load('L1TriggerConfig/L1GtConfigProducers/Luminosity/lumi1030.L1Menu2008_2E30_Unprescaled_cff')
process.load('Configuration/StandardSequences/DigiToRaw_cff')
process.load('HLTrigger/Configuration/HLT_2E30_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContent_cff')

process.ReleaseValidation = cms.untracked.PSet(
    primaryDatasetName = cms.untracked.string('RelValConfiguration/GenProduction/python/PYTHIA6_PhotonJetpt_170_10TeV_cff_py_py'),
    totalNumberOfEvents = cms.untracked.int32(5000),
    eventsPerJob = cms.untracked.int32(250)
)
process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('PhotonJetpt-170 at 10TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_PhotonJetpt_170_10TeV_cff.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PythiaSource",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(51.43),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettings = cms.vstring('MSTJ(11)=3     ! Choice of the fragmentation function', 
            'MSTJ(22)=2     ! Decay those unstable particles', 
            'PARJ(71)=10 .  ! for which ctau  10 mm', 
            'MSTP(2)=1      ! which order running alphaS', 
            'MSTP(33)=0     ! no K factors in hard cross sections', 
            'MSTP(51)=10042     ! CTEQ6L1 structure function chosen', 
            'MSTP(52)=2     ! work with LHAPDF', 
            'MSTP(81)=1     ! multiple parton interactions 1 is Pythia default', 
            'MSTP(82)=4     ! Defines the multi-parton model', 
            'MSTU(21)=1     ! Check on possible errors during program execution', 
            'PARP(82)=1.8387   ! pt cutoff for multiparton interactions', 
            'PARP(89)=1960. ! sqrts for which PARP82 is set', 
            'PARP(83)=0.5   ! Multiple interactions: matter distrbn parameter', 
            'PARP(84)=0.4   ! Multiple interactions: matter distribution parameter', 
            'PARP(90)=0.16  ! Multiple interactions: rescaling power', 
            'PARP(67)=2.5    ! amount of initial-state radiation', 
            'PARP(85)=1.0  ! gluon prod. mechanism in MI', 
            'PARP(86)=1.0  ! gluon prod. mechanism in MI', 
            'PARP(62)=1.25   ! ', 
            'PARP(64)=0.2    ! ', 
            'MSTP(91)=1     !', 
            'PARP(91)=2.1   ! kt distribution', 
            'PARP(93)=15.0  ! '),
        processParameters = cms.vstring('MSEL=10', 
            'CKIN(3)=170  ! minimum pt hat for hard interactions'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('PYTHIA6_PhotonJetpt_170_10TeV_cff_py_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RAW'),
        filterName = cms.untracked.string('IDEAL_V5')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V5::All'

# Path and EndPath definitions
process.generation_step = cms.Path(process.pgen)
process.simulation_step = cms.Path(process.psim)
process.digitisation_step = cms.Path(process.pdigi)
process.L1simulation_step = cms.Path(process.SimL1Emulator)
process.digi2raw_step = cms.Path(process.DigiToRaw)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.simulation_step,process.digitisation_step,process.L1simulation_step,process.digi2raw_step,process.HLTriggerFirstPath,process.HLT_IsoElec5_TripleJet30,process.HLT_LooseIsoEle15_SW_L1R,process.HLT_IsoPhoton20_L1R,process.HLT_IsoTau_MET65_Trk20,process.HLT_IsoPhoton30_L1I,process.HLT_DoubleIsoEle12_L1R,process.HLT_Mu5_TripleJet30,process.HLT_MET75,process.HLT_DoubleIsoEle12_LW_L1R,process.HLT_L1Jet15,process.HLT_BTagIP_Jet120_Relaxed,process.HLT_CSCBeamHalo,process.HLT_Ele18_SW_L1R,process.HLT_Mu14_Jet50,process.HLT_IsoEle12_SW_L1R,process.HLT_LooseIsoTau_MET30,process.HLT_IsoMu9,process.HLT_IsoPhoton12_L1I,process.HLT_IsoEle12_IsoTau_Trk3,process.HLT_Ele10_SW_L1R,process.HLT_DoubleEle5_SW_L1R,process.HLT_MET65,process.HLT_EM200,process.HLT_LooseIsoPhoton30_L1R,process.HLT_DoubleMu3_SameSign,process.HLT_DoubleIsoTau_Trk3,process.HLT_LooseIsoPhoton40_L1R,process.HLT_Photon25_L1R,process.HLT_BackwardBSC,process.HLT_IsoEle18_LW_L1R,process.HLT_DoubleMu4_BJPsi,process.HLT_IsoMu7_BTagMu_Jet20,process.HLT_IsoEle15_L1I,process.HLT_BTagMu_DoubleJet100_Relaxed,process.HLT_DoubleLooseIsoPhoton20_L1R,process.HLT_BTagIP_HT420_Relaxed,process.HLT_Jet50,process.HLT_BTagIP_DoubleJet120,process.HLT_MinBiasPixel_Trk5,process.HLT_BTagMu_DoubleJet120,process.HLT_BTagMu_HT250_Relaxed,process.HLT_BTagMu_QuadJet30_Relaxed,process.HLT_Ele15_SW_L1R,process.HLT_DiJetAve130,process.HLT_Mu10,process.HLT_Mu11,process.HLT_IsoEle15_LW_L1I,process.HLT_NoTrackerIsoMu15,process.HLT_BTagIP_TripleJet40_Relaxed,process.HLT_DoubleMu3_BJPsi,process.HLT_Mu15_Vtx2mm,process.HLT_Jet110,process.HLT_DoubleEle12_LW_OnlyPixelM_L1R,process.HLT_DoubleJet40_MET70_Aco,process.HLT_Mu15_Vtx2cm,process.HLT_IsoPhoton10_L1R,process.HLT_DoubleIsoEle10_LW_L1I,process.HLT_Mu3,process.HLT_Mu5,process.HLT_Mu7,process.HLT_Mu9,process.HLT_Jet80_Jet20_MET60_NV,process.HLT_TrackerCosmics,process.AlCa_IsoTrack,process.HLT_BTagIP_QuadJet40,process.HLT_BTagIP_QuadJet30_Relaxed,process.HLT_Mu15_L1Mu7,process.HLT_DiJetAve15,process.HLT_QuadJet60,process.HLT_Jet180_MET60,process.HLT_BTagMu_TripleJet70,process.HLT_BTagIP_TripleJet70,process.HLT_Jet250,process.HLT_BTagMu_QuadJet40,process.HLT_TriggerType,process.HLT_IsoEle10_BTagIP_Jet35,process.HLT_LooseIsoEle18_LW_L1R,process.HLT_DoublePhoton8_L1R,process.HLT_DoubleFwdJet40_MET60,process.HLT_DoubleMu3,process.HLT_LooseIsoTau_MET30_L1MET,process.HLT_L1MET20,process.HLT_Photon40_L1R,process.HLT_IsoMu7_BTagIP_Jet35,process.HLT_BTagIP_Jet180,process.HLT_IsoEle12_DoubleJet80,process.HLT_BTagIP_HT320_Relaxed,process.HLT_IsoPhoton40_L1R,process.HLT_L1MuOpen,process.HLT_MET35_HT350,process.HLT_MinBiasEcal,process.HLT_IsoMu14_IsoTau_Trk3,process.HLT_DiJetAve30,process.HLT_DoubleIsoMu3,process.HLT_DoubleJet125_Aco,process.HLT_LooseIsoEle15_LW_L1R,process.HLT_Ele15_LW_L1R,process.HLT_CSCBeamHaloOverlapRing1,process.HLT_CSCBeamHaloOverlapRing2,process.HLT_LooseIsoPhoton20_L1R,process.HLT_DoubleEle6_Exclusive,process.HLT_MinBiasPixel,process.HLT_BTagIP_Jet160_Relaxed,process.HLT_BTagIP_QuadJet35_Relaxed,process.HLT_IsoEle8_IsoMu7,process.HLT_BTagIP_HT470,process.HLT_QuadJet30,process.HLT_Ele8_SW_L1R,process.HLT_Jet30,process.HLT_Jet100_MET60_Aco,process.HLT_IsoMu7_Jet40,process.HLT_IsoEle12_QuadJet35,process.HLT_NoL2IsoMu8_Jet40,process.HLT_BTagMu_TripleJet60_Relaxed,process.AlCa_EcalPi0,process.HLT_Jet60_MET70_Aco,process.HLT_DoublePhoton20_L1R,process.HLT_DiJetAve220,process.HLT_DiJetAve50,process.HLT_BTagIP_DoubleJet100_Relaxed,process.HLT_Mu13,process.HLT_Mu15,process.HLT_TripleMu3_TauTo3Mu,process.AlCa_EcalPhiSym,process.HLT_DoubleEle10_Z,process.HLT_DoubleMu3_Vtx2cm,process.HLT_BTagIP_TripleJet60_Relaxed,process.HLT_DoubleMu3_Vtx2mm,process.HLT_FwdJet20,process.HLT_DoubleJet150,process.HLT_TripleJet85,process.HLT_TripleMu3,process.HLT_DoubleEle8_LW_OnlyPixelM_L1R,process.HLT_IsoEle18_L1R,process.HLT_DoubleMu3_Psi2S,process.HLT_IsoPhoton25_L1R,process.HLT_BTagMu_HT330_Relaxed,process.HLT_IsoPhoton15_L1R,process.HLT_BTagMu_DoubleJet60_Relaxed,process.HLT_L1Mu,process.HLT_MET35,process.HLT_TripleJet60_MET60,process.HLT_IsoEle12_Jet40,process.HLT_LooseIsoPhoton45_L1R,process.HLT_BTagMu_TripleJet40_Relaxed,process.HLT_QuadJet35_MET60,process.HLT_DiJetAve70,process.HLT_DoubleMu7_Z,process.HLT_DoublePhoton10_L1R,process.HLT_Photon15_L1R,process.HLT_DoubleJet125_MET60,process.HLT_MET25,process.HLT_Jet180,process.HLT_DoubleLooseIsoTau,process.HLT_EM80,process.HLT_Photon30_L1R,process.HLT_BTagMu_HT370,process.HLT_SumET120,process.HLT_DoubleJet60_MET60_Aco,process.HLT_IsoEle10_Mu10_L1R,process.HLT_DoubleEle10_LW_OnlyPixelM_L1R,process.HLT_ForwardBSC,process.HLT_BTagMu_QuadJet35_Relaxed,process.HLT_DoubleMu3_Upsilon,process.HLT_BTagMu_Jet20_Calib,process.HLT_MET50,process.HLT_L2Mu9,process.HLT_IsoMu15,process.HLT_IsoMu11,process.HLT_IsoMu13,process.HLT_MinBiasHcal,process.HLT_IsoTau_MET35_Trk15_L1MET,process.HLT_CSCBeamHaloRing2or3,process.HLT_DoubleIsoEle10_L1I,process.HLT_DoubleJet50_MET70_Aco,process.HLT_DoubleMu3_JPsi,process.HLT_MinBias,process.HLT_DoublePhoton10_Exclusive,process.HLT_Jet80,process.HLT_BTagIP_DoubleJet60_Relaxed,process.HLT_Ele12_LW_L1R,process.HLT_IsoEle12_TripleJet60,process.HLT_DoubleIsoPhoton20_L1R,process.HLT_ZeroBias,process.HLT_DoubleIsoPhoton20_L1I,process.HLTriggerFinalPath,process.HLTEndpath1,process.out_step)
