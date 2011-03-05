# Auto generated configuration file
# using: 
# Revision: 1.222.2.6 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: QCD_Pt_15_20_7TeV_cfi.py -s GEN,FASTSIM,HLT:GRun --pileup=LowLumiPileUp --geometry DB --conditions=auto:mc --eventcontent=RECOSIM --datatier GEN-SIM-DIGI-RECO -n 10 --no_exec
import FWCore.ParameterSet.Config as cms

process = cms.Process('HLT')

# import of standard configurations
process.load('Configuration.StandardSequences.Services_cff')
process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
process.load('FWCore.MessageService.MessageLogger_cfi')
process.load('FastSimulation.Configuration.RandomServiceInitialization_cff')
process.load('FastSimulation.Configuration.FamosSequences_cff')
process.load('FastSimulation.PileUpProducer.PileUpSimulator7TeV_cfi')
process.load('Configuration.StandardSequences.MagneticField_38T_cff')
process.load('Configuration.StandardSequences.Generator_cff')
process.load('IOMC.EventVertexGenerators.VtxSmearedParameters_cfi')
process.load('FastSimulation.Configuration.HLT_GRun_cff')
process.load('FastSimulation.Configuration.CommonInputs_cff')
process.load('FastSimulation.Configuration.EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.222.2.6 $'),
    annotation = cms.untracked.string('QCD_Pt_30_7TeV_cfi.py nevts:10'),
    name = cms.untracked.string('PyReleaseValidation')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000000)
)
process.options = cms.untracked.PSet(

)
# Input source
process.source = cms.Source("EmptySource")

# Output definition

process.RECOSIMoutput = cms.OutputModule("PoolOutputModule",
    splitLevel = cms.untracked.int32(0),
    outputCommands = process.RECOSIMEventContent.outputCommands,
    fileName = cms.untracked.string('QCD_7TeV_FASTSIM.root'),
    dataset = cms.untracked.PSet(
        filterName = cms.untracked.string(''),
        dataTier = cms.untracked.string('GEN-SIM-DIGI-RECO')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('gamgamfilter')
    )
)

# Additional output definition

# Other statements
process.famosPileUp.PileUpSimulator = process.PileUpSimulatorBlock.PileUpSimulator
process.famosPileUp.PileUpSimulator.averageNumber = 10.0
process.famosSimHits.SimulateCalorimetry = True
process.famosSimHits.SimulateTracking = True
process.simulation = cms.Sequence(process.simulationWithFamos)
process.HLTEndSequence = cms.Sequence(process.reconstructionWithFamos)

# set correct vertex smearing
process.Realistic7TeVCollisionVtxSmearingParameters.type = cms.string("BetaFunc")
process.famosSimHits.VertexGenerator = process.Realistic7TeVCollisionVtxSmearingParameters
process.famosPileUp.VertexGenerator = process.Realistic7TeVCollisionVtxSmearingParameters
# Apply Tracker and Muon misalignment
process.famosSimHits.ApplyAlignment = True
process.misalignedTrackerGeometry.applyAlignment = True

process.misalignedDTGeometry.applyAlignment = True
process.misalignedCSCGeometry.applyAlignment = True

process.GlobalTag.globaltag = 'MC_38Y_V13::All'
process.generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.000014),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(90000000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        pythiaUESettings = cms.vstring('MSTJ(11)=3     ! Choice of the fragmentation function', 
            'MSTJ(22)=2     ! Decay those unstable particles', 
            'PARJ(71)=10 .  ! for which ctau  10 mm', 
            'MSTP(2)=1      ! which order running alphaS', 
            'MSTP(33)=0     ! no K factors in hard cross sections', 
            'MSTP(51)=10042 ! structure function chosen (external PDF CTEQ6L1)', 
            'MSTP(52)=2     ! work with LHAPDF', 
            'MSTP(81)=1     ! multiple parton interactions 1 is Pythia default', 
            'MSTP(82)=1     ! Defines the multi-parton model', 
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
            'MSTP(91)=1      !', 
            'PARP(91)=2.1   ! kt distribution', 
            'PARP(93)=15.0  ! '),
        processParameters = cms.vstring('MSEL=1               ! QCD hight pT processes', 
            'CKIN(3)=30.          ! minimum pt hat for hard interactions', 
            'CKIN(4)=-1.          ! maximum pt hat for hard interactions'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

process.genfilter = cms.EDFilter('PythiaFilterGammaGamma',
                                      PtSeedThr = cms.untracked.double(5.),
                                      EtaSeedThr = cms.untracked.double(2.6),
                                      PtGammaThr = cms.untracked.double(0.),
                                      EtaGammaThr = cms.untracked.double(2.8),
                                      PtElThr = cms.untracked.double(2.0),
                                      EtaElThr = cms.untracked.double(2.6),
                                      dRSeedMax = cms.untracked.double(0.0),
                                      dPhiSeedMax = cms.untracked.double(0.3),
                                      dEtaSeedMax = cms.untracked.double(0.12),
                                      dRNarrowCone = cms.untracked.double(0.02),
                                      PtTkThr = cms.untracked.double(1.6),
                                      EtaTkThr = cms.untracked.double(2.2),
                                      dRTkMax = cms.untracked.double(-0.2),
                                      PtMinCandidate1 = cms.untracked.double(32.5),
                                      PtMinCandidate2 = cms.untracked.double(12.5),
                                      EtaMaxCandidate = cms.untracked.double(2.6),
                                      NTkConeMax = cms.untracked.int32(100),
                                      NTkConeSum = cms.untracked.int32(200),
                                      InvMassWide = cms.untracked.double(30),
                                      InvMassNarrow = cms.untracked.double(14000),
                                      AcceptPrompts = cms.untracked.bool(True),
                                      PromptPtThreshold = cms.untracked.double(15.0)
                         )

process.goodPhotonsLowEt = cms.EDFilter("PhotonSelector",
                                      src = cms.InputTag("photons"),
                                      cut = cms.string(
                                      "abs(superCluster.eta) < 2.5"
                                      " && superCluster.energy*sin(superCluster.position.theta) > 20."
                                      " && hadronicOverEm < 0.1 "
                                      " && trkSumPtHollowConeDR03 <  2.0*(3.5 + 0.0010*superCluster.energy*sin(superCluster.position.theta))"
                                      " && ecalRecHitSumEtConeDR03 < 2.0*(4.2 + 0.0060*superCluster.energy*sin(superCluster.position.theta))"
                                      " && hcalTowerSumEtConeDR03 <  2.0*(2.2 + 0.0025*superCluster.energy*sin(superCluster.position.theta))"
                                                      )
                                   )

process.goodPhotons40 = cms.EDFilter("PhotonSelector",
                                      src = cms.InputTag("photons"),
                                      cut = cms.string(
                                      "abs(superCluster.eta) < 2.5"
                                      " && superCluster.energy*sin(superCluster.position.theta) > 40."
                                      " && hadronicOverEm < 0.1 "
                                      " && trkSumPtHollowConeDR03 <  2.0*(3.5 + 0.0010*superCluster.energy*sin(superCluster.position.theta))"
                                      " && ecalRecHitSumEtConeDR03 < 2.0*(4.2 + 0.0060*superCluster.energy*sin(superCluster.position.theta))"
                                      " && hcalTowerSumEtConeDR03 <  2.0*(2.2 + 0.0025*superCluster.energy*sin(superCluster.position.theta))"
                                                      )
                                   )

process.TwoGoodPhotonsLowEt = cms.EDFilter("CandViewCountFilter",
                                       src = cms.InputTag("goodPhotonsLowEt"),
                                       minNumber = cms.uint32(2)
                                     )

process.OneGoodPhotons40 = cms.EDFilter("CandViewCountFilter",
                                       src = cms.InputTag("goodPhotons40"),
                                       minNumber = cms.uint32(1)
                                     )

process.options = cms.untracked.PSet(
    wantSummary = cms.untracked.bool(True)
)
process.load("FWCore.MessageLogger.MessageLogger_cfi")
process.MessageLogger.cerr.FwkReport.reportEvery = 500

# Path and EndPath definitions
process.ProductionFilterSequence = cms.Sequence(process.generator*process.genfilter)
process.generation_step = cms.Path(cms.SequencePlaceholder("randomEngineStateProducer")*process.ProductionFilterSequence)
process.reconstruction = cms.Path(process.reconstructionWithFamos)
process.gamgamfilter = cms.Path(process.goodPhotonsLowEt*process.TwoGoodPhotonsLowEt*process.goodPhotons40*process.OneGoodPhotons40)
process.RECOSIMoutput_step = cms.EndPath(process.RECOSIMoutput)

##HLT SCHEDULE DEFINED HERE TO REMOVE HLT ENDPATH. Copied from FastSimulation/Configuration/python/HLT_GRun_cff.py (MAY NOT HELP MUCH IN TIMING)
process.HLTSchedule = cms.Schedule( *(
    process.HLTriggerFirstPath, process.HLT_Activity_CSC, process.HLT_L1Jet6U,
    process.HLT_L1Jet10U, process.HLT_Jet15U, process.HLT_Jet30U,
    process.HLT_Jet50U, process.HLT_Jet70U, process.HLT_Jet100U,
    process.HLT_DiJetAve15U, process.HLT_DiJetAve30U, process.HLT_DiJetAve50U,
    process.HLT_DiJetAve70U, process.HLT_DoubleJet15U_ForwardBackward, process.HLT_DoubleJet25U_ForwardBackward,
    process.HLT_ExclDiJet30U, process.HLT_QuadJet15U, process.HLT_QuadJet20U,
    process.HLT_QuadJet25U, process.HLT_L1ETT100, process.HLT_EcalOnly_SumEt160,
    process.HLT_L1MET20, process.HLT_MET45, process.HLT_MET65,
    process.HLT_MET100, process.HLT_HT100U, process.HLT_HT120U,
    process.HLT_HT140U, process.HLT_L1MuOpen, process.HLT_L1MuOpen_DT,
    process.HLT_L1Mu, process.HLT_L1Mu20, process.HLT_L2Mu0_NoVertex,
    process.HLT_L2Mu0, process.HLT_L2Mu3, process.HLT_L2Mu9,
    process.HLT_L2Mu25, process.HLT_Mu3, process.HLT_Mu5,
    process.HLT_Mu7, process.HLT_Mu9, process.HLT_Mu11,
    process.HLT_IsoMu9, process.HLT_Mu20_NoVertex, process.HLT_L1DoubleMuOpen,
    process.HLT_L2DoubleMu0, process.HLT_DoubleMu0, process.HLT_DoubleMu3,
    process.HLT_Mu0_L1MuOpen, process.HLT_Mu3_L1MuOpen, process.HLT_Mu5_L1MuOpen,
    process.HLT_Mu0_L2Mu0, process.HLT_Mu5_L2Mu0, process.HLT_L1SingleEG2,
    process.HLT_L1SingleEG5, process.HLT_L1SingleEG8, process.HLT_L1DoubleEG5,
    process.HLT_Ele10_SW_L1R, process.HLT_Ele12_SW_TightEleId_L1R, process.HLT_Ele12_SW_TightEleIdIsol_L1R,
    process.HLT_Ele12_SW_TightEleIdIsol_NoDEtaInEE_L1R, process.HLT_Ele17_SW_L1R, process.HLT_Ele17_SW_CaloEleId_L1R,
    process.HLT_Ele17_SW_LooseEleId_L1R, process.HLT_Ele17_SW_EleId_L1R, process.HLT_Ele22_SW_CaloEleId_L1R,
    process.HLT_Ele40_SW_L1R, process.HLT_DoubleEle10_SW_L1R, process.HLT_Photon10_Cleaned_L1R,
    process.HLT_Photon15_Cleaned_L1R, process.HLT_Photon20_NoHE_L1R, process.HLT_Photon20_Cleaned_L1R,
    process.HLT_Photon30_Cleaned_L1R, process.HLT_Photon50_NoHE_L1R, process.HLT_Photon50_NoHE_Cleaned_L1R,
    process.HLT_DoublePhoton5_CEP_L1R, process.HLT_DoublePhoton5_L1R, process.HLT_DoublePhoton10_L1R,
    process.HLT_DoublePhoton15_L1R, process.HLT_DoublePhoton17_L1R, process.HLT_SingleIsoTau20_Trk5_MET20,
    process.HLT_SingleIsoTau20_Trk15_MET20, process.HLT_SingleIsoTau30_Trk5_MET20, process.HLT_SingleIsoTau30_Trk5_L120or30,
    process.HLT_DoubleIsoTau15_OneLeg_Trk5, process.HLT_DoubleIsoTau15_Trk5, process.HLT_BTagMu_Jet10U,
    process.HLT_BTagMu_Jet20U, process.HLT_StoppedHSCP, process.HLT_L2Mu5_Photon9_L1R,
    process.HLT_Mu5_Photon9_Cleaned_L1R, process.HLT_ZeroBias, process.HLT_ZeroBiasPixel_SingleTrack,
    process.HLT_MinBiasPixel_SingleTrack, process.HLT_MultiVertex6, process.HLT_MultiVertex8_L1ETT60,
    process.HLT_L1_BptxXOR_BscMinBiasOR, process.HLT_L1Tech_BSC_minBias_OR, process.HLT_L1Tech_BSC_minBias,
    process.HLT_L1Tech_BSC_halo, process.HLT_L1Tech_BSC_halo_forPhysicsBackground, process.HLT_L1Tech_BSC_HighMultiplicity,
    process.HLT_L1Tech_RPC_TTU_RBst1_collisions, process.HLT_L1Tech_HCAL_HF, process.HLT_TrackerCosmics,
    process.HLT_RPCBarrelCosmics, process.HLT_PixelTracks_Multiplicity70, process.HLT_PixelTracks_Multiplicity85,
    process.HLT_PixelTracks_Multiplicity100, process.HLT_GlobalRunHPDNoise, process.HLT_TechTrigHCALNoise,
    process.HLT_L1_BPTX, process.HLT_L1_BPTX_MinusOnly, process.HLT_L1_BPTX_PlusOnly,
    process.HLT_LogMonitor, process.DQM_TriggerResults, process.HLTriggerFinalPath
  ))

# Schedule definition
process.schedule = cms.Schedule(process.generation_step)
process.schedule.extend(process.HLTSchedule)
process.schedule.extend([process.reconstruction,process.gamgamfilter,process.RECOSIMoutput_step])

# special treatment in case of production filter sequence
for path in process.paths: 
    getattr(process,path)._seq = process.ProductionFilterSequence*getattr(process,path)._seq
