# Auto generated configuration file
# using: 
# Revision: 1.77.2.1 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/python/PYTHIA6_Tauola_MSSM_bbA_2tau_2l_mA115_10TeV_cff.py -s GEN,SIM,DIGI,L1,DIGI2RAW,HLT --eventcontent RAWSIM --datatier GEN-SIM-RAW --conditions FrontierConditions_GlobalTag,IDEAL_V9::All -n 10 --no_exec
import FWCore.ParameterSet.Config as cms

process = cms.Process('HLT')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryPilot2_cff')
process.load('Configuration/StandardSequences/MagneticField_38T_cff')
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

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    annotation = cms.untracked.string('PYTHIA6 bbA->tautau->2l mA=115GeV with TAUOLA at 10TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Tauola_MSSM_bbA_2tau_2l_mA115_10TeV_cff.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(10)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PythiaSource",
    UseExternalGenerators = cms.untracked.bool(True),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    ExternalGenerators = cms.PSet(
        Tauola = cms.untracked.PSet(
            UseTauolaPolarization = cms.bool(True),
            TauolaDefaultInputCards = cms.PSet(
                InputCards = cms.vstring('TAUOLA = 1 2 0 ! TAUOLA ')
            )
        ),
        parameterSets = cms.vstring('Tauola')
    ),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(55000000000.0),
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
        processParameters = cms.vstring('MSEL=0         ! User defined processes', 
            'MSUB(186)= 1   ! gg->QQbarH (MSSM)', 
            'KFPR(186,2)= 5 ! Q = b Registered by Alexandre.Nikitenko@cern.ch', 
            'IMSS(1)= 1     ! MSSM with parameters', 
            'RMSS(5)= 30.   ! tan beta', 
            'RMSS(19)= 115. ! m_A', 
            'MDME(420,1)=0  ! Higgs(H) decay into d              dbar', 
            'MDME(421,1)=0  ! Higgs(H) decay into u              ubar', 
            'MDME(422,1)=0  ! Higgs(H) decay into s              sbar', 
            'MDME(423,1)=0  ! Higgs(H) decay into c              cbar', 
            'MDME(424,1)=0  ! Higgs(H) decay into b              bbar', 
            'MDME(425,1)=0  ! Higgs(H) decay into t              tbar', 
            'MDME(426,1)=0  ! Higgs(H) decay into b              bbar', 
            'MDME(427,1)=0  ! Higgs(H) decay into t              tbar', 
            'MDME(428,1)=0  ! Higgs(H) decay into e-             e+', 
            'MDME(429,1)=0  ! Higgs(H) decay into mu-            mu+', 
            'MDME(430,1)=1  ! Higgs(H) decay into tau-           tau+', 
            'MDME(431,1)=0  ! Higgs(H) decay into tau-           tau+', 
            'MDME(432,1)=0  ! Higgs(H) decay into g              g', 
            'MDME(433,1)=0  ! Higgs(H) decay into gamma          gamma', 
            'MDME(434,1)=0  ! Higgs(H) decay into gamma          Z0', 
            'MDME(435,1)=0  ! Higgs(H) decay into Z0             Z0', 
            'MDME(436,1)=0  ! Higgs(H) decay into W+             W-', 
            'MDME(437,1)=0  ! Higgs(H) decay into Z0             h0', 
            'MDME(438,1)=0  ! Higgs(H) decay into h0             h0', 
            'MDME(439,1)=0  ! Higgs(H) decay into W+             H-', 
            'MDME(440,1)=0  ! Higgs(H) decay into H+             W-', 
            'MDME(441,1)=0  ! Higgs(H) decay into Z0             A0', 
            'MDME(442,1)=0  ! Higgs(H) decay into h0             A0', 
            'MDME(443,1)=0  ! Higgs(H) decay into A0             A0', 
            'MDME(444,1)=0  ! Higgs(H) decay into ~chi_10        ~chi_10', 
            'MDME(445,1)=0  ! Higgs(H) decay into ~chi_20        ~chi_10', 
            'MDME(446,1)=0  ! Higgs(H) decay into ~chi_20        ~chi_20', 
            'MDME(447,1)=0  ! Higgs(H) decay into ~chi_30        ~chi_10', 
            'MDME(448,1)=0  ! Higgs(H) decay into ~chi_30        ~chi_20', 
            'MDME(449,1)=0  ! Higgs(H) decay into ~chi_30        ~chi_30', 
            'MDME(450,1)=0  ! Higgs(H) decay into ~chi_40        ~chi_10', 
            'MDME(451,1)=0  ! Higgs(H) decay into ~chi_40        ~chi_20', 
            'MDME(452,1)=0  ! Higgs(H) decay into ~chi_40        ~chi_30', 
            'MDME(453,1)=0  ! Higgs(H) decay into ~chi_40        ~chi_40', 
            'MDME(454,1)=0  ! Higgs(H) decay into ~chi_1+        ~chi_1-', 
            'MDME(455,1)=0  ! Higgs(H) decay into ~chi_1+        ~chi_2-', 
            'MDME(456,1)=0  ! Higgs(H) decay into ~chi_2+        ~chi_1-', 
            'MDME(457,1)=0  ! Higgs(H) decay into ~chi_2+        ~chi_2-', 
            'MDME(458,1)=0  ! Higgs(H) decay into ~d_L           ~d_Lbar', 
            'MDME(459,1)=0  ! Higgs(H) decay into ~d_R           ~d_Rbar', 
            'MDME(460,1)=0  ! Higgs(H) decay into ~d_L           ~d_Rbar', 
            'MDME(461,1)=0  ! Higgs(H) decay into ~d_Lbar        ~d_R', 
            'MDME(462,1)=0  ! Higgs(H) decay into ~u_L           ~u_Lbar', 
            'MDME(463,1)=0  ! Higgs(H) decay into ~u_R           ~u_Rbar', 
            'MDME(464,1)=0  ! Higgs(H) decay into ~u_L           ~u_Rbar', 
            'MDME(465,1)=0  ! Higgs(H) decay into ~u_Lbar        ~u_R', 
            'MDME(466,1)=0  ! Higgs(H) decay into ~s_L           ~s_Lbar', 
            'MDME(467,1)=0  ! Higgs(H) decay into ~s_R           ~s_Rbar', 
            'MDME(468,1)=0  ! Higgs(H) decay into ~s_L           ~s_Rbar', 
            'MDME(469,1)=0  ! Higgs(H) decay into ~s_Lbar        ~s_R', 
            'MDME(470,1)=0  ! Higgs(H) decay into ~c_L           ~c_Lbar', 
            'MDME(471,1)=0  ! Higgs(H) decay into ~c_R           ~c_Rbar', 
            'MDME(472,1)=0  ! Higgs(H) decay into ~c_L           ~c_Rbar', 
            'MDME(473,1)=0  ! Higgs(H) decay into ~c_Lbar        ~c_R', 
            'MDME(474,1)=0  ! Higgs(H) decay into ~b_1           ~b_1bar', 
            'MDME(475,1)=0  ! Higgs(H) decay into ~b_2           ~b_2bar', 
            'MDME(476,1)=0  ! Higgs(H) decay into ~b_1           ~b_2bar', 
            'MDME(477,1)=0  ! Higgs(H) decay into ~b_1bar        ~b_2', 
            'MDME(478,1)=0  ! Higgs(H) decay into ~t_1           ~t_1bar', 
            'MDME(479,1)=0  ! Higgs(H) decay into ~t_2           ~t_2bar', 
            'MDME(480,1)=0  ! Higgs(H) decay into ~t_1           ~t_2bar', 
            'MDME(481,1)=0  ! Higgs(H) decay into ~t_1bar        ~t_2', 
            'MDME(482,1)=0  ! Higgs(H) decay into ~e_L-          ~e_L+', 
            'MDME(483,1)=0  ! Higgs(H) decay into ~e_R-          ~e_R+', 
            'MDME(484,1)=0  ! Higgs(H) decay into ~e_L-          ~e_R+', 
            'MDME(485,1)=0  ! Higgs(H) decay into ~e_L+          ~e_R-', 
            'MDME(486,1)=0  ! Higgs(H) decay into ~nu_eL         ~nu_eLbar', 
            'MDME(487,1)=0  ! Higgs(H) decay into ~nu_eR         ~nu_eRbar', 
            'MDME(488,1)=0  ! Higgs(H) decay into ~nu_eL         ~nu_eRbar', 
            'MDME(489,1)=0  ! Higgs(H) decay into ~nu_eLbar      ~nu_eR', 
            'MDME(490,1)=0  ! Higgs(H) decay into ~mu_L-         ~mu_L+', 
            'MDME(491,1)=0  ! Higgs(H) decay into ~mu_R-         ~mu_R+', 
            'MDME(492,1)=0  ! Higgs(H) decay into ~mu_L-         ~mu_R+', 
            'MDME(493,1)=0  ! Higgs(H) decay into ~mu_L+         ~mu_R-', 
            'MDME(494,1)=0  ! Higgs(H) decay into ~nu_muL        ~nu_muLbar', 
            'MDME(495,1)=0  ! Higgs(H) decay into ~nu_muR        ~nu_muRbar', 
            'MDME(496,1)=0  ! Higgs(H) decay into ~nu_muL        ~nu_muRbar', 
            'MDME(497,1)=0  ! Higgs(H) decay into ~nu_muLbar     ~nu_muR', 
            'MDME(498,1)=0  ! Higgs(H) decay into ~tau_1-        ~tau_1+', 
            'MDME(499,1)=0  ! Higgs(H) decay into ~tau_1-        ~tau_1+', 
            'MDME(500,1)=0  ! Higgs(H) decay into ~tau_1-        ~tau_1+', 
            'MDME(501,1)=0  ! Higgs(H) decay into ~tau_1-        ~tau_1+', 
            'MDME(502,1)=0  ! Higgs(H) decay into ~tau_2-        ~tau_2+'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('PYTHIA6_Tauola_MSSM_bbA_2tau_2l_mA115_10TeV_cff_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN-SIM-RAW'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V9::All'

# Path and EndPath definitions
process.generation_step = cms.Path(process.pgen)
process.simulation_step = cms.Path(process.psim)
process.digitisation_step = cms.Path(process.pdigi)
process.L1simulation_step = cms.Path(process.SimL1Emulator)
process.digi2raw_step = cms.Path(process.DigiToRaw)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.simulation_step,process.digitisation_step,process.L1simulation_step,process.digi2raw_step,process.HLTriggerFirstPath,process.HLT_L1Jet15,process.HLT_Jet30,process.HLT_Jet50,process.HLT_Jet80,process.HLT_Jet110,process.HLT_Jet180,process.HLT_Jet250,process.HLT_FwdJet20,process.HLT_DoubleJet150,process.HLT_DoubleJet125_Aco,process.HLT_DoubleFwdJet50,process.HLT_DiJetAve15,process.HLT_DiJetAve30,process.HLT_DiJetAve50,process.HLT_DiJetAve70,process.HLT_DiJetAve130,process.HLT_DiJetAve220,process.HLT_TripleJet85,process.HLT_QuadJet30,process.HLT_QuadJet60,process.HLT_SumET120,process.HLT_L1MET20,process.HLT_MET25,process.HLT_MET35,process.HLT_MET50,process.HLT_MET65,process.HLT_MET75,process.HLT_MET65_HT350,process.HLT_Jet180_MET60,process.HLT_Jet60_MET70_Aco,process.HLT_Jet100_MET60_Aco,process.HLT_DoubleJet125_MET60,process.HLT_DoubleFwdJet40_MET60,process.HLT_DoubleJet60_MET60_Aco,process.HLT_DoubleJet50_MET70_Aco,process.HLT_DoubleJet40_MET70_Aco,process.HLT_TripleJet60_MET60,process.HLT_QuadJet35_MET60,process.HLT_IsoEle15_L1I,process.HLT_IsoEle18_L1R,process.HLT_IsoEle15_LW_L1I,process.HLT_LooseIsoEle15_LW_L1R,process.HLT_Ele10_SW_L1R,process.HLT_Ele15_SW_L1R,process.HLT_Ele15_LW_L1R,process.HLT_EM80,process.HLT_EM200,process.HLT_DoubleIsoEle10_L1I,process.HLT_DoubleIsoEle12_L1R,process.HLT_DoubleIsoEle10_LW_L1I,process.HLT_DoubleIsoEle12_LW_L1R,process.HLT_DoubleEle5_SW_L1R,process.HLT_DoubleEle10_LW_OnlyPixelM_L1R,process.HLT_DoubleEle10_Z,process.HLT_DoubleEle6_Exclusive,process.HLT_IsoPhoton30_L1I,process.HLT_IsoPhoton10_L1R,process.HLT_IsoPhoton15_L1R,process.HLT_IsoPhoton20_L1R,process.HLT_IsoPhoton25_L1R,process.HLT_IsoPhoton40_L1R,process.HLT_Photon15_L1R,process.HLT_Photon25_L1R,process.HLT_DoubleIsoPhoton20_L1I,process.HLT_DoubleIsoPhoton20_L1R,process.HLT_DoublePhoton10_Exclusive,process.HLT_L1Mu,process.HLT_L1MuOpen,process.HLT_L2Mu9,process.HLT_IsoMu9,process.HLT_IsoMu11,process.HLT_IsoMu13,process.HLT_IsoMu15,process.HLT_Mu3,process.HLT_Mu5,process.HLT_Mu7,process.HLT_Mu9,process.HLT_Mu11,process.HLT_Mu13,process.HLT_Mu15,process.HLT_Mu15_L1Mu7,process.HLT_Mu15_Vtx2cm,process.HLT_Mu15_Vtx2mm,process.HLT_DoubleIsoMu3,process.HLT_DoubleMu3,process.HLT_DoubleMu3_Vtx2cm,process.HLT_DoubleMu3_Vtx2mm,process.HLT_DoubleMu3_JPsi,process.HLT_DoubleMu3_Upsilon,process.HLT_DoubleMu7_Z,process.HLT_DoubleMu3_SameSign,process.HLT_DoubleMu3_Psi2S,process.HLT_BTagIP_Jet180,process.HLT_BTagIP_Jet120_Relaxed,process.HLT_BTagIP_DoubleJet120,process.HLT_BTagIP_DoubleJet60_Relaxed,process.HLT_BTagIP_TripleJet70,process.HLT_BTagIP_TripleJet40_Relaxed,process.HLT_BTagIP_QuadJet40,process.HLT_BTagIP_QuadJet30_Relaxed,process.HLT_BTagIP_HT470,process.HLT_BTagIP_HT320_Relaxed,process.HLT_BTagMu_DoubleJet120,process.HLT_BTagMu_DoubleJet60_Relaxed,process.HLT_BTagMu_TripleJet70,process.HLT_BTagMu_TripleJet40_Relaxed,process.HLT_BTagMu_QuadJet40,process.HLT_BTagMu_QuadJet30_Relaxed,process.HLT_BTagMu_HT370,process.HLT_BTagMu_HT250_Relaxed,process.HLT_DoubleMu3_BJPsi,process.HLT_DoubleMu4_BJPsi,process.HLT_TripleMu3_TauTo3Mu,process.HLT_IsoTau_MET65_Trk20,process.HLT_IsoTau_MET35_Trk15_L1MET,process.HLT_LooseIsoTau_MET30,process.HLT_LooseIsoTau_MET30_L1MET,process.HLT_DoubleIsoTau_Trk3,process.HLT_DoubleLooseIsoTau,process.HLT_IsoEle8_IsoMu7,process.HLT_IsoEle10_Mu10_L1R,process.HLT_IsoEle12_IsoTau_Trk3,process.HLT_IsoEle10_BTagIP_Jet35,process.HLT_IsoEle12_Jet40,process.HLT_IsoEle12_DoubleJet80,process.HLT_IsoEle5_TripleJet30,process.HLT_IsoEle12_TripleJet60,process.HLT_IsoEle12_QuadJet35,process.HLT_IsoMu14_IsoTau_Trk3,process.HLT_IsoMu7_BTagIP_Jet35,process.HLT_IsoMu7_BTagMu_Jet20,process.HLT_IsoMu7_Jet40,process.HLT_NoL2IsoMu8_Jet40,process.HLT_Mu14_Jet50,process.HLT_Mu5_TripleJet30,process.HLT_BTagMu_Jet20_Calib,process.HLT_ZeroBias,process.HLT_MinBias,process.HLT_MinBiasHcal,process.HLT_MinBiasEcal,process.HLT_MinBiasPixel,process.HLT_MinBiasPixel_Trk5,process.HLT_BackwardBSC,process.HLT_ForwardBSC,process.HLT_CSCBeamHalo,process.HLT_CSCBeamHaloOverlapRing1,process.HLT_CSCBeamHaloOverlapRing2,process.HLT_CSCBeamHaloRing2or3,process.HLT_TrackerCosmics,process.AlCa_IsoTrack,process.AlCa_EcalPhiSym,process.AlCa_EcalPi0,process.HLTriggerFinalPath,process.out_step)
