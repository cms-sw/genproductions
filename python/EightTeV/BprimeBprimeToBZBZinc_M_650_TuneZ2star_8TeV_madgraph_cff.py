import FWCore.ParameterSet.Config as cms

#from Configuration.Generator.PythiaUEZ2Settings_cfi import *
from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    comEnergy = cms.double(8000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring(
	    'PMAS(25,1)=125.00D0    !mass of Higgs', 
            'MSTP(1) = 4',
            'MSEL=7         ! User defined processes',
	    'MWID(7)=2',
            'MSTJ(1)=1       ! Fragmentation/hadronization on or off',
            'MSTP(61)=1      ! Parton showering on or off',
            'PMAS(5,1)=4.8   ! b quark mass', #from Spring11 4000040
            'PMAS(6,1)=172.5 ! t quark mass', #from Spring11 4000040
            'PMAS(7,1) = 650.0D0         ! bprime quarks mass',
            'PMAS(7,2) = 6.500D0        ! bprime quark width',
            'PMAS(7,3) = 65.00D0 ! Max value above which the BW shape is truncated', 
            'VCKM(1,1) = 0.97414000D0',
            'VCKM(1,2) = 0.22450000D0',
            'VCKM(1,3) = 0.00420000D0',
            'VCKM(1,4) = 0.02500000D0',
            'VCKM(2,1) = 0.22560000D0',
            'VCKM(2,2) = 0.97170000D0',
            'VCKM(2,3) = 0.04109000D0',
            'VCKM(2,4) = 0.05700000D0',
            'VCKM(3,1) = 0.00100000D0',
            'VCKM(3,2) = 0.06200000D0',
            'VCKM(3,3) = 0.91000000D0',
            'VCKM(3,4) = 0.41000000D0',
            'VCKM(4,1) = 0.01300000D0',
            'VCKM(4,2) = 0.04000000D0',
            'VCKM(4,3) = 0.41000000D0',
            'VCKM(4,4) = 0.91000000D0',
            'MDME(56,1)=0     ! g b4',
            'MDME(57,1)=0     ! gamma b4',
	    'KFDP(58,2)=5     ! defines Z0 b',
            'MDME(58,1)=1     ! Z0 b',
            'MDME(59,1)=0     ! W u',
            'MDME(60,1)=0     ! W c',
            'MDME(61,1)=0     ! W t',
            'MDME(62,1)=0     ! W t4',
	    'KFDP(63,2)=0     ! defines H0 b',
            'MDME(63,1)=0     ! h0 b4',
            'MDME(64,1)=-1    ! H- c',
            'MDME(65,1)=-1    ! H- t',
            'BRAT(56)  = 0.0D0',
            'BRAT(57)  = 0.0D0',
            'BRAT(58)  = 1.0D0',
            'BRAT(59)  = 0.0D0',
            'BRAT(60)  = 0.0D0',
            'BRAT(61)  = 0.0D0',
            'BRAT(62)  = 0.0D0',
            'BRAT(63)  = 0.0D0',
            'BRAT(64)  = 0.0D0',
            'BRAT(65)  = 0.0D0',
            'MDME(174,1)=1     !Z decay into d dbar',
            'MDME(175,1)=1     !Z decay into u ubar',
            'MDME(176,1)=1     !Z decay into s sbar',
            'MDME(177,1)=1     !Z decay into c cbar',
            'MDME(178,1)=1     !Z decay into b bbar',
            'MDME(179,1)=1     !Z decay into t tbar',
            'MDME(180,1)=-1    !Z decay into b4 b4bar',
            'MDME(181,1)=-1    !Z decay into t4 t4bar',
            'MDME(182,1)=1     !Z decay into e- e+',
            'MDME(183,1)=1     !Z decay into nu_e nu_ebar',
            'MDME(184,1)=1     !Z decay into mu- mu+',
            'MDME(185,1)=1     !Z decay into nu_mu nu_mubar',
            'MDME(186,1)=1     !Z decay into tau- tau+',
            'MDME(187,1)=1     !Z decay into nu_tau nu_taubar',
            'MDME(188,1)=-1    !Z decay into tau4 tau4bar',
            'MDME(189,1)=-1    !Z decay into nu_tau4 nu_tau4bar',
	    ), 
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
            'processParameters')
    ),
    jetMatching = cms.untracked.PSet(
       scheme = cms.string("Madgraph"),
       mode = cms.string("auto"),       # soup, or "inclusive" / "exclusive"
       MEMAIN_etaclmax = cms.double(5.0),
       MEMAIN_qcut = cms.double(-1),
       MEMAIN_nqmatch = cms.int32(-1),
       MEMAIN_minjets = cms.int32(-1),
       MEMAIN_maxjets = cms.int32(-1),
       MEMAIN_showerkt = cms.double(0),
       MEMAIN_excres = cms.string(''),
       outTree_flag = cms.int32(0)
    )
)

ProductionFilterSequence = cms.Sequence(generator)


