import FWCore.ParameterSet.Config as cms

source = cms.Source("MCatNLOSource",
       # the energy in the centre-of-mass has to be set
       # in HERWIG - comEnergy and MC@NLO - ECM;
       # the default value in CMSSW is 14000., if a different value
       # is chosen only in MC@NLO and not adapted also in HERWIG
       # the following error message shows up:
       # "terminate called after throwing an instance of 'cms::Exception'
       #  what():  ---- Herwig6Error BEGIN
       #  Herwig stopped run with error code 507."

       comEnergy            = cms.untracked.double(10000.),               
       herwigVerbosity      = cms.untracked.int32(0),
       mcatnloVerbosity     = cms.untracked.int32(0),
       herwigHepMCVerbosity = cms.untracked.bool(False),
       maxEventsToPrint     = cms.untracked.int32(0),
       #Higgs production = -1600; Higgs decay to 2 tau = -9             
       #Higgs production = -1600; Higgs decay to 2 gamma = -12  
       processNumber        = cms.untracked.int32(-1612),
       doHardEvents         = cms.untracked.bool(True),
       numHardEvents        = cms.untracked.int32(10000),
       doMPInteraction      = cms.untracked.bool(True),
       #Must update for H--->gamma gamma
       crossSection         = cms.untracked.double(0.0334),

       filterEfficiency     = cms.untracked.double(1.),

       useJimmy             = cms.untracked.bool(True),
                    
       stringFileName       = cms.untracked.string('stringInput.txt'),
       printCards           = cms.untracked.bool(False),

       HerwigParameters = cms.PSet(
          defaultHerwig = cms.vstring(),
          parameterSets = cms.vstring('defaultHerwig')
       ),

       MCatNLOParameters    = cms.PSet(
            parameterSets   = cms.vstring('mcatnloReadin'),
            mcatnloReadin   = cms.vstring(
                     'ECM=10000',    # CM energy 
                     'FREN=1',       # renormalization scale factor
                     'FFACT=1',      # factorization scale factor
                     'HVQMASS=175',  # mass of the heavy quark( top for Higgs)
                     'TWIDTH=1.4',   # top quark width
                     'WMASS=80.41',  # W mass
                     'WWIDTH=2.124', # W width
                     'ZMASS=91.17',  # Z mass
                     'ZWIDTH=2.495', # Z width
                     'HGGMASS=130',  # Higgs mass
       # Must update width as function of mass               
                     'HGGWIDTH=0.00497',# Higgs width
                     'IBORNHGG=1',   # IBORNHGG=1 --> exact M_top dependence,
                                     # IBORNHGG=2 --> M_top -> infinity
                     # If the Higgs, Z or W mass is distributed according to
                     # Breit-Wigner (this is always the case when the vector
                     # bosons decay), the mass range is
                     # M0_Vi - ViGAMMAX * WIDTH <M_Vi< M0_Vi + ViGAMMAX * WIDTH
                     # for the vector bosons, and
                     # M0_H - HGAMMAX * WIDTH < M_H < M0_H + HGAMMAX * WIDTH
                     # for the Higgs, M0 being the pole mass.
                     # If VGAMMAX<0 (this option may not be implemented for
                     # all processes), then
                     # ViMASSINF < M_Vi < ViMASSSUP
                     # and
                     # HMASSINF < M_H < VMASSSUP
                     # if HGAMMAX<0. In the case of gamma* production, we have
                     # V1MASSINF < Q < V1MASSSUP
                     # where Q is the virtuality of the virtual photon.
                     # For all processes except vector boson pair production,
                     # only one vector boson is present in the final state,
                     # and is labelled as V1. In the case of vector boson pair
                     # production, the labels (V,V2) correspond to (W+,W-), 
                     # (Z,Z), (W+,Z), and (W-,Z) for
                     # IPROC=-2850, -2860, -2870, and 2880  respectively
                     'V1GAMMAX=30',
                     'V1MASSINF=0',
                     'V1MASSSUP=0',
                     'V2GAMMAX=30',
                     'V2MASSINF=0',
                     'V2MASSSUP=0',
                     'HGAMMAX=30',
                     'HMASSINF=0',
                     'HMASSSUP=0',
                     'UMASS=0.32', # quarks and gluon masses
                     'DMASS=0.32',
                     'SMASS=0.5',
                     'CMASS=1.55',
                     'BMASS=4.8',
                     'GMASS=0.75',
                     # absolute values of the CKM matrix elements; used only
                     # for single-top production. Set VUD=VUS=VUB=0 to use the
                     # defaults in the code
                     'VUD=0.9748',
                     'VUS=0.2225',
                     'VUB=0.0036',
                     'VCD=0.2225',
                     'VCS=0.9740',
                     'VCB=0.041 ',
                     'VTD=0.009 ',
                     'VTS=0.0405',
                     'VTB=0.9992',
                     # Set AEMRUN=YES to use running alpha_em,
                     # AEMRUN=NO to use the Thomson value
                     'AEMRUN=YES',
                     # process number; MC@NLO process codes are negative.
                     # A positive process vector boson code: IVCODE=-1,0,1 for
                     # W^-, Z, and W^+ respectively.
                     # This variables is only used in WH and ZH production
                     'IVCODE=1',
                     # lepton identification: IL1CODE=1,..,6 for Z,
                     # IL1CODE=1,..,3 for W, in accordance to HERWIG
                     # convention. Set IL1CODE=7 for undecayed vector bosons.
                     # IL1CODE is relevant to WH, ZH, and vector bosons pair
                     # production. In the latter case, the variable IL2CODE is
                     # also introduced, and (IL1CODE,IL2CODE)
                     # control the decays of (W+,W-), (Z,Z), (W+,Z), and (W-,Z)
                     # for IPROC=-2850, -2860, -2870, and 2880 respectively
                     'IL1CODE=1',
                     'IL2CODE=1',
                     # PDF group name; unused when linked to LHAPDF
                     'PDFGROUP=LHAPDF',
                     # PDF set number; use LHAGLUE conventions when linked to
                     # LHAPDF
                     'PDFSET=10050',
                     # Lambda_5, used in NLO computations. A negative entry
                     # returns the value resulting from PDF fit. 
                     # WARNING: negative entries may lead to inconsistent
                     # results when using PDFLIB or LHAPDF: use a positive
                     # entry when in doubt
                     #'LAMBDAFIVE=1',
                     'LAMBDAFIVE=0.2262',
                     # Scheme
                     'SCHEMEOFPDF=MS',
                     # Lambda_5, used by HERWIG. A negative entry returns the
                     # HERWIG default value
                     'LAMBDAHERW=-1',
                     'FPREFIX=Hgg130',
                     'EVPREFIX=Hgg130',
                     'WGTTYPE=1', 
                     'PDFLIBRARY=HWLHAPDF', 
                     'HERPDF=EXTPDF', 
                     'LHAPATH="/afs/cern.ch/sw/lcg/external/MCGenerators/lhapdf/5.4.0/share/PDFsets"', 
                     'LHAOFL=FREEZE')
       )
)

#to avoid problems with unknown particles IDs the next line
#process.genParticles.abortOnUnknownPDGCode = False
#should be added in final python card, e.g.
#MCatNLO_HiggsSM_ggH_2tau_mH130_10TeV_cff_py_GEN_IDEAL_V5.py

filterMCatNLO = cms.EDFilter("MCatNLOFilter")
ProductionFilterSequence = cms.Sequence(filterMCatNLO)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/MCatNLO_HiggsSM_H_2gamma_mH130_10TeV_cff.py,v $'),
    annotation = cms.untracked.string(
    'MC@NLO HiggsSM H 2gamma mH130 at 10TeV')
)
 
