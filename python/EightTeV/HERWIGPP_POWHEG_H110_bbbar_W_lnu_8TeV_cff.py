import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *

generator = cms.EDFilter(
    "ThePEGGeneratorFilter",
    herwigDefaultsBlock,
    configFiles = cms.vstring(),

    parameterSets = cms.vstring(
    'cm8TeV',
    'powhegDefaults',
    'HbbWlnuParameters',
    'basicSetup',
    'setParticlesStableForDetector',
    ),

    powhegDefaults = cms.vstring(
    '# Need to use an NLO PDF',
    'cp /Herwig/Partons/MRST-NLO /cmsPDFSet',
    '# and strong coupling',
    'create Herwig::O2AlphaS O2AlphaS',
    'set /Herwig/Generators/LHCGenerator:StandardModelParameters:QCD/RunningAlphaS O2AlphaS',
    '# Setup the POWHEG shower',
    'cd /Herwig/Shower',
    '# use the general recon for now',
    'set KinematicsReconstructor:ReconstructionOption General',
    '# create the Powheg evolver and use it instead of the default one',
    'create Herwig::PowhegEvolver PowhegEvolver HwPowhegShower.so',
    'set ShowerHandler:Evolver PowhegEvolver',
    'set PowhegEvolver:ShowerModel ShowerModel',
    'set PowhegEvolver:SplittingGenerator SplittingGenerator',
    'set PowhegEvolver:MECorrMode 0',
    '# create and use the Drell-yan hard emission generator',
    'create Herwig::DrellYanHardGenerator DrellYanHardGenerator',
    'set DrellYanHardGenerator:ShowerAlpha AlphaQCD',
    'insert PowhegEvolver:HardGenerator 0 DrellYanHardGenerator',
    '# create and use the gg->H hard emission generator',
    'create Herwig::GGtoHHardGenerator GGtoHHardGenerator',
    'set GGtoHHardGenerator:ShowerAlpha AlphaQCD',
    'insert PowhegEvolver:HardGenerator 0 GGtoHHardGenerator',
    ),
    
    pdfCTEQ6M = cms.vstring(
    'mkdir /LHAPDF',
    'cd /LHAPDF',
    'create ThePEG::LHAPDF CTEQ6M',
    'set CTEQ6M:PDFName cteq6mE.LHgrid',
    'set CTEQ6M:RemnantHandler /Herwig/Partons/HadronRemnants',
    'cp CTEQ6M /cmsPDFSet',
    'cd /'
    ),
    
    HbbWlnuParameters = cms.vstring(
    'cd /Herwig/MatrixElements/',
    'insert SimpleQCD:MatrixElements[0] PowhegMEPP2WH',
    'set /Herwig/Cuts/JetKtCut:MinKT 0.0*GeV',
    
    'set /Herwig/Particles/h0:NominalMass 110.*GeV',
    'set /Herwig/Particles/h0/h0->b,bbar;:OnOff On',
    'set /Herwig/Particles/h0/h0->b,bbar;:BranchingRatio 0.7195',
    'set /Herwig/Particles/h0/h0->W+,W-;:OnOff Off',
    'set /Herwig/Particles/h0/h0->tau-,tau+;:OnOff Off',
    'set /Herwig/Particles/h0/h0->g,g;:OnOff Off',
    'set /Herwig/Particles/h0/h0->c,cbar;:OnOff Off',
    'set /Herwig/Particles/h0/h0->Z0,Z0;:OnOff Off',
    'set /Herwig/Particles/h0/h0->gamma,gamma;:OnOff Off',
    'set /Herwig/Particles/h0/h0->mu-,mu+;:OnOff Off',
    'set /Herwig/Particles/h0/h0->t,tbar;:OnOff Off',

    'set /Herwig/Particles/W+/W+->u,dbar;:OnOff Off',
    'set /Herwig/Particles/W+/W+->c,sbar;:OnOff Off',
    'set /Herwig/Particles/W+/W+->nu_mu,mu+;:OnOff On',
    'set /Herwig/Particles/W+/W+->nu_e,e+;:OnOff On',
    'set /Herwig/Particles/W+/W+->nu_tau,tau+;:OnOff On',
    'set /Herwig/Particles/W+/W+->sbar,u;:OnOff Off',
    'set /Herwig/Particles/W+/W+->c,dbar;:OnOff Off',
    'set /Herwig/Particles/W+/W+->bbar,c;:OnOff Off',
    
    'set /Herwig/Particles/W-/W-->ubar,d;:OnOff Off',
    'set /Herwig/Particles/W-/W-->cbar,s;:OnOff Off',
    'set /Herwig/Particles/W-/W-->nu_mubar,mu-;:OnOff On',
    'set /Herwig/Particles/W-/W-->nu_ebar,e-;:OnOff On',
    'set /Herwig/Particles/W-/W-->nu_taubar,tau-;:OnOff On',
    'set /Herwig/Particles/W-/W-->s,ubar;:OnOff Off',
    'set /Herwig/Particles/W-/W-->cbar,d;:OnOff Off',
    'set /Herwig/Particles/W-/W-->b,cbar;:OnOff Off',
    ),
    
    crossSection     = cms.untracked.double(0.1665),
    filterEfficiency = cms.untracked.double(1.0)
    )



configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('\$Revision: 1.3 $'),
    name = cms.untracked.string('\$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/HERWIGPP_POWHEG_H110_bbbar_W_lnu_8TeV_cff.py,v $'),
	annotation = cms.untracked.string('HERWIGPP/POWHEG: (H->bb)(W->lnu), m(H)=110 GeV, l=e or mu or tau')
)
