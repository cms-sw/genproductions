import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *

generator = cms.EDFilter(
    "ThePEGGeneratorFilter",
    herwigDefaultsBlock,
    configFiles = cms.vstring(),

    parameterSets = cms.vstring(
    'cm7TeV',
    'powhegDefaults',
    'HmmZParameters',
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
    
    HmmZParameters = cms.vstring(
    'cd /Herwig/MatrixElements/',
    'insert SimpleQCD:MatrixElements[0] PowhegMEPP2ZH',
    'set /Herwig/Cuts/JetKtCut:MinKT 0.0*GeV',
    
    'set /Herwig/Particles/h0:NominalMass 155.*GeV',
    'set /Herwig/Particles/h0/h0->b,bbar;:OnOff Off',
    'set /Herwig/Particles/h0/h0->W+,W-;:OnOff Off',
    'set /Herwig/Particles/h0/h0->tau-,tau+;:OnOff Off',
    'set /Herwig/Particles/h0/h0->g,g;:OnOff Off',
    'set /Herwig/Particles/h0/h0->c,cbar;:OnOff Off',
    'set /Herwig/Particles/h0/h0->Z0,Z0;:OnOff Off',
    'set /Herwig/Particles/h0/h0->gamma,gamma;:OnOff Off',
    'set /Herwig/Particles/h0/h0->mu-,mu+;:OnOff On',
    'set /Herwig/Particles/h0/h0->mu-,mu+;:BranchingRatio 3.65e-05',
    'set /Herwig/Particles/h0/h0->t,tbar;:OnOff Off',

    'set /Herwig/Particles/Z0/Z0->d,dbar;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->s,sbar;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->b,bbar;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->u,ubar;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->c,cbar;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->nu_e,nu_ebar;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->nu_mu,nu_mubar;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->nu_tau,nu_taubar;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->e-,e+;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->mu-,mu+;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->tau-,tau+;:OnOff On',
    ),
    
    crossSection     = cms.untracked.double(0.1525),
    filterEfficiency = cms.untracked.double(1.0)
    )

