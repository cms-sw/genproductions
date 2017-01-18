import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *

generator = cms.EDFilter(
    "ThePEGGeneratorFilter",
    herwigDefaultsBlock,
    configFiles = cms.vstring(),

    parameterSets = cms.vstring(
    'cm8TeV',
    'powhegNewDefaults',
    'HbbZllParameters',
    'basicSetup',
    'setParticlesStableForDetector',
    ),

    powhegNewDefaults = cms.vstring(
    '#  Need to use an NLO PDF',
    '#  and strong coupling',
    'cp /Herwig/Partons/MRST-NLO /Herwig/Partons/cmsPDFSet',
    'create Herwig::O2AlphaS O2AlphaS',
    'set /Herwig/Generators/LHCGenerator:StandardModelParameters:QCD/RunningAlphaS O2AlphaS',
    '#  Setup the POWHEG shower',
    'cd /Herwig/Shower',
    'set Evolver:HardEmissionMode POWHEG',

    '# higgs + W (N.B. if considering all W decay modes useful to set )',
    '#           (jet pT cut to zero so no cut on W decay products    )',
    '# insert SimpleQCD:MatrixElements[0] PowhegMEPP2WH',
    '# set /Herwig/Cuts/JetKtCut:MinKT 0.0*GeV',
    '# higgs + Z (N.B. if considering all Z decay modes useful to set )',
    '#           (jet pT cut to zero so no cut on Z decay products    )',
    '# insert SimpleQCD:MatrixElements[0] PowhegMEPP2ZH',
    '# set /Herwig/Cuts/JetKtCut:MinKT 0.0*GeV',
    '# gg/qqbar -> Higgs',
    '# insert SimpleQCD:MatrixElements[0] PowhegMEHiggs',
    '# Weak boson pair production: WW / ZZ / WZ / W+Z [WpZ] / W-Z [WmZ]',
    '# insert SimpleQCD:MatrixElements[0] PowhegMEPP2VV',
    '# set PowhegMEPP2VV:Process WpZ'
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
    
    HbbZllParameters = cms.vstring(
    'cd /Herwig/MatrixElements/',
    'insert SimpleQCD:MatrixElements[0] PowhegMEPP2ZH',
    'set /Herwig/Cuts/JetKtCut:MinKT 0.0*GeV',
    
    'set /Herwig/Particles/h0:NominalMass 115.*GeV',
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

    'set /Herwig/Particles/Z0/Z0->d,dbar;:OnOff Off',
    'set /Herwig/Particles/Z0/Z0->s,sbar;:OnOff Off',
    'set /Herwig/Particles/Z0/Z0->b,bbar;:OnOff On',
    'set /Herwig/Particles/Z0/Z0->u,ubar;:OnOff Off',
    'set /Herwig/Particles/Z0/Z0->c,cbar;:OnOff Off',
    'set /Herwig/Particles/Z0/Z0->nu_e,nu_ebar;:OnOff Off',
    'set /Herwig/Particles/Z0/Z0->nu_mu,nu_mubar;:OnOff Off',
    'set /Herwig/Particles/Z0/Z0->nu_tau,nu_taubar;:OnOff Off',
    'set /Herwig/Particles/Z0/Z0->e-,e+;:OnOff Off',
    'set /Herwig/Particles/Z0/Z0->mu-,mu+;:OnOff Off',
    'set /Herwig/Particles/Z0/Z0->tau-,tau+;:OnOff Off',
    ),
    
    crossSection     = cms.untracked.double(0.0288),
    filterEfficiency = cms.untracked.double(1.0)
    )

