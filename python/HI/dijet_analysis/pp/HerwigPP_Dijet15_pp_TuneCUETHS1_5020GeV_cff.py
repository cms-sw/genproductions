from Configuration.Generator.HerwigppDefaults_cfi import *
from Configuration.Generator.HerwigppUE_CUETHS1_cfi import *
from Configuration.Generator.HerwigppPDF_CTEQ6_LO_cfi import *
from Configuration.Generator.HerwigppEnergy_13TeV_cfi import *

generator = cms.EDFilter("ThePEGGeneratorFilter",
	herwigDefaultsBlock,
	herwigppUESettingsBlock,
	herwigppPDFSettingsBlock,
	herwigppEnergySettingsBlock,

    crossSection = cms.untracked.double(1),
    filterEfficiency = cms.untracked.double(1),
    configFiles = cms.vstring(),
    
    parameterSets = cms.vstring(
      'hwpp_cmsDefaults',
      'hwpp_ue_CUETHS1',
      'hwpp_pdf_CTEQ6L1_CUETHS1',
      'hwpp_cm_13TeV',
      'productionParameters',
    ),
    productionParameters = cms.vstring(                                                                    
      'cd /Herwig/MatrixElements/',
      'insert SimpleQCD:MatrixElements[0] MEQCD2to2',
      'cd /',
      'set /Herwig/Cuts/JetKtCut:MinKT 15*GeV',
      'set /Herwig/Cuts/JetKtCut:MaxKT 30*GeV',
      'set /Herwig/Cuts/QCDCuts:MHatMin 0.0*GeV',
      'set /Herwig/UnderlyingEvent/MPIHandler:IdenticalToUE 0',
    ),
)