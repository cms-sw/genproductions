import FWCore.ParameterSet.Config as cms

from Configuration.Generator.HerwigppDefaults_cfi import *
from Configuration.Generator.HerwigppUE_EE_5C_cfi import *
from Configuration.Generator.HerwigppPDF_CTEQ6_LO_cfi import *									# Import CTEQ6L PDF as shower pdf
from Configuration.Generator.HerwigppEnergy_13TeV_cfi import *
from Configuration.Generator.HerwigppLHEFile_cfi import *
from Configuration.Generator.HerwigppMECorrections_cfi import *

# Showering LO MadGraph5_aMC@NLO LHE files with a different PDF for the hard subprocess 
############ WARNING ######
# This option should only be used with LO MadGraph5_aMC@NLO LHE files.
# In case of NLO, MC@NLO matched LHE files this results most likely in a mismatch of phase space
############ WARNING ######
herwigNewPhysicsBlock = cms.PSet(
    herwigNewPhysics = cms.vstring(
        'cd /Herwig/Particles',
        'create ThePEG::ParticleData graviton',
        'setup graviton 39 graviton 1000 0.0 0.0 0.0 0 0 5 0',
        'cd /'
        ),
    )

herwigpphardPDFSettingsBlock = cms.PSet(
    # PDF for shower
    hwpp_pdf_NNPDF30LO = cms.vstring(
        'create ThePEG::LHAPDF /Herwig/Partons/cmsPDFSet ThePEGLHAPDF.so',          # cmsPDFSet Default name for shower PDF
        'set /Herwig/Partons/cmsPDFSet:PDFName NNPDF30_lo_as_0130.LHgrid',
        'set /Herwig/Partons/cmsPDFSet:RemnantHandler /Herwig/Partons/HadronRemnants',
        'set /Herwig/Particles/p+:PDF /Herwig/Partons/cmsPDFSet',               # Use PDF in shower
        'set /Herwig/Particles/pbar-:PDF /Herwig/Partons/cmsPDFSet'
    ),
    # PDF for hard subprocess
    hwpp_pdf_NNPDF30LO_Hard = cms.vstring(
        'create ThePEG::LHAPDF /Herwig/Partons/cmsHardPDFSet ThePEGLHAPDF.so',          # cmsHardPDFSet Default name for hard subprocess PDF
        'set /Herwig/Partons/cmsHardPDFSet:PDFName NNPDF30_lo_as_0130.LHgrid',
        'set /Herwig/Partons/cmsHardPDFSet:RemnantHandler /Herwig/Partons/HadronRemnants'
    )
)


generator = cms.EDFilter("ThePEGHadronizerFilter",
	herwigDefaultsBlock,
        herwigNewPhysicsBlock,
	herwigppUESettingsBlock,
	herwigppPDFSettingsBlock,
        herwigpphardPDFSettingsBlock,
	herwigppEnergySettingsBlock,
	herwigppLHEFileSettingsBlock,
	herwigppMECorrectionsSettingsBlock,

	configFiles = cms.vstring(),
	parameterSets = cms.vstring(
		'hwpp_cmsDefaults',
                'herwigNewPhysics',
		'hwpp_ue_EE5C',
		'hwpp_cm_13TeV',
		'hwpp_pdf_CTEQ6L1',			# Shower PDF matching with the tune
		'hwpp_pdf_NNPDF30LO_Hard',		# PDF of hard subprocess
		'hwpp_LHE_MadGraph_DifferentPDFs',	### WARNING ### Use this option only with LO MadGraph5_aMC@NLO LHE files
		'hwpp_MECorr_Off',			# Switch off ME corrections while showering LHE files as recommended by Herwig++ authors
	),

        crossSection = cms.untracked.double(-1),
        filterEfficiency = cms.untracked.double(1.0),
)
ProductionFilterSequence = cms.Sequence(generator)
