import FWCore.ParameterSet.Config as cms
import os

source = cms.Source("EmptySource")

generator = cms.EDFilter("SherpaGeneratorFilter",
  maxEventsToPrint = cms.int32(0),
  filterEfficiency = cms.untracked.double(1.0),
  crossSection = cms.untracked.double(-1),
  SherpaProcess = cms.string('13TeV_gam_2j_Pt-5000-8000_71X'),
  SherpackLocation = cms.string('./'),
  SherpackChecksum = cms.string('9871add6f020b6d07c74be922b596801'),
  FetchSherpack = cms.bool(False),
  SherpaPath = cms.string('./'),
  SherpaPathPiece = cms.string('./'),
  SherpaResultDir = cms.string('Result'),
  SherpaDefaultWeight = cms.double(1.0),
  SherpaParameters = cms.PSet(parameterSets = cms.vstring(
                             "MPI_Cross_Sections",
                             "Run"),
                              MPI_Cross_Sections = cms.vstring(
				" MPIs in Sherpa, Model = Amisic:",
				" semihard xsec = 39.5682 mb,",
				" non-diffractive xsec = 17.0318 mb with nd factor = 0.3142."
                                                  ),
                              Run = cms.vstring(
				" (run){",
				" EVENTS = 100;",
				" HEPMC_USE_NAMED_WEIGHTS=1;",
				" SCALE_VARIATIONS 0.25,0.25 0.25,1. 0.25,4 1,0.25 1.,1. 1.,4. 4.,0.25 4.,1. 4.,4.;",
				" PDF_VARIATIONS CT14nnlo[all] MMHT2014nnlo68cl[all] NNPDF30_nnlo_as_0118[all];",
				" ME_QED=Off",
				"}(run)",
				" (beam){",
				" BEAM_1 = 2212; BEAM_ENERGY_1 = 6500.;",
				" BEAM_2 = 2212; BEAM_ENERGY_2 = 6500.;",
				"}(beam)",
				" (processes){",
				" Process 93 93 -> 22 93 93{1};",
				" Order (*,1);",
				" Enhance_Factor 2 {3};",
				" CKKW sqr(20./E_CMS);",
				" Integration_Error 0.005 {3};",
				" End process;",
				"}(processes)",
				" (selector){",
				" PT 22 5000. 8000.;",
				"}(selector)",
				" (shower){",
				" CSS_EW_MODE = 1;",
				"}(shower)",
				" (integration){",
				" FINISH_OPTIMIZATION = Off;",
				"}(integration)",
				" (isr){",
				" PDF_LIBRARY     = LHAPDFSherpa;",
				" PDF_SET         = NNPDF30_nnlo_as_0118;",
				" PDF_SET_VERSION = 0;",
				" PDF_GRID_PATH   = PDFsets;",
				"}(isr)",
				" (me){",
				" ME_SIGNAL_GENERATOR = Internal Comix;",
				" EVENT_GENERATION_MODE = Weighted;",
				"}(me)",
				" (mi){",
				" MI_HANDLER = Amisic  # None or Amisic;",
				"}(mi)"
                                                  ),
                             )
)

ProductionFilterSequence = cms.Sequence(generator)

