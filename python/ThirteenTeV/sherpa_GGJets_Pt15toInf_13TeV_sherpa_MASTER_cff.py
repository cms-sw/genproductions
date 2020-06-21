import FWCore.ParameterSet.Config as cms
import os

source = cms.Source("EmptySource")

generator = cms.EDFilter("SherpaGeneratorFilter",
  maxEventsToPrint = cms.int32(0),
  filterEfficiency = cms.untracked.double(1.0),
  crossSection = cms.untracked.double(-1),
  SherpaProcess = cms.string('GGJets_Pt15toInf_13TeV_sherpa'),
  SherpackLocation = cms.string('./'),
  SherpackChecksum = cms.string('d5174cc5977f500727a2d3d3994b4ef5'),
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
				" semihard xsec = 39.773 mb,",
				" non-diffractive xsec = 17.0318 mb with nd factor = 0.3142."
                                                  ),
                              Run = cms.vstring(
				" (run){",
				" EVENTS 100;",
				" EVENT_MODE HepMC;",
				" ME_SIGNAL_GENERATOR Comix Internal;",
				" EVENT_GENERATION_MODE Unweighted;",
				" LOOPGEN:=BlackHat;",
				" BEAM_1 2212; BEAM_ENERGY_1 6500.;",
				" BEAM_2 2212; BEAM_ENERGY_2 6500.;",
				" CSS_EW_MODE=1; ME_QED=0;",
				"}(run)",
				" (processes){",
				" Process 21 21 -> 22 22;",
				" Scales VAR{Abs2(p[2]+p[3])};",
				" ME_Generator Internal;",
				" End process;",
				" Process 93 93 -> 22 22 93{3};",
				" Order (*,2);",
				" Enhance_Factor 2 {3};",
				" Enhance_Factor 5 {4};",
				" Enhance_Factor 10 {5};",
				" CKKW sqr(20./E_CMS);",
				" Integration_Error 0.005 {3};",
				" Integration_Error 0.03 {4};",
				" Integration_Error 0.05 {5};",
				" End process;",
				"}(processes)",
				" (selector){",
				" PT 22 15.0 E_CMS;",
				" PseudoRapidity 22 -2.5 2.5;",
				"}(selector)",
				" (integration){",
				" FINISH_OPTIMIZATION Off;",
				"}(integration)",
				" (isr){",
				" PDF_LIBRARY LHAPDFSherpa;",
				" PDF_SET NNPDF30_nnlo_as_0118;",
				" PDF_SET_VERSION 0;",
				" PDF_GRID_PATH PDFsets;",
				"}(isr)"
                                                  ),
                             )
)

ProductionFilterSequence = cms.Sequence(generator)

