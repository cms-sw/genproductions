import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

generator = cms.EDFilter("SherpaGeneratorFilter",
  maxEventsToPrint = cms.untracked.int32(0),
  filterEfficiency = cms.untracked.double(1.0),
  crossSection = cms.untracked.double(-1),
  libDir    = cms.untracked.string('SherpaRun'),
  resultDir = cms.untracked.string('Result'),
  SherpaParameters = cms.PSet(parameterSets = cms.vstring(
                             "Run"),
                              Run = cms.vstring(
				"(run){",
				" EVENTS = 10000",
				" EVENT_MODE = HepMC",
				" # avoid comix re-init after runcard modification",
				" WRITE_MAPPING_FILE 3;",
				"}(run)",
				"(beam){",
				" BEAM_1 = 2212; BEAM_ENERGY_1 = 4000;",
				" BEAM_2 = 2212; BEAM_ENERGY_2 = 4000;",
				"}(beam)",
				"(processes){",
				" Process 93 93 -> 90 90 93{5}",
				" Order_EW 2;",
				" CKKW sqr(30/E_CMS)",
				" Integration_Error 0.02 {6};",
				" Integration_Error 0.05 {7};",
				" End process;",
				"}(processes)",
				"(selector){",
				" Mass 90 90 50 E_CMS",
				"}(selector)",
				"(isr){",
				" PDF_LIBRARY     = LHAPDFSherpa",
				" PDF_SET         = cteq6ll.LHpdf",
				" PDF_SET_VERSION = 1",
				" PDF_GRID_PATH   = PDFsets",
				"}(isr)",
				"(me){",
				" ME_SIGNAL_GENERATOR = Internal Comix",
				"}(me)",
				"(mi){",
				" MI_HANDLER = Amisic  # None or Amisic",
				"}(mi)"
                                                  ),
                             )
)

ProductionFilterSequence = cms.Sequence(generator)

