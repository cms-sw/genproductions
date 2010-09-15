import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

generator = cms.EDFilter("SherpaGeneratorFilter",
  maxEventsToPrint = cms.untracked.int32(0),
  filterEfficiency = cms.untracked.double(1.0),
  crossSection = cms.untracked.double(-1),
  libDir    = cms.untracked.string('SherpaRun'),
  resultDir = cms.untracked.string('Result'),
  SherpaParameters = cms.PSet(parameterSets = cms.vstring(
                             "Analysis",
                             "Run"),
                              Analysis = cms.vstring(
				" BEGIN_ANALYSIS {",
				" LEVEL Hadron;",
				" PATH_PIECE Norm/;",
				" Statistics FinalState;",
				"} END_ANALYSIS;"
                                                  ),
                              Run = cms.vstring(
				"(run){",
				" EVENTS = 10000;",
				" ANALYSIS = 0;",
				" EVENT_MODE = HepMC",
				"}(run)",
				"(beam){",
				" BEAM_1          = 2212",
				" BEAM_ENERGY_1   = 3500.",
				" BEAM_2          = 2212",
				" BEAM_ENERGY_2   = 3500.",
				"}(beam)",
				"(processes){",
				" Process 93 93 -> 22 39;",
				" Print_Graphs : Process;",
				" End process;",
				"}(processes)",
				"(selector){",
				" JetFinder   sqr(20/E_CMS) 1.",
				" PT 22 50 14000",
				"}(selector)",
				"(model){",
				" MODEL = ADD",
				" N_ED  = 2",
				" M_S   = 1000",
				" M_CUT = 1000",
				" KK_CONVENTION = 5",
				" G_NEWTON      = 6.707e-39",
				"}(model)",
				"(me){",
				" ME_SIGNAL_GENERATOR = Amegic",
				"}(me)",
				"(mi){",
				" MI_HANDLER   = Amisic",
				"}(mi)",
				"(isr){",
				" PDF_LIBRARY     = LHAPDFSherpa",
				" PDF_SET         = cteq6ll.LHpdf",
				" PDF_SET_VERSION = 1",
				" PDF_GRID_PATH   = PDFsets",
				"}(isr)",
				"(analysis){",
				" BEGIN_ANALYSIS {",
				" LEVEL  MEs Showers",
				" PT    39  0. 2000. 100 LinErr FinalState",
				"} END_ANALYSIS",
				"}(analysis)"
                                                  ),
                             )
)

ProductionFilterSequence = cms.Sequence(generator)

