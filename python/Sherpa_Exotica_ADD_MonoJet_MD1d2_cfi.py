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
				" EVENTS = 200;",
				" ANALYSIS = 0;",
				" EVENT_MODE = HepMC",
				"}(run)",
				"(beam){",
				" BEAM_1          = 2212",
				" BEAM_ENERGY_1   = 5000.",
				" BEAM_2          = 2212",
				" BEAM_ENERGY_2   = 5000.",
				"}(beam)",
				"(processes){",
				" Process 93 93 -> 93 39;",
				" Print_Graphs : Process;",
				" End process;",
				"}(processes)",
				"(selector){",
				" PT 93 150 14000",
				"}(selector)",
				"(model){",
				" MODEL = ADD",
				" N_ED  = 2",
				" M_S   = 2000",
				" M_CUT = 2000",
				" KK_CONVENTION = 5",
				" MASS[39] = 100.",
				" MASS[40] = 100.",
				"}(model)",
				"(me){",
				" ME_SIGNAL_GENERATOR = Amegic",
				"}(me)",
				"(fragmentation){",
				" FRAGMENTATION=Off;",
				"}(fragmentation)",
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

