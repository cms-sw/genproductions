import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

generator = cms.EDFilter("SherpaGeneratorFilter",
  maxEventsToPrint = cms.untracked.int32(0),
  filterEfficiency = cms.untracked.double(1.0),
  crossSection = cms.untracked.double(-1),
  Path = cms.untracked.string('SherpaRun'),
  PathPiece = cms.untracked.string('SherpaRun'),
  ResultDir = cms.untracked.string('Result'),
  default_weight = cms.untracked.double(1.0),
  SherpaParameters = cms.PSet(parameterSets = cms.vstring(
                             "Run"),
                              Run = cms.vstring(
				"(run){",
				" EVENTS = 1000;",
				" EVENT_MODE = HepMC;",
				" # avoid comix re-init after runcard modification",
				" WRITE_MAPPING_FILE 3;",
				"}(run)",
				"(beam){",
				" BEAM_1 = 2212; BEAM_ENERGY_1 = 3500.;",
				" BEAM_2 = 2212; BEAM_ENERGY_2 = 3500.;",
				"}(beam)",
				"(processes){",
				" Process 93 93 -> 90 90 93{4};",
				" Order_EW 2;",
                                " Enhance_Factor 2 {3};",
                                " Enhance_Factor 35 {4};",
                                " Enhance_Factor 40 {5};",
                                " Enhance_Factor 50 {6};",
				" CKKW sqr(20./E_CMS);",
				" Integration_Error 0.02 {5,6};",
				" End process;",
				"}(processes)",
				"(selector){",
				" Mass  90 90 50. E_CMS;",
				"}(selector)",
				"(me){",
				" ME_SIGNAL_GENERATOR = Internal Comix",
				" EVENT_GENERATION_MODE = Unweighted;",
				"}(me)",
				"(mi){",
				" MI_HANDLER = Amisic  # None or Amisic",
				"}(mi)"
                                                  ),
                             )
)

ProductionFilterSequence = cms.Sequence(generator)

