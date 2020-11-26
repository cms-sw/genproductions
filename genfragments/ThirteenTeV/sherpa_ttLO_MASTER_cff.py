import FWCore.ParameterSet.Config as cms
import os

source = cms.Source("EmptySource")

generator = cms.EDFilter("SherpaGeneratorFilter",
  maxEventsToPrint = cms.int32(0),
  filterEfficiency = cms.untracked.double(1.0),
  crossSection = cms.untracked.double(-1),
  SherpaProcess = cms.string('ttLO'),
  SherpackLocation = cms.string('slc6_amd64_gcc481/13TeV/sherpa/2.1.1/ttLO/v1 '),
  SherpackChecksum = cms.string('2655d705328e3b6df9a973994eb6a6c0'),
  FetchSherpack = cms.bool(True),
  SherpaPath = cms.string('./'),
  SherpaPathPiece = cms.string('./'),
  SherpaResultDir = cms.string('Result'),
  SherpaDefaultWeight = cms.double(1.0),
  SherpaParameters = cms.PSet(parameterSets = cms.vstring(
                             "Run"),
                              Run = cms.vstring(
				"(run){",
				" EVENTS 10K; ERROR 0.99;",
				" MI_HANDLER None",
				" EVENT_OUTPUT=HepMC_GenEvent[HepMC.tt.LO]",
				" FSF:=1.; RSF:=1.; QSF:=1.;",
				" SCALES METS{FSF*MU_F2}{RSF*MU_R2}{QSF*MU_Q2};",
				" CORE_SCALE QCD;",
				" METS_BBAR_MODE 5;",
				" NJET:=1; LJET:=0; QCUT:=30.;",
				" RESULT_DIRECTORY = Results.tt.LO",
				" ME_SIGNAL_GENERATOR Comix Amegic LOOPGEN;",
				" OL_PREFIX=/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/openloops/1.1.1",
				" EVENT_GENERATION_MODE Weighted;",
				" LOOPGEN:=OpenLoops;",
				" BEAM_1 2212; BEAM_ENERGY_1 = 6500.;",
				" BEAM_2 2212; BEAM_ENERGY_2 = 6500.;",
				" HARD_DECAYS On;",
				" HDH_STATUS[24,2,-1]=0",
				" HDH_STATUS[24,4,-3]=0",
				" HDH_STATUS[-24,-2,1]=0",
				" HDH_STATUS[-24,-4,3]=0",
				" STABLE[24] 0; STABLE[6] 0; WIDTH[6] 0;",
				" NLO_SMEAR_THRESHOLD 1;",
				" NLO_SMEAR_POWER 2;",
				"}(run)",
				"(processes){",
				" Process : 93 93 ->  6 -6 93{NJET}",
				" Order_EW 0;",
				" CKKW sqr(QCUT/E_CMS);",
				" NLO_QCD_Mode MC@NLO {LJET};",
				" ME_Generator Amegic {LJET};",
				" RS_ME_Generator Comix {LJET};",
				" Loop_Generator LOOPGEN {LJET};",
				" Integration_Error 0.05;",
				" End process",
				"}(processes)"                                                  ),
                             )
)

ProductionFilterSequence = cms.Sequence(generator)

