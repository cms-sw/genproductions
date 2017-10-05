import FWCore.ParameterSet.Config as cms
import os

source = cms.Source("EmptySource")

generator = cms.EDFilter("SherpaGeneratorFilter",
  maxEventsToPrint = cms.int32(0),
  filterEfficiency = cms.untracked.double(1.0),
  crossSection = cms.untracked.double(-1),
  SherpaProcess = cms.string('WWbbLO'),
  SherpackLocation = cms.string('slc6_amd64_gcc481/13TeV/sherpa/2.1.1/WWbbLO/v1'),
  SherpackChecksum = cms.string('6a9996a780ba460ffe4fbb4e7dcc30fe'),
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
				" EVENT_OUTPUT=HepMC_GenEvent[HepMC.WWbb.LO]",
				" FSF:=1.; RSF:=1.; QSF:=1.;",
				" SCALES STRICT_METS{FSF*MU_F2}{RSF*MU_R2}{QSF*MU_Q2};",
				" METS_BBAR_MODE 5;",
				" NJET:=1; QCUT:=20.;",
				" ME_SIGNAL_GENERATOR Comix;",
				" EVENT_GENERATION_MODE Weighted;",
				" RESULT_DIRECTORY = Results.WWbb.LO",
				" BEAM_1 2212; BEAM_ENERGY_1 = 6500.;",
				" BEAM_2 2212; BEAM_ENERGY_2 = 6500.;",
				" ACTIVE[5]=1; MASSIVE[5]=1; MASS[5]=4.6",
				" ACTIVE[6]=1; MASSIVE[6]=1; MASS[6]=172.6",
				" HARD_DECAYS On;",
				" HDH_STATUS[24,2,-1]=0",
				" HDH_STATUS[24,4,-3]=0",
				" HDH_STATUS[-24,-2,1]=0",
				" HDH_STATUS[-24,-4,3]=0",
				" STABLE[24] 0;",
				"}(run)",
				"(processes){",
				" Process : 93 93 ->  24 -24 5 -5 93{NJET}",
				" Order_EW 2;",
				" CKKW sqr(QCUT/E_CMS);",
				" Integration_Error 0.05;",
				" End process",
				"}(processes)",
				"(selector){",
				" PT      5     10.   E_CMS",
				" PT     -5     10.   E_CMS",
				" Mass 5 -5     20.   E_CMS",
				"}(selector)"                                                  ),
                             )
)

ProductionFilterSequence = cms.Sequence(generator)

