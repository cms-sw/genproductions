import FWCore.ParameterSet.Config as cms
import os

source = cms.Source("EmptySource")

generator = cms.EDFilter("SherpaGeneratorFilter",
  maxEventsToPrint = cms.int32(0),
  filterEfficiency = cms.untracked.double(1.0),
  crossSection = cms.untracked.double(-1),
  SherpaProcess = cms.string('ttH01mcnlo'),
  SherpackLocation = cms.string('/afs/cern.ch/cms/generators/www/slc6_amd64_gcc491/sherpa/2.1.1/13TeV/sherpa_ttH_Hinclusive_01j_MCatNLO_OpenLoops_13TeV_MASTER_V2.tgz'),
  SherpackChecksum = cms.string('f96b3a4c7a8ba863ffe5c767d1770a60'),
  FetchSherpack = cms.bool(True),
  SherpaPath = cms.string('./'),
  SherpaPathPiece = cms.string('./'),
  SherpaResultDir = cms.string('Result'),
  SherpaDefaultWeight = cms.double(1.0),
  SherpaParameters = cms.PSet(parameterSets = cms.vstring(
                             "Run"),
                              Run = cms.vstring(
				" # sample generation ttH+0/1j MC@NLO: 13TeV, NNPDF30, scale 1*235, QCut 30",
				" #",
				"(run){",
				" EVENTS 100k;",
				" EVENT_GENERATION_MODE Weighted;",
				" EVENT_OUTPUT=HepMC_GenEvent[Events];",
				" ERROR 0.99;",
				" SCF:=1;",
				" SCALES VAR{sqr(SCF*235)}",
				" CORE_SCALE QCD;",
				" METS_BBAR_MODE 5;",
				" QCUT:=30.;",
				" LOOPGEN:=OpenLoops;",
				" ME_SIGNAL_GENERATOR Comix Amegic LOOPGEN;",
				" OL_PREFIX=/cvmfs/cms.cern.ch/slc6_amd64_gcc491/external/openloops/1.0.1",
				" EBEAM:=13;",
				" BEAM_1 2212 EBEAM*1000/2;",
				" BEAM_2 2212 EBEAM*1000/2;",
				" HARD_DECAYS On;",
				" STABLE[24] 0;",
				" STABLE[6] 0;",
				" WIDTH[6] 0;",
				" WIDTH[25] 0;",
				" NLO_SMEAR_THRESHOLD 1;",
				" NLO_SMEAR_POWER 2;",
				" PDF_LIBRARY LHAPDFSherpa;",
				" PDF_SET NNPDF30_nlo_as_0118;",
				" USE_PDF_ALPHAS=1;",
				"}(run)",
				"(processes){",
				" Process 93 93 -> 6 -6 25 93{1};",
				" NLO_QCD_Mode MC@NLO;",
				" Order_EW 1;",
				" CKKW sqr(QCUT/E_CMS);",
				" ME_Generator Amegic;",
				" RS_ME_Generator Comix;",
				" Loop_Generator LOOPGEN;",
				" End process",
				"}(processes)"
                                                  ),
                             )
)

ProductionFilterSequence = cms.Sequence(generator)
