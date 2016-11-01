import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
   comEnergy = cms.double(8000.0),
   crossSection = cms.untracked.double(0.00463),
   filterEfficiency = cms.untracked.double(1),
   maxEventsToPrint = cms.untracked.int32(0),
   pythiaHepMCVerbosity = cms.untracked.bool(False),
   pythiaPylistVerbosity = cms.untracked.int32(0),

   PythiaParameters = cms.PSet(
      processParameters = cms.vstring(
         'Main:timesAllowErrors    = 10000',
         'ParticleDecays:limitTau0 = on',
         'ParticleDecays:tauMax = 10',
         'Tune:pp 5',
         'Tune:ee 3',
         'PDF:pSet = 5',
         'ExtraDimensionsLED:ffbar2llbar = on', 
         'ExtraDimensionsLED:gg2llbar = on', 
         'PhaseSpace:mHatMin = 1050',
         'ExtraDimensionsLED:CutOffmode = 0',
         'ExtraDimensionsLED:LambdaT = 4000'
      ),
      parameterSets = cms.vstring('processParameters')
   )
)

configurationMetadata = cms.untracked.PSet(
   version = cms.untracked.string('\$Revision: 1.0 $'),
   name = cms.untracked.string('\$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/ADD_Dilepton_LambdaT_4000_8TeV_pythia8_cff.py,v $'),
   annotation = cms.untracked.string('2012 sample with PYTHIA8 at 8 TeV: ADD Dilepton samples with LambdaT = 4000 GeV, Tune4C, pdf: MSTW 2008 LO')
)
