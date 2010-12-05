import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
  version = cms.untracked.string('$Revision: 1.1 $'),
  name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/ZH_ZToLL_HToBB_M-135_7TeV-mcatnlo-herwig6.py,v $'),
  annotation = cms.untracked.string('Fall 10: MC@NLO 3.4 Z->ll H(bb), mH=135 events with Herwig+Jimmy, 7 TeV')
)

generator = cms.EDFilter("Herwig6HadronizerFilter",
  comEnergy = cms.double(7000.0),
  crossSection = cms.untracked.double(0.233),
  doMPInteraction = cms.bool(True),
  emulatePythiaStatusCodes = cms.untracked.bool(True),
  filterEfficiency = cms.untracked.double(1.0),
  herwigHepMCVerbosity = cms.untracked.bool(False),
  herwigVerbosity = cms.untracked.int32(0),
  lhapdfSetPath = cms.untracked.string(''),
  maxEventsToPrint = cms.untracked.int32(0),
  printCards = cms.untracked.bool(False),
  useJimmy = cms.bool(True),

  HerwigParameters = cms.PSet(
    herwigUEsettings = cms.vstring(
      'JMUEO     = 1       ! multiparton interaction model',
      'PTJIM     = 4.449   ! 2.8x(sqrt(s)/1.8TeV)^0.27 @ 10 TeV',
      'JMRAD(73) = 1.8     ! inverse proton radius squared',
      'PRSOF     = 0.0     ! prob. of a soft underlying event',
      'MAXER     = 1000000 ! max error'
    ),
                herwigMcatnlo = cms.vstring(
      'PTMIN      = 0.5    ! minimum pt in hadronic jet'
    ),
    parameterSets = cms.vstring('herwigUEsettings',
                                'herwigMcatnlo')
  )
)



