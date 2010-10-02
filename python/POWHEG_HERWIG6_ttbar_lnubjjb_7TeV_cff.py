import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter(
    "Herwig6HadronizerFilter",
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(65.83),
    doMPInteraction = cms.bool(True),
    emulatePythiaStatusCodes = cms.untracked.bool(True),
    filterEfficiency = cms.untracked.double(1.0),
    herwigHepMCVerbosity = cms.untracked.bool(False),
    herwigVerbosity = cms.untracked.int32(0),
    lhapdfSetPath = cms.untracked.string(''),
    maxEventsToPrint = cms.untracked.int32(5),
    printCards = cms.untracked.bool(False),
    useJimmy = cms.bool(True),
    HerwigParameters = cms.PSet(
        herwigUEsettings = cms.vstring(
            'JMUEO     = 1         ! multiparton interaction model',
            'PTJIM     = 4.449     ! 2.8x(sqrt(s)/1.8TeV)^0.27 @ 10 TeV',
            'JMRAD(73) = 1.8       ! inverse proton radius squared',
            'PRSOF     = 0.0       ! prob. of a soft underlying event',
            'MAXER     = 1000000   ! max error',
            ),
        herwigPowheg = cms.vstring(
            'PTMIN      = 0.5    ! minimum pt in hadronic jet',
            'IPROC      = -1706  ! - = dont generate hard event, 1700 = hvq, 6=top'
            ),
        parameterSets = cms.vstring(
            'herwigUEsettings',
            'herwigPowheg'
            )
        )
    )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/Attic/POWHEG_HERWIG6_ttbar_lnubjjb_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('POWHEG + HERWIG6 - ttbar -> lnubjjb at 7TeV')
    )

