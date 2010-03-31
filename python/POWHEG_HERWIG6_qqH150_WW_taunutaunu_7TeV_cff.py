import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter(
    "Herwig6HadronizerFilter",
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(0.00841),
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
            'MODPDF(1) = 10050     ! PDF set according to LHAPDF',
            'MODPDF(2) = 10050     ! CTEQ6m',
            'MODBOS(1) = 4         ! first  W -> tau',
            'MODBOS(2) = 4         ! second W -> tau ',
            ),
        herwigPowheg = cms.vstring(
            'PTMIN      = 0.5    ! minimum pt in hadronic jet',
            'IPROC      = -1910     ! - = dont generate hard event, 1000 = qqH   10 = H->WW'
            ),
        parameterSets = cms.vstring(
            'herwigUEsettings',
            'herwigPowheg'
            )
        )
    )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/POWHEG_HERWIG6_qqH150_WW_taunutaunu_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('POWHEG + HERWIG6 - VBF Higgs -> WW -> taunutaunu at 7TeV')
    )
