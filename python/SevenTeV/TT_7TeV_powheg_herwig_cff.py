import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version    = cms.untracked.string('$Revision: 1.1 $'),
	name       = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/SevenTeV/TT_7TeV_powheg_herwig_cff.py,v $'),
	annotation = cms.untracked.string('Showering of Powheg TTbar events with Herwig+Jimmy, 7 TeV, AUET2')
)

generator = cms.EDFilter("Herwig6HadronizerFilter",
    HerwigParameters = cms.PSet(
        parameterSets = cms.vstring('herwigParams', 'herwigAUET2settings', 
            'herwigMcatnlo'),
        herwigMcatnlo = cms.vstring('PTMIN      = 0.5    ! minimum pt in hadronic jet'),
        herwigParams  = cms.vstring(
            'RMASS(1) = 0.32',
            'RMASS(2) = 0.32',
            'RMASS(3) = 0.5',
            'RMASS(4) = 1.55',
            'RMASS(5) = 4.95',
            'RMASS(6) = 172.5',
            'RMASS(7) = 0.32',
            'RMASS(8) = 0.32',
            'RMASS(9) = 0.5',
            'RMASS(10) = 1.55',
            'RMASS(11) = 4.95',
            'RMASS(13) = 0.75',
            'RMASS(198) = 80.42',
            'RMASS(199) = 80.42',
            'SOFTME = 0',
            'NOWGT = 0',
            'NEGWTS = 1',
            'WGTMAX = 1',
            'AVABW = 1',
            'RLTIM(6) = 1e-23',
            'RLTIM(12) = 1e-23',
        ),
        herwigAUET2settings = cms.vstring(
            'MODPDF(1)  = 10042      ! PDF set according to LHAGLUE',
            'MODPDF(2)  = 10042      ! CTEQ6L1',
            #'JMUEO      = 1          ! multiparton interaction model',
            'ISPAC      = 2',
            'QSPAC      = 2.5',
            'PTRMS      = 1.2',
            'PTJIM      = 4.412      ! 3.224 * (runArgs.ecmEnergy/1800.)**0.231 @ 7 TeV',
            'JMRAD(73)  = 2.386      ! inverse proton radius squared',
            'PRSOF      = 0.0        ! prob. of a soft underlying event',
            'MAXER      = 1000000    ! max error'
        )
    ),
    doMPInteraction = cms.bool(True),
    useJimmy = cms.bool(True),
    herwigHepMCVerbosity = cms.untracked.bool(False),
    filterEfficiency = cms.untracked.double(1.0),
    herwigVerbosity = cms.untracked.int32(0),
    emulatePythiaStatusCodes = cms.untracked.bool(True),
    comEnergy = cms.double(7000.0),
    lhapdfSetPath = cms.untracked.string(''),
    printCards = cms.untracked.bool(False),
    crossSection = cms.untracked.double(211.1),
    maxEventsToPrint = cms.untracked.int32(0)
)
