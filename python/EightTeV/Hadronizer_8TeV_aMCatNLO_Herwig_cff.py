import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version    = cms.untracked.string('$Revision: 1.1 $'),
	name       = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/Hadronizer_8TeV_aMCatNLO_Herwig_cff.py,v $'),
	annotation = cms.untracked.string('Showering of generic aMCatNLO events with Herwig, 8 TeV')
)

generator = cms.EDFilter("Herwig6HadronizerFilter",
        comEnergy = cms.double(8000.0),
        doMPInteraction = cms.bool(False),
        emulatePythiaStatusCodes = cms.untracked.bool(True),
        filterEfficiency = cms.untracked.double(1.0),
        herwigHepMCVerbosity = cms.untracked.bool(False),
        herwigVerbosity = cms.untracked.int32(0),
        lhapdfSetPath = cms.untracked.string(''),
        maxEventsToPrint = cms.untracked.int32(0),
        printCards = cms.untracked.bool(False),
        useJimmy = cms.bool(False),
        doMatching = cms.untracked.bool(False),
        nMatch = cms.untracked.int32(0),
        inclusiveMatching = cms.untracked.bool(True),
        matchingScale = cms.untracked.double(0.0), 
        ExternalDecays = cms.PSet(
            Photos = cms.untracked.PSet(),
            parameterSets = cms.vstring( "Photos" )
        ),
        HerwigParameters = cms.PSet(
                herwigUEsettings = cms.vstring(
                       'JMUEO     = 1       ! multiparton interaction model',
                       'PTJIM     = 4.189   ! 2.8x(sqrt(s)/1.8TeV)^0.27 @ 8 TeV',
                       'JMRAD(73) = 1.8     ! inverse proton radius squared',
                       'PRSOF     = 0.0     ! prob. of a soft underlying event',
                       'MAXER     = 1000000 ! max error'
                ),
                herwigMcatnlo = cms.vstring(
                        'PTMIN      = 0.5    ! minimum pt in hadronic jet',
                        'IPROC      = -18000  ! proc should be -ve'
                ),
                parameterSets = cms.vstring('herwigUEsettings',
                                            'herwigMcatnlo')
        )
)

