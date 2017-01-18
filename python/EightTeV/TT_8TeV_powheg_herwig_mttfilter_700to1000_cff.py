import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version    = cms.untracked.string('$Revision: 1.1 $'),
	name       = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/TT_8TeV_powheg_herwig_mttfilter_700to1000_cff.py,v $'),
	annotation = cms.untracked.string('Showering of Powheg TTbar events with Herwig+Jimmy, 7 TeV, AUET2')
)

generator = cms.EDFilter("Herwig6HadronizerFilter",
    HerwigParameters = cms.PSet(
        parameterSets = cms.vstring('herwigParams', 'herwigAUET2settings'),
        herwigParams  = cms.vstring(
            'RMASS(5) = 4.8       ! Set b mass.',
            'RMASS(6) = 172.5     ! Set top mass.',
            'SOFTME   = 0         ! Do not use soft matrix-element corrections.',
        ),
        herwigAUET2settings = cms.vstring(
            'MODPDF(1)  = 10042      ! PDF set according to LHAGLUE',
            'MODPDF(2)  = 10042      ! CTEQ6L1',
            'ISPAC      = 2',
            'QSPAC      = 2.5',
            'PTRMS      = 1.2',
            'PTJIM      = 4.550      ! 3.224 * (runArgs.ecmEnergy/1800.)**0.231 @ 8 TeV',
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
    comEnergy = cms.double(8000.0),
    lhapdfSetPath = cms.untracked.string(''),
    printCards = cms.untracked.bool(False),
    crossSection = cms.untracked.double(149.6),
    maxEventsToPrint = cms.untracked.int32(0)
)

ttgenfilter = cms.EDFilter("MCParticlePairFilter",
                               Status = cms.untracked.vint32(3, 3),
                               MinPt = cms.untracked.vdouble(0., 0.),
                               ParticleCharge = cms.untracked.int32(0),
                               ParticleID1 = cms.untracked.vint32(6),
                               ParticleID2 = cms.untracked.vint32(6),
                               MinInvMass =  cms.untracked.double(700.),
                               MaxInvMass =  cms.untracked.double(1000.)
                           )

ProductionFilterSequence = cms.Sequence(generator*ttgenfilter)
