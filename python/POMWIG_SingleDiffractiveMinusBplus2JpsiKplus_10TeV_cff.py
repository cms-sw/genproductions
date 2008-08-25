import FWCore.ParameterSet.Config as cms

# Pomwig DataCard

source = cms.Source("PomwigSource",
    herwigVerbosity = cms.untracked.int32(1),
    herwigHepMCVerbosity = cms.untracked.bool(False),
    herwigLhapdfVerbosity = cms.untracked.int32(0),
    maxEventsToPrint = cms.untracked.int32(2),
    printCards = cms.untracked.bool(True),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(-1.),
    diffTopology = cms.int32(2),
    h1fit = cms.int32(2),	
    HerwigParameters = cms.PSet(	
        parameterSets = cms.vstring('SD1Bplus2JpsiKplus'),
        SD1Bplus2JpsiKplus = cms.vstring(
		'NSTRU      = 14         ! H1 Pomeron Fit B',
                'Q2WWMN     = 1E-6       ! Minimum |t|',
                'Q2WWMX     = 4.0        ! Maximum |t|',
                'YWWMIN     = 1E-6       ! Minimum xi',
                'YWWMAX     = 0.2        ! Maximum xi',
                'IPROC      = 11705      ! Process PomP -> bb',
                'MODPDF(1)  = 10150      ! Set MODPDF CTEQ61',
                'MODPDF(2)  = -1         ! Set MODPDF'),
    ),
    enableForcedDecay = cms.untracked.bool(True),
    ForcedDecaysParameters = cms.PSet(
        Ietmp = cms.untracked.vint32(0),
        Ibtmp = cms.untracked.vint32(321),
        Imetmp = cms.untracked.vint32(0),
        Brtmp = cms.untracked.vdouble(1.0),
        Idtmp = cms.untracked.vint32(0),
        Idktmp = cms.untracked.vint32(521),
        Iatmp = cms.untracked.vint32(443),
        Ictmp = cms.untracked.vint32(0)
        ),
)

pomwigfilter = cms.EDFilter("PomwigFilter")
BpFilter = cms.EDFilter("PythiaFilter",
	 ParticleID = cms.untracked.int32(521)
)
jpsifilter = cms.EDFilter("MCSingleParticleFilter", 
         ParticleID = cms.untracked.vint32(443),
         MinPt = cms.untracked.vdouble(0.),
         MinEta = cms.untracked.vdouble(-15.),
         MaxEta = cms.untracked.vdouble(15.)
)     
# -- Filter dimuon kinematical acceptance
MuMuFilter = cms.EDFilter("MCParticlePairFilter", 
        ParticleID1 = cms.untracked.vint32(13),
        ParticleID2 = cms.untracked.vint32(13),
        ParticleCharge = cms.untracked.int32(-1),
	Status = cms.untracked.vint32(1,1),
        MinPt = cms.untracked.vdouble(2.5,2.5),
        MinEta = cms.untracked.vdouble(-2.5,-2.5),
        MaxEta = cms.untracked.vdouble(2.5,2.5),
	MinInvMass = cms.untracked.double(3.0),
	MaxInvMass = cms.untracked.double(3.2)
)

ProductionFilterSequence = cms.Sequence(pomwigfilter*BpFilter*jpsifilter*MuMuFilter)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: $'),
    annotation = cms.untracked.string('POMWIG SD Plus Bplus to JPsi Kplus at 10TeV'),
    name = cms.untracked.string('$Source: $')
)

