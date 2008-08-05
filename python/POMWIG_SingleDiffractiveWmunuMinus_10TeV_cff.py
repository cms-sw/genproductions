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
	parameterSets = cms.vstring('SD2InclusiveWmunu'),
	SD2InclusiveWmunu = cms.vstring(
		'NSTRU      = 14         ! H1 Pomeron Fit B',
                'Q2WWMN     = 1E-6       ! Minimum |t|',
                'Q2WWMX     = 4.0        ! Maximum |t|',
                'YWWMIN     = 1E-6       ! Minimum xi',
                'YWWMAX     = 0.2        ! Maximum xi',
                'IPROC      = 11452      ! Process PomP -> W -> munu',
                'MODPDF(1)  = 10150      ! Set MODPDF CTEQ61',
                'MODPDF(2)  = -1         ! Set MODPDF'),
    )
)	

pomwigfilter = cms.EDFilter("PomwigFilter")

ProductionFilterSequence = cms.Sequence(pomwigfilter)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: $'),
    name = cms.untracked.string('$Source: $'),
    annotation = cms.untracked.string('POMWIG SD minus W->munu at 10TeV')
)

