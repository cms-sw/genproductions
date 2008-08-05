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
    diffTopology = cms.int32(1),
    h1fit = cms.int32(2),	
    HerwigParameters = cms.PSet(	
	parameterSets = cms.vstring('SD1InclusiveZmumu'),
	SD1InclusiveZmumu = cms.vstring(
		'NSTRU      = 14         ! H1 Pomeron Fit B',
                'Q2WWMN     = 1E-6       ! Minimum |t|',
                'Q2WWMX     = 4.0        ! Maximum |t|',
                'YWWMIN     = 1E-6       ! Minimum xi',
                'YWWMAX     = 0.2        ! Maximum xi',
		'EMMIN      = 40         ! Minimum DY mass',
                'IPROC      = 11353      ! Process PomP -> Z/gamma* -> mumu',
                'MODPDF(1)  = -1         ! Set MODPDF',
                'MODPDF(2)  = 10150      ! Set MODPDF CTEQ61'),
    )
)	

pomwigfilter = cms.EDFilter("PomwigFilter")

ProductionFilterSequence = cms.Sequence(pomwigfilter)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: $'),
    name = cms.untracked.string('$Source: $'),
    annotation = cms.untracked.string('POMWIG SD plus Z->mumu Mmin 40 GeV at 10TeV')
)

