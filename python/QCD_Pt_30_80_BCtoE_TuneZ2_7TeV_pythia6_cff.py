import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(0.00230),
    crossSection = cms.untracked.double(59480000.),                                                  
    comEnergy = cms.double(7000.0),  # center of mass energy in GeV
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=1                 ! QCD high pT processes',
                                        'CKIN(3)=30.          ! minimum pt hat for hard interactions',
                                        'CKIN(4)=80.          ! maximum pt hat for hard interactions'
                                        ),
        parameterSets = cms.vstring('pythiaUESettings', 
                                    'processParameters')
        )
)


genParticlesForFilter = cms.EDProducer("GenParticleProducer",
    saveBarCodes = cms.untracked.bool(True),
    src = cms.InputTag("generator"),
    abortOnUnknownPDGCode = cms.untracked.bool(True)
)

bctoefilter = cms.EDFilter("BCToEFilter",
                           filterAlgoPSet = cms.PSet(eTThreshold = cms.double(10),
                                                     genParSource = cms.InputTag("genParticlesForFilter")
                                                     )
                           )


# enter below the configuration metadata (only a description is needed, the rest is filled in by cvs)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/Attic/PYTHIA6_QCD_Pt_30_80_7TeV_BCToEFilter_cff.py,v $'),
    annotation = cms.untracked.string('b/c->e filtered QCD pthat 30-80, 7 TeV')
)

# add your filters to this sequence
ProductionFilterSequence = cms.Sequence(generator * (genParticlesForFilter + bctoefilter))

