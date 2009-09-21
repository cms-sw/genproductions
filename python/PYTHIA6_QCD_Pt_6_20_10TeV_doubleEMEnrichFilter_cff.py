import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(0.0270),
    crossSection = cms.untracked.double(31400000000.),
    comEnergy = cms.double(10000.0),  # center of mass energy in GeV
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=1               ! QCD high pT processes',
                                        'CKIN(3)=6.          ! minimum pt hat for hard interactions',
                                        'CKIN(4)=20.          ! maximum pt hat for hard interactions'
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


doubleEMenrichingfilter = cms.EDFilter("doubleEMEnrichingFilter",
                                       filterAlgoPSet = cms.PSet(requireTrackMatch = cms.bool(False),
                                                                 caloIsoMax = cms.double(3.0),
                                                                 isoGenParConeSize = cms.double(0.1),
                                                                 tkIsoMax = cms.double(3.0),
                                                                 isoConeSize = cms.double(0.2),
                                                                 isoGenParETMin = cms.double(4.0),
                                                                 hOverEMax = cms.double(0.5),
                                                                 clusterThreshold = cms.double(4.0),
                                                                 seedThreshold = cms.double(3.5),
                                                                 eTThreshold = cms.double(3.0),
                                                                 genParSource = cms.InputTag("genParticlesForFilter")
                                                                 )
                                       )


# enter below the configuration metadata (only a description is needed, the rest is filled in by cvs)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCD_Pt_6_20_7TeV_doubleEMEnrichFilter_cff.py,v $'),
    annotation = cms.untracked.string('double em-enriching filtered QCD pthat 6-20, 10 TeV')
)

# add your filters to this sequence
ProductionFilterSequence = cms.Sequence(generator * (genParticlesForFilter + doubleEMenrichingfilter))
