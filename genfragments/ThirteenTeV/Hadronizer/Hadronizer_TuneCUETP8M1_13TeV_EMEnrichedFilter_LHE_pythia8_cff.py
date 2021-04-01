import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
                                     args=cms.vstring(
                                         'GRIDPACK'),
                                     nEvents=cms.untracked.uint32(5000),
                                     numberOfParameters=cms.uint32(1),
                                     outputFile=cms.string(
                                         'cmsgrid_final.lhe'),
                                     scriptName=cms.FileInPath(
                                         'GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
                                     )

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint=cms.untracked.int32(1),
                         pythiaPylistVerbosity=cms.untracked.int32(1),
                         filterEfficiency=cms.untracked.double(1.0),
                         pythiaHepMCVerbosity=cms.untracked.bool(False),
                         comEnergy=cms.double(13000.),
                         PythiaParameters=cms.PSet(
                             pythia8CommonSettingsBlock,
                             pythia8CUEP8M1SettingsBlock,
                             parameterSets=cms.vstring('pythia8CommonSettings',
                                                       'pythia8CUEP8M1Settings',
                                                       )
                         )
                         )

genParticlesForFilter = cms.EDProducer(
    "GenParticleProducer",
    saveBarCodes=cms.untracked.bool(True),
    src=cms.InputTag("generator", "unsmeared"),
    abortOnUnknownPDGCode=cms.untracked.bool(False),
)

emenrichingfilter = cms.EDFilter(
    "EMEnrichingFilter",
    filterAlgoPSet=cms.PSet(
        isoGenParETMin=cms.double(20.0),
        isoGenParConeSize=cms.double(0.1),
        clusterThreshold=cms.double(20.0),
        isoConeSize=cms.double(0.2),
        hOverEMax=cms.double(0.5),
        tkIsoMax=cms.double(5.0),
        caloIsoMax=cms.double(10.0),
        requireTrackMatch=cms.bool(False),
        genParSource=cms.InputTag("genParticlesForFilter"),
    ),
)

ProductionFilterSequence = cms.Sequence(
    generator * (genParticlesForFilter + emenrichingfilter)
)
