import FWCore.ParameterSet.Config as cms

# link to cards:
# https://github.com/cms-sw/genproductions/tree/118394f305ad7358c41939606b359761ad0afe5f/bin/MadGraph5_aMCatNLO/cards/production/13TeV/exo_diboson/Spin-2/BulkGraviton_WW_inclu/BulkGraviton_WW_inclu_narrow_M1000


externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc6_amd64_gcc481/13TeV/madgraph/V5_2.2.2/exo_diboson/Spin-2/BulkGraviton_WW_inclu/narrow/v1/BulkGraviton_WW_inclu_narrow_M1000_tarball.tar.xz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)
