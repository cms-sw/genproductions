import FWCore.ParameterSet.Config as cms

gridpackName = 'DYJets_M-10-50_PtLL70-100'

externalLHEProducer = cms.EDProducer('ExternalLHEProducer',
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_madgraph_gridpack.sh'),
    outputFile = cms.string(gridpackName+'_final.lhe'),
    numberOfParameters = cms.uint32(10),
    args = cms.vstring('slc5_amd64_gcc472/8TeV/madgraph/V5_1.5.11/'+gridpackName+'/v1',
    gridpackName,'false','false','zjets','5','40','false','0','3'),
    nEvents = cms.uint32(10000)
)
