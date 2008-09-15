# Auto generated configuration file
# using: 
# Revision: 1.77.2.1 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/python/PYTHIA8_PhotonJetpt35_10TeV_cff.py -s GEN:ProductionFilterSequence --eventcontent RAWSIM --datatier GEN --conditions FrontierConditions_GlobalTag,IDEAL_V9::All -n 10 --no_exec --customise Configuration/GenProduction/PYTHIA8_custom.py
import FWCore.ParameterSet.Config as cms

process = cms.Process('GEN')

# import of standard configurations
process.load('Configuration/StandardSequences/Services_cff')
process.load('FWCore/MessageService/MessageLogger_cfi')
process.load('Configuration/StandardSequences/MixingNoPileUp_cff')
process.load('Configuration/StandardSequences/GeometryPilot2_cff')
process.load('Configuration/StandardSequences/MagneticField_38T_cff')
process.load('Configuration/StandardSequences/Generator_cff')
process.load('Configuration/StandardSequences/VtxSmearedEarly10TeVCollision_cff')
process.load('Configuration/StandardSequences/FrontierConditions_GlobalTag_cff')
process.load('Configuration/EventContent/EventContent_cff')

process.configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    annotation = cms.untracked.string('PYTHIA8 Photon + Jet for 35 < pT at 10TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA8_PhotonJetpt35_10TeV_cff.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(10)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("Pythia8Source",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.64),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(19000.0),
    maxEventsToPrint = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring('Main:timesAllowErrors    = 10000', 
            'ParticleDecays:limitTau0 = on', 
            'ParticleDecays:tau0Max   = 10.', 
            'PromptPhoton:all         = on', 
            'PhaseSpace:pTHatMin      = 35.'),
        parameterSets = cms.vstring('processParameters')
    )
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('PYTHIA8_PhotonJetpt35_10TeV_cff_GEN.root'),
    dataset = cms.untracked.PSet(
        dataTier = cms.untracked.string('GEN'),
        filterName = cms.untracked.string('')
    ),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('generation_step')
    )
)

# Additional output definition

# Other statements
process.GlobalTag.globaltag = 'IDEAL_V9::All'
process.photonfilter = cms.EDFilter("MCSingleParticleFilter",
    MaxEta = cms.untracked.vdouble(2.4),
    MinEta = cms.untracked.vdouble(-2.4),
    MinPt = cms.untracked.vdouble(15.0),
    ParticleID = cms.untracked.vint32(22)
)
process.ProductionFilterSequence = cms.Sequence(process.photonfilter)

# Path and EndPath definitions
process.generation_step = cms.Path(process.ProductionFilterSequence*process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)


# Automatic addition of the customisation function

def customise(process):

	process.genParticles.abortOnUnknownPDGCode = False

	return(process)


# End of customisation function definition

process = customise(process)