# Auto generated configuration file
# using: 
# Revision: 1.77.2.1 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/MadGraph_SingleTop_LHEProducer_t_15_84_10TeV_cff.py -s GEN:ProducerSourceSequence --eventcontent RAWSIM --datatier GEN --conditions FrontierConditions_GlobalTag,IDEAL_V9::All -n 100 --no_exec --customise=Configuration/GenProduction/generatorProducer.py
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
    version = cms.untracked.string('$Revision: 1.5 $'),
    annotation = cms.untracked.string('MadGraph single top t-channel at 10TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/MadGraph_XQCUT15_10TeV_GEN_cff.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(100)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("PoolSource",
    skipEvents = cms.untracked.uint32(0),
    fileNames = cms.untracked.vstring('file:LHEtoEDM.root')
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('MadGraph_SingleTop_LHEProducer_t_15_84_10TeV_cff_py_GEN.root'),
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
process.singleTopFilter = cms.EDFilter("STFilter",
    produceHistos = cms.bool(False),
    TH1bEtaFiltered = cms.PSet(
        xmin = cms.double(-8.0),
        Nbinx = cms.int32(100),
        xmax = cms.double(8.0)
    ),
    histOutFile = cms.untracked.string(''),
    TH1bPt = cms.PSet(
        xmin = cms.double(0.0),
        Nbinx = cms.int32(150),
        xmax = cms.double(150.0)
    ),
    debuglvl = cms.untracked.int32(0),
    TH1bEta = cms.PSet(
        xmin = cms.double(-8.0),
        Nbinx = cms.int32(100),
        xmax = cms.double(8.0)
    ),
    TH1bPtFiltered = cms.PSet(
        xmin = cms.double(0.0),
        Nbinx = cms.int32(150),
        xmax = cms.double(150.0)
    ),
    pTMax = cms.double(15.84)
)
process.generator = cms.EDProducer("LHEProducer",
    hadronisation = cms.PSet(
        pythiaUESettings = cms.vstring('MSTJ(11)=3     ! Choice of the fragmentation function', 
            'MSTJ(22)=2     ! Decay those unstable particles', 
            'PARJ(71)=10 .  ! for which ctau  10 mm', 
            'MSTP(2)=1      ! which order running alphaS', 
            'MSTP(33)=0     ! no K factors in hard cross sections', 
            'MSTP(51)=10042     ! CTEQ6L1 structure function chosen', 
            'MSTP(52)=2     ! work with LHAPDF', 
            'MSTP(81)=1     ! multiple parton interactions 1 is Pythia default', 
            'MSTP(82)=4     ! Defines the multi-parton model', 
            'MSTU(21)=1     ! Check on possible errors during program execution', 
            'PARP(82)=1.8387   ! pt cutoff for multiparton interactions', 
            'PARP(89)=1960. ! sqrts for which PARP82 is set', 
            'PARP(83)=0.5   ! Multiple interactions: matter distrbn parameter', 
            'PARP(84)=0.4   ! Multiple interactions: matter distribution parameter', 
            'PARP(90)=0.16  ! Multiple interactions: rescaling power', 
            'PARP(67)=2.5    ! amount of initial-state radiation', 
            'PARP(85)=1.0  ! gluon prod. mechanism in MI', 
            'PARP(86)=1.0  ! gluon prod. mechanism in MI', 
            'PARP(62)=1.25   ! ', 
            'PARP(64)=0.2    ! ', 
            'MSTP(91)=1     !', 
            'PARP(91)=2.1   ! kt distribution', 
            'PARP(93)=15.0  ! '),
        pythiaCMSDefaults = cms.vstring('PMAS(5,1)=4.4    ! b quarks mass', 
            'PMAS(6,1)=172.4  ! t quarks mass', 
            'MSTJ(1)=1        !...Fragmentation/hadronization on or off', 
            'MSTP(61)=1       ! Parton showering on or off', 
            'MSEL=0           ! User defined processes/Full user control'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'pythiaCMSDefaults'),
        generator = cms.string('Pythia6')
    )
)
process.ProducerSourceSequence = cms.Sequence(process.generator*process.singleTopFilter)

# Path and EndPath definitions
process.generation_step = cms.Path(process.ProducerSourceSequence*process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)


# Automatic addition of the customisation function

def customise(process):
	process.RandomNumberGeneratorService.generator = process.RandomNumberGeneratorService.theSource

	process.genParticles.abortOnUnknownPDGCode = False
	process.genParticles.src = 'generator'
	process.genParticleCandidates.src = 'generator'
	process.genEventWeight.src = 'generator'
	process.genEventScale.src = 'generator'
	process.genEventPdfInfo.src = 'generator'

	process.VtxSmeared.src = 'generator'

	try:
		process.g4SimHits.Generator.HepMCProductLabel = 'generator'
		process.mergedtruth.HepMCDataLabels.append('generator')
	except:
		pass

	process.output.outputCommands.append('keep *_source_*_*')
	process.output.outputCommands.append('keep *_generator_*_*')

	return process


# End of customisation function definition

process = customise(process)