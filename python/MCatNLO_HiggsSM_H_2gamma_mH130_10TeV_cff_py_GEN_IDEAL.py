# Auto generated configuration file
# using: 
# Revision: 1.77 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: Configuration/GenProduction/python/MCatNLO_HiggsSM_H_2gamma_mH130_10TeV_cff.py -s GEN:ProductionFilterSequence --eventcontent RAWSIM --datatier GEN --conditions FrontierConditions_GlobalTag,IDEAL_V9::All -n 1000 --no_exec --customise Configuration/GenProduction/MCatNLO_custom.py
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
    version = cms.untracked.string('$Revision: 1.2 $'),
    annotation = cms.untracked.string('MC@NLO HiggsSM H 2gamma mH130 at 10TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/MCatNLO_HiggsSM_H_2gamma_mH130_10TeV_cff.py,v $')
)
process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(1000)
)
process.options = cms.untracked.PSet(
    Rethrow = cms.untracked.vstring('ProductNotFound')
)
# Input source
process.source = cms.Source("MCatNLOSource",
    HerwigParameters = cms.PSet(
        defaultHerwig = cms.vstring(),
        parameterSets = cms.vstring('defaultHerwig')
    ),
    doMPInteraction = cms.untracked.bool(True),
    useJimmy = cms.untracked.bool(True),
    stringFileName = cms.untracked.string('stringInput.txt'),
    processNumber = cms.untracked.int32(-1612),
    filterEfficiency = cms.untracked.double(1.0),
    doHardEvents = cms.untracked.bool(True),
    herwigVerbosity = cms.untracked.int32(0),
    MCatNLOParameters = cms.PSet(
        mcatnloReadin = cms.vstring('ECM=10000', 
            'FREN=1', 
            'FFACT=1', 
            'HVQMASS=175', 
            'TWIDTH=1.4', 
            'WMASS=80.41', 
            'WWIDTH=2.124', 
            'ZMASS=91.17', 
            'ZWIDTH=2.495', 
            'HGGMASS=130', 
            'HGGWIDTH=0.00497', 
            'IBORNHGG=1', 
            'V1GAMMAX=30', 
            'V1MASSINF=0', 
            'V1MASSSUP=0', 
            'V2GAMMAX=30', 
            'V2MASSINF=0', 
            'V2MASSSUP=0', 
            'HGAMMAX=30', 
            'HMASSINF=0', 
            'HMASSSUP=0', 
            'UMASS=0.32', 
            'DMASS=0.32', 
            'SMASS=0.5', 
            'CMASS=1.55', 
            'BMASS=4.8', 
            'GMASS=0.75', 
            'VUD=0.9748', 
            'VUS=0.2225', 
            'VUB=0.0036', 
            'VCD=0.2225', 
            'VCS=0.9740', 
            'VCB=0.041 ', 
            'VTD=0.009 ', 
            'VTS=0.0405', 
            'VTB=0.9992', 
            'AEMRUN=YES', 
            'IVCODE=1', 
            'IL1CODE=1', 
            'IL2CODE=1', 
            'PDFGROUP=LHAPDF', 
            'PDFSET=10050', 
            'LAMBDAFIVE=0.2262', 
            'SCHEMEOFPDF=MS', 
            'LAMBDAHERW=-1', 
            'FPREFIX=Hgg130', 
            'EVPREFIX=Hgg130', 
            'WGTTYPE=1', 
            'PDFLIBRARY=HWLHAPDF', 
            'HERPDF=EXTPDF', 
            'LHAPATH="/afs/cern.ch/sw/lcg/external/MCGenerators/lhapdf/5.4.0/share/PDFsets"', 
            'LHAOFL=FREEZE'),
        parameterSets = cms.vstring('mcatnloReadin')
    ),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(0.0334),
    printCards = cms.untracked.bool(False),
    numHardEvents = cms.untracked.int32(10000),
    maxEventsToPrint = cms.untracked.int32(0),
    herwigHepMCVerbosity = cms.untracked.bool(False),
    mcatnloVerbosity = cms.untracked.int32(0)
)

# Output definition
process.output = cms.OutputModule("PoolOutputModule",
    outputCommands = process.RAWSIMEventContent.outputCommands,
    fileName = cms.untracked.string('MCatNLO_HiggsSM_H_2gamma_mH130_10TeV_cff_py_GEN.root'),
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
process.filterMCatNLO = cms.EDFilter("MCatNLOFilter")
process.ProductionFilterSequence = cms.Sequence(process.filterMCatNLO)

# Path and EndPath definitions
process.generation_step = cms.Path(process.ProductionFilterSequence*process.pgen)
process.out_step = cms.EndPath(process.output)

# Schedule definition
process.schedule = cms.Schedule(process.generation_step,process.out_step)


# Automatic addition of the customisation function

def customise(process):

	process.genParticles.abortOnUnknownPDGCode = False
	process.source.numHardEvents = process.maxEvents.input

	return(process)


# End of customisation function definition

process = customise(process)