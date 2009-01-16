import FWCore.ParameterSet.Config as cms


#
# NOTE: it's a MANDATORY that the 2 parameters match, ECM and comEnergy
#       a mismatch will result in job abort on inconsistent init
#
source = cms.Source("MCatNLOSource",
    HerwigParameters = cms.PSet(
        defaultHerwig = cms.vstring(),
        parameterSets = cms.vstring('defaultHerwig')
    ),
    doMPInteraction = cms.untracked.bool(True),
    processNumber = cms.untracked.int32(-1706), #ttbar
    herwigHepMCVerbosity = cms.untracked.bool(False),
    doHardEvents = cms.untracked.bool(True),
    herwigVerbosity = cms.untracked.int32(0),
    comEnergy = cms.untracked.double(10000.),
    MCatNLOParameters = cms.PSet(
        mcatnloReadin = cms.vstring('TWIDTH=1.4', 
            'ECM=10000', 
            'FREN=1', 
            'FFACT=1', 
            'HVQMASS=172.4', 
            'WMASS=80.419', 
            'WWIDTH=2.124', 
            'ZMASS=91.188', 
            'ZWIDTH=2.495', 
            'HGGMASS=165', 
            'HGGWIDTH=1.04', 
            'IBORNHGG=1', 
            'V1GAMMAX=-1', 
            'V1MASSINF=1', 
            'V1MASSSUP=10000', 
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
            'BMASS=4.95', 
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
            'IL1CODE=7', 
            'IL2CODE=7', 
            'PDFGROUP=LHAPDF', 
            'PDFSET=10050', #CTEQ6M PDFs
            'LAMBDAFIVE=-1', 
            'SCHEMEOFPDF=MS', 
            'LAMBDAHERW=-1', 
            'FPREFIX=ttbar', 
            'EVPREFIX=ttbar', 
            'WGTTYPE=1', 
            'PDFLIBRARY=HWLHAPDF', 
            'HERPDF=EXTPDF', 
            'LHAPATH="/home/xv/fabstoec/LHAPDF"', 
            'LHAOFL=FREEZE'),
        parameterSets = cms.vstring('mcatnloReadin')
    ),
    mcatnloVerbosity = cms.untracked.int32(0),
    lhapdfSetPath = cms.untracked.string(''),
    printCards = cms.untracked.bool(False),
    numHardEvents = cms.untracked.int32(10000),
    maxEventsToPrint = cms.untracked.int32(0),
    stringFileName = cms.untracked.string('stringInput.txt'),
    useJimmy = cms.untracked.bool(True)
)


mcatnlofilter = cms.EDFilter("MCatNLOFilter")

ProductionFilterSequence = cms.Sequence(mcatnlofilter)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/MCatNLO_TTbar_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('MCatNLO ttbar  at 10TeV')
)
