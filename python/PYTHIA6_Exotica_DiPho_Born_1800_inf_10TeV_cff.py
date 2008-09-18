import FWCore.ParameterSet.Config as cms


from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(0.001454),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0', 
            'MSUB(18)=1               ! qq -> gamma gamma ', 
            'CKIN(1)=1800.          ! minimum pt hat for hard interactions', 
            'CKIN(2)=-1          ! maximum pt hat for hard interactions'),
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    annotation = cms.untracked.string('qq -> gamma gamma (1800-inf GeV)  at sqrt{s} = 10 TeV'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_Exotica_DiPho_Born_1800_inf_10TeV_cff_py_GEN_IDEAL.py,v $')

)

