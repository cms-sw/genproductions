import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUED6TSettings_cfi import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0          ! User defined processes',
                                        'PMAS(5,1)=4.4   ! b quark mass',
                                        'PMAS(6,1)=172.4 ! t quark mass',
                                        'MSTJ(41)=3      ! QED fsr off leptons is off',
                                        'MSTP(61)=1      ! only QDC branchings in ISR'
                                        ),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings',
                                    'processParameters'
                                    )
    ),
    jetMatching = cms.untracked.PSet(
       scheme = cms.string("Madgraph"),
       mode = cms.string("auto"),       # soup, or "inclusive" / "exclusive"
       MEMAIN_etaclmax = cms.double(-1),
       MEMAIN_qcut = cms.double(-1),
       MEMAIN_minjets = cms.int32(-1),
       MEMAIN_maxjets = cms.int32(-1),
       MEMAIN_showerkt = cms.double(0),
       MEMAIN_excres = cms.string(""),
       outTree_flag = cms.int32(0)
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string ('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/Attic/PYTHIA6_EWK_MadgraphVgamma_cff.py,v $'),
    annotation = cms.untracked.string('runs Pythia6 on Madgraph MC output, without QED radiation for Vgamma')
)

