import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL        = 0    !User defined processes', 
                                        'PMAS(5,1)=4.4   ! b quark mass',
                                        'PMAS(6,1)=172.4 ! t quark mass',
                                        'MSTJ(1)=1       ! Fragmentation/hadronization on or off',
                                        'MSTP(61)=1      ! Parton showering on or off'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    ),

    jetMatching = cms.untracked.PSet(
       scheme = cms.string("Madgraph"),
       mode = cms.string("auto"),       # soup, or "inclusive" / "exclusive"
       MEMAIN_etaclmax = cms.double(5),
       MEMAIN_qcut = cms.double(30),
       MEMAIN_minjets = cms.int32(0),
       MEMAIN_maxjets = cms.int32(15),
       MEMAIN_showerkt = cms.double(0),
       MEMAIN_nqmatch = cms.int32(5),
       MEMAIN_excres = cms.string(""),
       outTree_flag = cms.int32(0)
    )
)

ProductionFilterSequence = cms.Sequence(generator)
