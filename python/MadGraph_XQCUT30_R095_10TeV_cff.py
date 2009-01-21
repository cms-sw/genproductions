import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("MadGraphSource",
                    ## DEFAULT SETTINGS
                    produceEventTreeFile = cms.untracked.bool(False),
                    pythiaPylistVerbosity = cms.untracked.int32(0),
                    firstEvent = cms.untracked.uint32(0),
                    maxEventsToPrint = cms.untracked.int32(5),
                    getInputFromMCDB = cms.untracked.bool(False),
                    
                    # ME-PS matching
                    # turn to standard sources way of inputting filename
                    fileNames = cms.untracked.vstring('file:events.lhe'),
                    # matching scale cut ~1.5xqcut in run_card.dat
                    MEMAIN_qcut = cms.untracked.double(30),
                    pythiaHepMCVerbosity = cms.untracked.bool(False),
                    # values for the MEMAIN routine (matching). if set to 0. default values will be chosen from the interface
                    MEMAIN_etaclmax = cms.untracked.double(5.0),
                    # for reading non-MG LHE files
                    minimalLH = cms.untracked.bool(False),
                    # set to 1 only if need to perform exclusive matching
                    MEMAIN_iexcfile = cms.untracked.uint32(0), 
                    MCDBArticleID = cms.int32(0),
                    
                    # PYTHIA
                    PythiaParameters = cms.PSet( pythiaUESettingsBlock,
                                                 pythiaCMSDefaults = cms.vstring('PMAS(5,1)=4.4    ! b quarks mass',
                                                                                 'PMAS(6,1)=172.4  ! t quarks mass',
                                                                                 'MSTJ(1)=1        !...Fragmentation/hadronization on or off',
                                                                                 'MSTP(61)=1       ! Parton showering on or off',
                                                                                 'MSTP(143)=1      ! MUST BE 1 FOR THE MATCHING ROUTINE TO RUN!!!!',
                                                                                 'MSEL=0           ! User defined processes/Full user control'),
                                                 TopDecaySteer = cms.vstring('VCKM(3,1) = 0.05 ! V_td',
                                                                             'VCKM(3,2) = 0.22 ! V_ts',
                                                                             'VCKM(3,3) = 0.97 ! V_tb'
                                                                             ),
                                                 # This is a vector of ParameterSet names to be read, in this order
                                                 # The first is general default pythia parameters, the second are own additions
                                                 parameterSets = cms.vstring('pythiaUESettings',
                                                                             'pythiaCMSDefaults',
                                                                             'TopDecaySteer')
                                                 )
                    )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string(''),
    name = cms.untracked.string(''),
    annotation = cms.untracked.string('MadGraph-PYTHIA6 at 10TeV, qcut 30, cteq6l, R=0.95')
    )
