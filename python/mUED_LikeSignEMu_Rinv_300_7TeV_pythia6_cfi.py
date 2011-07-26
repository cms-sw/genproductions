import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

# Disclaimer :
#
# This set of Pythia configuration cards has been copied 
# and pasted from 
#   RecoTracker/RoadSearchCloudMaker/test/sim_pythia.cfg
# prepared by Oliver Gutsche (FNAL)
#
# the only change is MSTP(128)=2 - this option takes decay 
# products out of doc section; decay products point at parents
# in the main section (Julia Yarba, FNAL).
generator = cms.EDFilter("Pythia6GeneratorFilter",
    # to printout HepMC::GenEvent record (HepMC::GenEvent::print())
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(1),
    # to printout pythia event record (call pylist)
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUED = cms.vstring('MSEL=0',
           'IUED(1)=1',
           'IUED(2)=0',
           'IUED(3)=5',
           'IUED(4)=6',
           'IUED(5)=0',
           'IUED(6)=1',
           'RUED(1)=300.',
           'RUED(2)=5000.',
           'RUED(3)=10000.0',
#           'RUED(4)=9.09090909090909',
           'MSUB(311)=1',
           'MSUB(312)=1',
           'MSUB(313)=1',
           'MSUB(314)=1',
           'MSUB(315)=1',
           'MSUB(316)=1',
           'MSUB(317)=1',
           'MSUB(318)=1',
           'MSUB(319)=1'
),
        parameterSets = cms.vstring('pythiaUED')
    )
)

filter = cms.EDFilter("UEDMultiLeptonFilter")
filter.UseFilter = cms.untracked.int32(1)
#filter.SSDiMuFilter = cms.untracked.int32(1)
filter.SSDiEMuFilter = cms.untracked.int32(1)

ProductionFilterSequence = cms.Sequence(generator*filter)
#ProductionFilterSequence = cms.Sequence(generator)
