import FWCore.ParameterSet.Config as cms

source = cms.Source("Pythia8Source",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.49),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(180000.),
    PythiaParameters = cms.PSet(
    processParameters = cms.vstring('Main:timesAllowErrors    = 10000',
                                    'ParticleDecays:limitTau0 = on',              # Decay those unstable particles
                                    'ParticleDecays:tau0Max   = 10.',             # for which _nominal_ proper lifetime < 10 mm
                                    'PromptPhoton:all         = on',
                                    'PhaseSpace:pTHatMin      = 15.',
                                    'PhaseSpace:pTHatMax      = 20.'),
    # This is a vector of ParameterSet names to be read, in this order
    parameterSets = cms.vstring('processParameters')
    )
                    )

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/PYTHIA8_PhotonJetpt15_20_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA8 Photon + Jet for 15 < pT < 20 at 10TeV')
    )

photonfilter = cms.EDFilter("MCSingleParticleFilter",
                          MaxEta = cms.untracked.vdouble(2.4),
                          MinEta = cms.untracked.vdouble(-2.4),
                          MinPt = cms.untracked.vdouble(15.0),
                          ParticleID = cms.untracked.vint32(22)
                          )
ProductionFilterSequence = cms.Sequence( photonfilter )
