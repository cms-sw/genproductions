import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

generator = cms.EDFilter("Pythia8GeneratorFilter",
    crossSection = cms.untracked.double(8.32e+08),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
	    'Main:timesAllowErrors    = 10000', 
#            'ParticleDecays:limitTau0 = on',
#	    'ParticleDecays:tauMax = 10',
            'HardQCD:all = on',
            'PhaseSpace:pTHatMin = 15.',
            'Tune:pp 2',                      
            'Tune:ee 3',
            'ParticleDecays:limitTau0 = off',
            'ParticleDecays:limitCylinder = on',
            'ParticleDecays:xyMax = 2000',
            'ParticleDecays:zMax = 4000',
            '130:mayDecay = on',
            '211:mayDecay = on',
            '321:mayDecay = on'),
        parameterSets = cms.vstring('processParameters')
    )
)

mugenfilter = cms.EDFilter("MCSmartSingleParticleFilter",
                           MinPt = cms.untracked.vdouble(5.,5.),
                           MinEta = cms.untracked.vdouble(-2.5,-2.5),
                           MaxEta = cms.untracked.vdouble(2.5,2.5),
                           ParticleID = cms.untracked.vint32(13,-13),
                           Status = cms.untracked.vint32(1,1),
                           # Decay cuts are in mm
                           MaxDecayRadius = cms.untracked.vdouble(2000.,2000.),
                           MinDecayZ = cms.untracked.vdouble(-4000.,-4000.),
                           MaxDecayZ = cms.untracked.vdouble(4000.,4000.)
)


ProductionFilterSequence = cms.Sequence(generator*mugenfilter)
