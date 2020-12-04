import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
       comEnergy = cms.double(8000.0),
       crossSection = cms.untracked.double(1.242e+09),
       filterEfficiency = cms.untracked.double(1),
       maxEventsToPrint = cms.untracked.int32(0),
       pythiaHepMCVerbosity = cms.untracked.bool(False),
       pythiaPylistVerbosity = cms.untracked.int32(0),
       #reweightGen = cms.PSet(),       # flat in Pt
       #reweightGenRap = cms.PSet(      # flat in eta
          #yLabSigmaFunc = cms.string("15.44/pow(x,0.0253)-12.56"),
          #yLabPower = cms.double(2.),
          #yCMSigmaFunc = cms.string("5.45/pow(x+64.84,0.34)"),
          #yCMPower = cms.double(2.),
          #pTHatMin = cms.double(15.),
          #pTHatMax = cms.double(3000.)
       #),
       reweightGenPtHatRap = cms.PSet( # flat in Pt and eta
          yLabSigmaFunc = cms.string("15.44/pow(x,0.0253)-12.56"),
          yLabPower = cms.double(2.),
          yCMSigmaFunc = cms.string("5.45/pow(x+64.84,0.34)"),
          yCMPower = cms.double(2.),
          pTHatMin = cms.double(15.),
          pTHatMax = cms.double(3000.)
       ),

       PythiaParameters = cms.PSet(
              processParameters = cms.vstring(
                     'Main:timesAllowErrors = 10000',
                     'ParticleDecays:limitTau0 = on',
                     'ParticleDecays:tauMax = 10',
                     'HardQCD:all = on',
                     'PhaseSpace:pTHatMin = 15',
                     'PhaseSpace:pTHatMax = 500',
                     'Tune:pp 5',
                     'Tune:ee 3'
              ),
              parameterSets = cms.vstring('processParameters')
       )
)

configurationMetadata = cms.untracked.PSet(
       version = cms.untracked.string('\$Revision$'),
       name = cms.untracked.string('\$Source$'),
       annotation = cms.untracked.string('Upgrade TP sample with PYTHIA8: QCD dijet production, pThat = 15 .. 500 GeV, weighted, Tune4C')
)
