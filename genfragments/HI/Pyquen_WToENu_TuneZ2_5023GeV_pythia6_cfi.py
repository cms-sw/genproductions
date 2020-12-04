import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("PyquenGeneratorFilter",
                         comEnergy = cms.double(5023.0),
                         aBeamTarget = cms.double(208.0),
                         protonSide = cms.untracked.int32(2),
                         qgpInitialTemperature = cms.double(1.0), ## initial temperature of QGP; allowed range [0.2,2.0]GeV;
                         qgpProperTimeFormation = cms.double(0.1), ## proper time of QGP formation; allowed range [0.01,10.0]fm/c;
                         hadronFreezoutTemperature = cms.double(0.14),
                         doRadiativeEnLoss = cms.bool(True), ## if true, perform partonic radiative en loss
                         doCollisionalEnLoss = cms.bool(False),
                         qgpNumQuarkFlavor = cms.int32(0),  ## number of active quark flavors in qgp; allowed values: 0,1,2,3
                         numQuarkFlavor = cms.int32(0), ## to be removed
                         doIsospin = cms.bool(True),
                         angularSpectrumSelector = cms.int32(0), ## angular emitted gluon spectrum :
                         embeddingMode = cms.bool(False),
                         backgroundLabel = cms.InputTag("generator"), ## ineffective in no mixing
                         doQuench = cms.bool(False),
                         bFixed = cms.double(0.0), ## fixed impact param (fm); valid only if cflag_=0
                         cFlag = cms.int32(0), ## centrality flag
                         bMin = cms.double(0.0), ## min impact param (fm); valid only if cflag_!=0
                         bMax = cms.double(0.0), ## max impact param (fm); valid only if cflag_!=0
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         pythiaHepMCVerbosity = cms.untracked.bool(True),
                         maxEventsToPrint = cms.untracked.int32(0),
                         PythiaParameters = cms.PSet(pythiaUESettingsBlock,
                                                     processParameters = cms.vstring('MSEL        = 0    !User defined processes',
                                                                                     'MSUB(2)     = 1    !W production',
                                                                                     'MDME(190,1) = 0    !W decay into dbar u',
                                                                                     'MDME(191,1) = 0    !W decay into dbar c',
                                                                                     'MDME(192,1) = 0    !W decay into dbar t',
                                                                                     'MDME(194,1) = 0    !W decay into sbar u',
                                                                                     'MDME(195,1) = 0    !W decay into sbar c',
                                                                                     'MDME(196,1) = 0    !W decay into sbar t',
                                                                                     'MDME(198,1) = 0    !W decay into bbar u',
                                                                                     'MDME(199,1) = 0    !W decay into bbar c',
                                                                                     'MDME(200,1) = 0    !W decay into bbar t',
                                                                                     'MDME(205,1) = 0    !W decay into bbar tp',
                                                                                     'MDME(206,1) = 1    !W decay into e+ nu_e',
                                                                                     'MDME(207,1) = 0    !W decay into mu+ nu_mu',
                                                                                     'MDME(208,1) = 0    !W decay into tau+ nu_tau'),
                                                     parameterSets = cms.vstring('pythiaUESettings',
                                                                                 'processParameters')
                                                     )
                         )


configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/reps/CMSSW/CMSSW/Configuration/GenProduction/python/HI/Pyquen_WToENu_TuneZ2_5023GeV_pythia6_cfi.py,v $'),
    annotation = cms.untracked.string('PYQUEN-Wenu Tune Z2 at 5.023 TeV')
)

#ProductionFilterSequence = cms.Sequence(hiSignal)
ProductionFilterSequence = cms.Sequence(generator)
