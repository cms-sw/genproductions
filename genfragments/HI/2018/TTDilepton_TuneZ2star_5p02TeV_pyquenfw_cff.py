import FWCore.ParameterSet.Config as cms

from GeneratorInterface.PyquenInterface.pyquenPythiaDefault_cff import *
from Configuration.Generator.PythiaUEZ2starSettings_cfi import * 

generator = cms.EDFilter("PyquenGeneratorFilter",
      PythiaParameters = cms.PSet(
         pythiaUESettingsBlock,
         processParameters = cms.vstring('MSEL      = 0      !User defined processes', 
            'MSUB(81)  = 1      ! qqbar to QQbar', 
            'MSUB(82)  = 1      ! gg to QQbar', 
            'MSTP(7)   = 6      ! flavour = top', 
            'PMAS(6,1) = 172.5  ! top quark mass', 
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
            'MDME(207,1) = 1    !W decay into mu+ nu_mu', 
            'MDME(208,1) = 1    !W decay into tau+ nu_tau'),

         parameterSets = cms.vstring('pythiaUESettings', 
                                      'processParameters'),
        ),
        aBeamTarget = cms.double(208.0), 
        comEnergy = cms.double(5020.0),

        qgpInitialTemperature = cms.double(1.0), 
        qgpProperTimeFormation = cms.double(0.1), 
        hadronFreezoutTemperature = cms.double(0.14),
        qgpNumQuarkFlavor = cms.int32(0),  
        numQuarkFlavor = cms.int32(0),

        doQuench = cms.bool(True),
        doRadiativeEnLoss = cms.bool(True), 
        doCollisionalEnLoss = cms.bool(True),
        angularSpectrumSelector = cms.int32(1), 

        doIsospin = cms.bool(True),

       embeddingMode = cms.bool(False),
       backgroundLabel = cms.InputTag("generator"),
                  
       bFixed = cms.double(0.0),
       bMax = cms.double(0.0),
       bMin = cms.double(0.0),
       cFlag = cms.int32(0),
      )

ProductionFilterSequence = cms.Sequence(generator)
