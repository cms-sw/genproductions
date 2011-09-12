## Wprime to WZ, Z2 tune, 310x production


import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2Settings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0', 
                                        "MSUB(142)=1 !qq->Wprime", 
                                        'PMAS(34,1)=900.0  !Wprime Mass', 
                                        'MDME(311,1)=    0   !Wprime to d          ubar', 
                                        'MDME(312,1)=    0   !Wprime to d          cbar', 
                                        'MDME(313,1)=    0   !Wprime to d          tbar', 
                                        'MDME(315,1)=    0   !Wprime to s          ubar', 
                                        'MDME(316,1)=    0   !Wprime to s          cbar', 
                                        'MDME(317,1)=    0   !Wprime to s          tbar', 
                                        'MDME(319,1)=    0   !Wprime to b          ubar', 
                                        'MDME(320,1)=    0   !Wprime to b          cbar', 
                                        'MDME(321,1)=    0   !Wprime to b          tbar', 
                                        'MDME(327,1)=    0   !Wprime to e          nu_e', 
                                        'MDME(328,1)=    0   !Wprime to mu         nu_mu', 
                                        'MDME(329,1)=    0   !Wprime to tau        nu_tau', 
                                        'MDME(331,1)=    1   !Wprime to W          Z', 
                                        'MDME(332,1)=    0   !Wprime to W          gamma', 
                                        'MDME(333,1)=    0   !Wprime to W          h0', 
                                        'MDME(190,1)=    1   !W to d               ubar', 
                                        'MDME(191,1)=    1   !W to d               cbar', 
                                        'MDME(192,1)=    1   !W to d               tbar', 
                                        'MDME(194,1)=    1   !W to s               ubar', 
                                        'MDME(195,1)=    1   !W to s               cbar', 
                                        'MDME(196,1)=    1   !W to s               tbar', 
                                        'MDME(198,1)=    1   !W to b               ubar', 
                                        'MDME(199,1)=    1   !W to b               cbar', 
                                        'MDME(200,1)=    1   !W to b               tbar', 
                                        'MDME(206,1)=    0   !W to e               nu_e', 
                                        'MDME(207,1)=    0   !W to mu              nu_mu', 
                                        'MDME(208,1)=    0   !W to tau             nu_tau', 
                                        'MDME(174,1)=    0   !Z to d               dbar', 
                                        'MDME(175,1)=    0   !Z to u               ubar', 
                                        'MDME(176,1)=    0   !Z to s               sbar', 
                                        'MDME(177,1)=    0   !Z to c               cbar', 
                                        'MDME(178,1)=    0   !Z to b               bbar', 
                                        'MDME(179,1)=    0   !Z to t               tbar', 
                                        'MDME(182,1)=    1   !Z to e-              e+', 
                                        'MDME(183,1)=    0   !Z to nu_e            nu_ebar', 
                                        'MDME(184,1)=    1   !Z to mu-             mu+', 
                                        'MDME(185,1)=    0   !Z to nu_mu           nu_mubar', 
                                        'MDME(186,1)=    0   !Z to tau-            tau+', 
                                        'MDME(187,1)=    0   !Z to nu_tau          nu_taubar'), 
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)




ProductionFilterSequence = cms.Sequence(generator)
