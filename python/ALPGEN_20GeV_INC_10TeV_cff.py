import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUESettings_cfi import pythiaUESettingsBlock
pythiaUESettings = pythiaUESettingsBlock.pythiaUESettings
forPythia = cms.vstring("MSEL=0         ! (D=1) ",
                        "MSTJ(11)=3     ! Choice of the fragmentation function",
                        "MSTP(143)=1    ! Call the matching routine in ALPGEN"
                        )
forPythia += pythiaUESettings

paramsForPythia = cms.PSet(
    parameterSets = cms.vstring("pythia"),
    pythia = forPythia
    )

source = cms.Source("AlpgenSource",
                    PythiaParameters = paramsForPythia,
                    fileNames = cms.untracked.vstring('file:alpgen'),
                    pythiaPylistVerbosity = cms.untracked.int32(0),
                    pythiaHepMCVerbosity = cms.untracked.bool(False),
                    GeneratorParameters = cms.PSet(parameterSets = cms.vstring("generator"),
                                                   generator = cms.vstring("IXpar(2) = 0   ! inclus./exclus. sample: 0/1",
                                                                           #Inputs for clustering: minET(CLUS), deltaR(CLUS)
                                                                           "RXpar(1) = 20. ! ETCLUS : minET(CLUS)",
                                                                           "RXpar(2) = 0.7 ! RCLUS  : deltaR(CLUS)"
                                                                           )
                                                   )
                    )
