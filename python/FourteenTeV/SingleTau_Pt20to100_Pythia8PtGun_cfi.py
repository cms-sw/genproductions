import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8PtGun",
    PGunParameters = cms.PSet(
        ParticleID = cms.vint32(15),
        AddAntiParticle = cms.bool(False),
        MinPhi = cms.double(-3.14159265359),
        MaxPhi = cms.double(3.14159265359),
        MinPt = cms.double(20.0),
        MaxPt = cms.double(100.0),
        MinEta = cms.double(-3.0),
        MaxEta = cms.double(3.0)
    ),
    PythiaParameters = cms.PSet(
        py8TauDecaySettings = cms.vstring('ParticleDecays:sophisticatedTau = 0', # isotropic decays
            '15:0:onMode = 1',   # bRatio=  0.1076825          products=     16     -211
            '15:1:onMode = 0',   # bRatio=  0.0069601          products=     16     -321
            '15:2:onMode = 0',   # bRatio=  0.1772832          products=     16       11      -12
            '15:3:onMode = 0',   # bRatio=  0.1731072          products=     16       13      -14
            '15:4:onMode = 1',   # bRatio=  0.2537447          products=     16      111     -211
            '15:5:onMode = 0',   # bRatio=  0.0015809          products=     16      311     -321
            '15:6:onMode = 0',   # bRatio=  0.0001511          products=     16      221     -321
            '15:7:onMode = 0',   # bRatio=  0.0083521          products=     16     -211     -311
            '15:8:onMode = 0',   # bRatio=  0.0042655          products=     16      111     -321
            '15:9:onMode = 0',   # bRatio=  0.0924697          products=     16      111      111     -211
            '15:10:onMode = 0',  # bRatio=  0.0925691          products=     16     -211     -211      211
            '15:11:onMode = 0',  # bRatio=  0.0039772          products=     16      111     -211     -311
            '15:12:onMode = 0',  # bRatio=  0.0034701          products=     16     -211      211     -321
            '15:13:onMode = 0',  # bRatio=  0.0014318          products=     16     -211     -321      321
            '15:14:onMode = 0',  # bRatio=  0.0015809          products=     16      111      311     -321
            '15:15:onMode = 0',  # bRatio=  0.0011932          products=     16      130     -211      310
            '15:16:onMode = 0',  # bRatio=  0.0006463          products=     16      111      111     -321
            '15:17:onMode = 0',  # bRatio=  0.0002386          products=     16      130      130     -211
            '15:18:onMode = 0',  # bRatio=  0.0002386          products=     16     -211      310      310
            '15:19:onMode = 0',  # bRatio=  0.0013821          products=     16      111     -211      221
            '15:20:onMode = 0',  # bRatio=  0.0017520          products=     16       22      111     -211
            '15:21:onMode = 0',  # bRatio=  0.0459365          products=     16      111     -211     -211      211
            '15:22:onMode = 0',  # bRatio=  0.0104401          products=     16      111      111      111     -211
            '15:23:onMode = 0',  # bRatio=  0.0049069          products=     16      111      111     -211     -211      211
            '15:24:onMode = 0',  # bRatio=  0.0009515          products=     16      111      111      111      111     -211
            '15:25:onMode = 0',  # bRatio=  0.0008342          products=     16     -211     -211     -211      211      211
            '15:26:onMode = 0',  # bRatio=  0.0001631          products=     16     -211     -211      211      221
            '15:27:onMode = 0',  # bRatio=  0.0001491          products=     16      111      111     -211      221
            '15:28:onMode = 0',  # bRatio=  0.0001392          products=     16      111      111     -211      223
            '15:29:onMode = 0',  # bRatio=  0.0001193          products=     16     -211     -211      211      223
            '15:30:onMode = 0',  # bRatio=  0.0004077          products=     16      223     -321
            '15:31:onMode = 0',  # bRatio=  0.0004773          products=     16      111      111      111     -321
            '15:32:onMode = 0',  # bRatio=  0.0003052          products=     16      111     -211      211     -321
            '15:33:onMode = 0',  # bRatio=  0.0002784          products=     16      221     -323
            '15:34:onMode = 0',  # bRatio=  0.0002366          products=     16      111      111     -211     -311
            '15:35:onMode = 0',  # bRatio=  0.0002237          products=     16     -211     -211      211     -311
            '15:36:onMode = 0',  # bRatio=  0.0002953          products=     16      111     -211     -311      311
            '15:37:onMode = 0',  # bRatio=  0.0000590          products=     16      111     -211     -321      321
        ),
        parameterSets = cms.vstring('py8TauDecaySettings')
    )
)
