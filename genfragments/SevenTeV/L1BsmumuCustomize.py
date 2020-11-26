import FWCore.ParameterSet.Config as cms

def customiseL1(process):
        process.GlobalTag.toGet.append(
            cms.PSet(
                record  = cms.string( 'L1MuGMTParametersRcd' ),
                tag     = cms.string( 'L1MuGMTParameters_synctf_10_mc' ),
                label   = cms.untracked.string( '' ),
                connect = cms.untracked.string('frontier://FrontierProd/CMS_COND_31X_L1T')
            ))
        process.GlobalTag.toGet.append(
            cms.PSet(
                record  = cms.string( 'L1MuDTTFParametersRcd' ),
                tag     = cms.string( 'L1MuDTTFParameters_dttf11_TSC_09_17_col_mc' ),
                label   = cms.untracked.string( '' ),
                connect = cms.untracked.string('frontier://FrontierProd/CMS_COND_31X_L1T')
                ))
        process.GlobalTag.toGet.append(
            cms.PSet(
                record  = cms.string( 'L1MuCSCTFConfigurationRcd' ),
                tag     = cms.string( 'L1MuCSCTFConfiguration_90511_mc' ),
                label   = cms.untracked.string( '' ),
                connect = cms.untracked.string('frontier://FrontierProd/CMS_COND_31X_L1T')
                ))
        process.GlobalTag.toGet.append(
            cms.PSet(
                record  = cms.string( 'L1RPCBxOrConfigRcd' ),
                tag     = cms.string( 'L1RPCBxOrConfig_LHC7_1EX_mc' ),
                label   = cms.untracked.string( '' ),
                connect = cms.untracked.string('frontier://FrontierProd/CMS_COND_31X_L1T')
                ))
        process.GlobalTag.toGet.append(
            cms.PSet(
                record  = cms.string( 'L1RPCConeDefinitionRcd' ),
                tag     = cms.string( 'L1RPCConeDefinition_LHC7_1EX_mc' ),
                label   = cms.untracked.string( '' ),
                connect = cms.untracked.string('frontier://FrontierProd/CMS_COND_31X_L1T')
                ))
        process.GlobalTag.toGet.append(
            cms.PSet(
                record  = cms.string( 'L1RPCConfigRcd' ),
                tag     = cms.string( 'L1RPCConfig_LHC7_1EX_mc' ),
                label   = cms.untracked.string( '' ),
                connect = cms.untracked.string('frontier://FrontierProd/CMS_COND_31X_L1T')
                ))
        process.GlobalTag.toGet.append(
            cms.PSet(
                record  = cms.string( 'L1RPCHsbConfigRcd' ),
                tag     = cms.string( 'L1RPCHsbConfig_LHC7_1EX_mc' ),
                label   = cms.untracked.string( '' ),
                connect = cms.untracked.string('frontier://FrontierProd/CMS_COND_31X_L1T')
                )
            )
        return(process)
