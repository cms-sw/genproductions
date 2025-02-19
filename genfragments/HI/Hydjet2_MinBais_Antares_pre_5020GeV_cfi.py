import FWCore.ParameterSet.Config as cms
source = cms.Source("EmptySource")

from GeneratorInterface.Hydjet2Interface.hydjet2DefaultParameters_cff import *

generator = cms.EDFilter("Hydjet2GeneratorFilter",
        collisionParameters5020GeV,
        qgpParametersLHC,
        hydjet2Parameters,
        fNhsel  = cms.int32(2),
        PythiaParameters = cms.PSet(PythiaDefaultBlock,
                parameterSets = cms.vstring(
                        'ProQ2Otune',
                        'hydjet2PythiaDefault',
                )
        ),
        maxEventsToPrint = cms.untracked.int32(0),
        pythiaPylistVerbosity = cms.untracked.int32(0),
        fIfb    = cms.int32(1),
        fBmin   = cms.double(0.),
        fBmax   = cms.double(30.),
        fBfix   = cms.double(0.)
        #fPtmin = cms.double(9.06),
        #fUmax = cms.double(1.322),
        #fYlmax = cms.double(4.0),
)
generator.fPtmin = cms.double(9.06)  
generator.fUmax = cms.double(1.322)
generator.fYlmax = cms.double(4.0)
