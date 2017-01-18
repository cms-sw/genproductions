import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *
#from GeneratorInterface.GenFilters.Zto2lFilter_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(14000.0),
    crossSection = cms.untracked.double(1.0),
    ExternalDecays = cms.PSet(
        Tauola = cms.untracked.PSet(
            TauolaPolar,
            TauolaDefaultInputCards
        ),
        parameterSets = cms.vstring('Tauola')
    ),
    UseExternalGenerators = cms.untracked.bool(True),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL=0         ! User defined processes',
            'MSUB(81)  = 1     ! qqbar to QQbar',
            'MSUB(82)  = 1     ! gg to QQbar',
            'MSTP(7)   = 6     ! flavor = top',
            'PMAS(6,1) = 172.5  ! top quark mass'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

#TTbarFilter = cms.EDFilter("PythiaFilterTTBar",
#                                decayType = cms.untracked.int32(2),
#                                leptonFlavour = cms.untracked.int32(0)
#                              )

DileptonFilter = cms.EDFilter("Zto2lFilter",
                                MaxEtaLepton = cms.untracked.double(6.),
                                MindiLeptonInvariantMass = cms.untracked.double(75.)
                              )

#ProductionFilterSequence = cms.Sequence(generator*TTbarFilter*DileptonFilter)
ProductionFilterSequence = cms.Sequence(generator*DileptonFilter)


