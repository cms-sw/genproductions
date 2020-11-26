import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEP11Settings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia6HadronizerFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(True),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    comEnergy = cms.double(7000.0),
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
                        'PMAS(5,1)=4.8   ! b quark mass',
                        'PMAS(6,1)=172.5 ! t quark mass',
			'MSTJ(1)=1       ! Fragmentation/hadronization on or off',
			'MSTP(61)=1      ! Parton showering on or off'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    ),
    jetMatching = cms.untracked.PSet(
       scheme = cms.string("Madgraph"),
       mode = cms.string("auto"),	# soup, or "inclusive" / "exclusive"
       MEMAIN_nqmatch = cms.int32(5),
       MEMAIN_etaclmax = cms.double(-1),
       MEMAIN_qcut = cms.double(-1),
       MEMAIN_minjets = cms.int32(-1),
       MEMAIN_maxjets = cms.int32(-1),
       MEMAIN_showerkt = cms.double(0),   
       MEMAIN_excres = cms.string(""),
       outTree_flag = cms.int32(0)    
    )    
)

# Parameters for the Perugia2011 noCR variant
generator.PythiaParameters.pythiaUESettings[24] =  'PARP(82) = 3.0500 ! Perugia2011 noCR: UE IR cutoff at reference ecm'
generator.PythiaParameters.pythiaUESettings[35] =  'PARP(95) =      0 ! Perugia2011 noCR: FSI color (re-)connection model'
generator.PythiaParameters.pythiaUESettings[36] =  'PARP(78) = 0.0000 ! Perugia2011 noCR: FSI color reconnection strength'
generator.PythiaParameters.pythiaUESettings[37] =  'PARP(77) = 0.0000 ! Perugia2011 noCR: FSI color reco high-pT damping strength'
