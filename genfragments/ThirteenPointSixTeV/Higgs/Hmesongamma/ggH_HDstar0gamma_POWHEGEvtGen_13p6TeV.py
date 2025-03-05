import FWCore.ParameterSet.Config as cms

# link to cards

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/RunIII/13p6TeV/slc7_amd64_gcc10/Powheg/V2/gg_H_quark-mass-effects_NNPDF31_13p6TeV_M125_slc7_amd64_gcc10_CMSSW_12_4_11_patch3.tgz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *
from GeneratorInterface.EvtGenInterface.EvtGenSetting_cff import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         comEnergy = cms.double(13600.0),
                         crossSection = cms.untracked.double(1),
                         filterEfficiency = cms.untracked.double(1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         maxEventsToPrint = cms.untracked.int32(5),
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         ExternalDecays = cms.PSet(
        EvtGen130 = cms.untracked.PSet(
            decay_table = cms.string('GeneratorInterface/EvtGenInterface/data/DECAY_2020_NOLONGLIFE.DEC'),
            particle_property_file = cms.FileInPath('GeneratorInterface/EvtGenInterface/data/evt_2020.pdl'),
            user_decay_embedded= cms.vstring(
'#',
'Alias      MyHiggs0    Higgs0',
'Alias      MyDst       D*0',
'Alias      MyantiDst   anti-D*0',
'Alias      MyDz        D0',
'Alias      MyantiDz    anti-D0',
'#',
'Decay MyHiggs0',
'0.500      MyDst       gamma     HELAMP 1.0 0.0 1.0 0.0;',
'0.500      MyantiDst   gamma     HELAMP 1.0 0.0 1.0 0.0;',
'Enddecay',
'Decay MyDst',
'0.619   MyDz     pi0     VSS;',
'0.381   MyDz     gamma    VSP_PWAVE;',
'Enddecay',
'Decay MyantiDst',
'0.619   MyantiDz     pi0     VSS;',
'0.381   MyantiDz     gamma    VSP_PWAVE;',
'Enddecay',
'Decay MyDz',
'1.000    K-      pi+      PHSP;',
'Enddecay',
'Decay MyantiDz',
'1.000    K+      pi-      PHSP;',
'Enddecay',
'#',
'End'),
            list_forced_decays = cms.vstring('MyHiggs0'),
            operates_on_particles = cms.vint32(25),
            convertPythiaCodes = cms.untracked.bool(False)
            ),
        parameterSets = cms.vstring('EvtGen130')
        ),
        PythiaParameters = cms.PSet(
         pythia8CommonSettingsBlock,
         pythia8CP5SettingsBlock,
         pythia8PSweightsSettingsBlock,   
         pythia8PowhegEmissionVetoSettingsBlock,     
         processParameters = cms.vstring(
            "POWHEG:nFinal = 1",
                        ),
        parameterSets = cms.vstring(
                            'pythia8PowhegEmissionVetoSettings',
                            'pythia8CommonSettings',
                            'pythia8PSweightsSettings',
                            'pythia8CP5Settings',
                            'processParameters',
 
        )
    )
)

generator.PythiaParameters.processParameters.extend(EvtGenExtraParticles)

ProductionFilterSequence = cms.Sequence(generator)



