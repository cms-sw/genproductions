import FWCore.ParameterSet.Config as cms

# link to cards

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/RunIII/13p6TeV/slc7_amd64_gcc10/Powheg/V2/VBF_H_NNPDF31_13p6TeV_M125_slc7_amd64_gcc10_CMSSW_12_4_11_patch3.tgz'),
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
'Alias      MyKst       K*0',
'Alias      MyantiKst   anti-K*0',
'#',
'Decay MyHiggs0',
'0.500      MyKst       gamma     HELAMP 1.0 0.0 1.0 0.0;',
'0.500      MyantiKst   gamma     HELAMP 1.0 0.0 1.0 0.0;',
'Enddecay',
'Decay MyKst',
'1.000      K+     pi-  VSS;',
'Enddecay',
'Decay MyantiKst',
'1.000      K-     pi+  VSS;',
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
            "POWHEG:nFinal = 3",
                        ),
        parameterSets = cms.vstring(
                            'pythia8PowhegEmissionVetoSettings',
                            'pythia8CommonSettings',
                            'pythia8CP5Settings',
                            'pythia8PSweightsSettings',
                            'processParameters',
 
        )
    )
)

generator.PythiaParameters.processParameters.extend(EvtGenExtraParticles)

ProductionFilterSequence = cms.Sequence(generator)



