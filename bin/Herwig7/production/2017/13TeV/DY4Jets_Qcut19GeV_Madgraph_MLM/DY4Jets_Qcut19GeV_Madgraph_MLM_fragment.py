import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Herwig7Settings.Herwig7CH2TuneSettings_cfi import herwig7CH2SettingsBlock


externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/cvmfs/cms.cern.ch/phys_generator/gridpacks/2017/13TeV/madgraph/V5_2.4.2/DYJets_HT_mll50/DYJets_HT-incl/DYJets_HT-incl_slc6_amd64_gcc481_CMSSW_7_1_30_tarball_v1.tar.xz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

#Link to madgraph datacards:
#https://github.com/cms-sw/genproductions/tree/1e06dc8f0c86ab6fdb547b27c43c92b022608b80/bin/MadGraph5_aMCatNLO/cards/production/2017/13TeV/DYJets_HT_LO_MLM/DYJets_HT_mll50/DYJets_HT-incl

generator = cms.EDFilter("Herwig7GeneratorFilter",
    herwig7CH2SettingsBlock,
    hw_LHEsettings = cms.vstring(
                #First load settings for PP collider
                'read snippets/PPCollider.in',
                #############################################################
                # Create a LH event handler (set up & assigned below) ...   #
                #############################################################
                'cd /Herwig/EventHandlers',
                'library FxFx.so',
                'create Herwig::FxFxEventHandler theLesHouchesHandler',

                #############################################################
                # Create a LH reader (set up & assigned below) ...          #
                #############################################################
                'cd /Herwig/EventHandlers',
                'library FxFx.so',
                'create Herwig::FxFxFileReader theLHReader',

                #############################################################
                # Create an FxFxHandler (set up & assigned below) ...     #
                #############################################################
                'cd /Herwig/Shower',
                'library FxFxHandler.so',
                'create Herwig::FxFxHandler FxFxHandler',
                'set /Herwig/Shower/FxFxHandler:ShowerModel /Herwig/Shower/ShowerModel',
                'set /Herwig/Shower/FxFxHandler:SplittingGenerator /Herwig/Shower/SplittingGenerator',

                ############################################################
                # Create a cuts object ...                                 #
                ############################################################
                'cd /Herwig/EventHandlers',
                'create ThePEG::Cuts   /Herwig/Cuts/NoCuts',

                #############################################################
                # Setup the LH event handler ...                            #
                #############################################################
                'cd /Herwig/EventHandlers',
                'insert theLesHouchesHandler:FxFxReaders[0] theLHReader',
                'set theLesHouchesHandler:WeightOption VarNegWeight',
                'set theLesHouchesHandler:PartonExtractor /Herwig/Partons/PPExtractor',
                'set theLesHouchesHandler:CascadeHandler /Herwig/Shower/FxFxHandler',
                'set theLesHouchesHandler:HadronizationHandler /Herwig/Hadronization/ClusterHadHandler',
                'set theLesHouchesHandler:DecayHandler /Herwig/Decays/DecayHandler',
                ############################################################

                ##################################################
                #  Shower parameters
                ##################################################
                # normally, you want
                # the scale supplied in the event files (SCALUP)
                # to be used as a pT veto scale in the parton shower
                'set /Herwig/Shower/ShowerHandler:MaxPtIsMuF Yes',
                'set /Herwig/Shower/ShowerHandler:RestrictPhasespace Yes',
                # Shower parameters
                # treatment of wide angle radiation
                'set /Herwig/Shower/PartnerFinder:PartnerMethod Random',
                'set /Herwig/Shower/PartnerFinder:ScaleChoice Partner',

                #############################################################
                # Set up the LH reader ...                                  #
                #############################################################
                'cd /Herwig/EventHandlers',
                'set theLHReader:WeightWarnings    false',
                # Input event file name:
                'set theLHReader:FileName cmsgrid_final.lhe',
                'set theLHReader:MomentumTreatment      RescaleEnergy',
                'set theLHReader:Cuts  /Herwig/Cuts/NoCuts',

                ####################################################
                # Set up the generator ...                         #
                ####################################################
                'cd /Herwig/Generators',
                'set EventGenerator:EventHandler  /Herwig/EventHandlers/theLesHouchesHandler',
                'set EventGenerator:NumberOfEvents 100000000',
                #'set EventGenerator:RandomNumberGenerator:Seed 31122001', #gets set by CMSSW
                'set EventGenerator:PrintEvent     1',
                'set EventGenerator:MaxErrors      10000',

                #############################################################
                # Set up the FxFxHandler ...                              #
                #############################################################
                'cd /Herwig/Shower',
                'set /Herwig/Shower/FxFxHandler:MPIHandler  /Herwig/UnderlyingEvent/MPIHandler',
                'set /Herwig/Shower/FxFxHandler:RemDecayer  /Herwig/Partons/RemnantDecayer',
                'set /Herwig/Shower/FxFxHandler:ShowerAlpha  AlphaQCD',
                # set the heavy quark decay product vetoing process on/off
                'set FxFxHandler:HeavyQVeto Yes',
                # Automatic detection of the hard process (experimental)
                'set FxFxHandler:HardProcessDetection Automatic',
                'set FxFxHandler:ihrd        3',
                # No. of light jets in maximum-multiplicity FxFx process
                'set FxFxHandler:njetsmax      4', # Maximum number of colored partons in the matrix element. Make sure this is set correctly!
                # Mimimum parton-parton R-sep used for generation.
                'set FxFxHandler:drjmin      0',

                ######################################################### 
                # Recommended key merging parameters below              #
                # Please make sure that they are inline with        #
                # the settings in the gridpack              #
                ######################################################### 
                'cd /Herwig/Shower',
                # turn the Vetoing On or Off completely
                'set FxFxHandler:VetoIsTurnedOff VetoingIsOn',
                # merging mode: in this case Tree level with MG5 information:
                'set FxFxHandler:MergeMode TreeMG5', #setting for MLM with LO MG5
                # merging scale
                # Set merging scale according to same value as in gridpack used
                'set FxFxHandler:ETClus 19*GeV', 
                # jet radius used in clustering in merging.
                'set FxFxHandler:RClus 1.0',
                # Max |eta| for jets in clustering in merging.
                'set FxFxHandler:EtaClusMax 10',
                # Default 1.5 factor used to decide if a jet matches a parton
                # in merging: if DR(parton,jet)<rclusfactor*rclus the parton 
                # and jet are said to have been matched.
                'set FxFxHandler:RClusFactor 1.5'
),
    parameterSets = cms.vstring('hw_LHEsettings', 'herwig7CH2PDF', 'herwig7CH2AlphaS', 'herwig7CH2MPISettings'),
    configFiles = cms.vstring(),
    crossSection = cms.untracked.double(-1),
    dataLocation = cms.string('${HERWIGPATH:-6}'),
    eventHandlers = cms.string('/Herwig/EventHandlers'),
    filterEfficiency = cms.untracked.double(1.0),
    generatorModule = cms.string('/Herwig/Generators/EventGenerator'),
    repository = cms.string('${HERWIGPATH}/HerwigDefaults.rpo'),
    run = cms.string('DYJetsMLM'),
    runModeList = cms.untracked.string('read,run')
)

ProductionFilterSequence = cms.Sequence(generator)
