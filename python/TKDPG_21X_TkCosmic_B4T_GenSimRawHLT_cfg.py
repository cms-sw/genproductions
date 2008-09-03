import FWCore.ParameterSet.Config as cms

process = cms.Process("GenSimRawHLT")
process.load("FWCore.MessageService.MessageLogger_cfi")

process.load("GeneratorInterface.CosmicMuonGenerator.CMSCGENsource_cfi")

process.load("GeneratorInterface.GenFilters.CosmicGenFilterHelix_cfi")

process.load("Configuration.StandardSequences.Simulation_cff")

process.load("Configuration.StandardSequences.Reconstruction_cff")

process.load("SimTracker.SiPixelDigitizer.PixelDigi_cfi")

process.load("Configuration.StandardSequences.MixingNoPileUp_cff")

process.load("Configuration.StandardSequences.VtxSmearedNoSmear_cff")
process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")


process.load("Configuration.StandardSequences.DigiToRaw_cff")

process.load("Configuration.StandardSequences.L1Emulator_cff")
process.load("L1Trigger.DTTrackFinder.dttfDigis_cfi")


process.load("Configuration.EventContent.EventContent_cff")

process.load("Configuration.StandardSequences.MagneticField_cff")

process.load("Configuration.StandardSequences.Geometry_cff")
#process.load("Configuration.StandardSequences.TrackerHLTFromConfDB_cff")




process.FitterRK = cms.ESProducer("KFTrajectoryFitterESProducer",
    ComponentName = cms.string('FitterRK'),
    Estimator = cms.string('Chi2'),
    Propagator = cms.string('RungeKuttaTrackerPropagator'),
    Updator = cms.string('KFUpdator'),
    minHits = cms.int32(3)
)

process.FittingSmootherRK = cms.ESProducer("KFFittingSmootherESProducer",
    EstimateCut = cms.double(-1.0),
    Fitter = cms.string('FitterRK'),
    MinNumberOfHits = cms.int32(5),
    Smoother = cms.string('SmootherRK'),
    BreakTrajWith2ConsecutiveMissing = cms.bool(False),
    ComponentName = cms.string('FittingSmootherRK'),
    NoInvalidHitsBeginEnd = cms.bool(False),
    RejectTracks = cms.bool(True)
)

process.SiStripRegionConnectivity = cms.ESProducer("SiStripRegionConnectivity",
    EtaDivisions = cms.untracked.uint32(20),
    PhiDivisions = cms.untracked.uint32(20),
    EtaMax = cms.untracked.double(2.5)
)

process.SmootherRK = cms.ESProducer("KFTrajectorySmootherESProducer",
    errorRescaling = cms.double(100.0),
    minHits = cms.int32(3),
    ComponentName = cms.string('SmootherRK'),
    Estimator = cms.string('Chi2'),
    Updator = cms.string('KFUpdator'),
    Propagator = cms.string('RungeKuttaTrackerPropagator')
)

process.hltOfflineBeamSpot = cms.EDProducer("BeamSpotProducer")

process.hltSiPixelDigis = cms.EDFilter("SiPixelRawToDigi",
    InputLabel = cms.untracked.string('rawDataCollector')
)

process.hltSiPixelClusters = cms.EDProducer("SiPixelClusterProducer",
    src = cms.InputTag("hltSiPixelDigis"),
    ChannelThreshold = cms.int32(2500),
    MissCalibrate = cms.untracked.bool(True),
    VCaltoElectronGain = cms.int32(65),
    VCaltoElectronOffset = cms.int32(0),
    payloadType = cms.string('HLT'),
    SeedThreshold = cms.int32(3000),
    ClusterThreshold = cms.double(5050.0)
)

process.hltSiPixelRecHits = cms.EDFilter("SiPixelRecHitConverter",
    src = cms.InputTag("hltSiPixelClusters"),
    CPE = cms.string('PixelCPEGeneric')
)

process.hltSiStripRawToClustersFacility = cms.EDFilter("SiStripRawToClusters",
    ProductLabel = cms.untracked.string('rawDataCollector'),
    ChannelThreshold = cms.untracked.double(2.0),
    MaxHolesInCluster = cms.untracked.uint32(0),
    ClusterizerAlgorithm = cms.untracked.string('ThreeThreshold'),
    SeedThreshold = cms.untracked.double(3.0),
    ClusterThreshold = cms.untracked.double(5.0)
)

process.hltSiStripClusters = cms.EDProducer("MeasurementTrackerSiStripRefGetterProducer",
    InputModuleLabel = cms.InputTag("hltSiStripRawToClustersFacility"),
    measurementTrackerName = cms.string('')
)

process.hltBoolEnd = cms.EDFilter("HLTBool",
    result = cms.bool(True)
)

process.SkippingLayerCosmicNavigationSchoolESProducer = cms.ESProducer("SkippingLayerCosmicNavigationSchoolESProducer",
    noPXB = cms.bool(True),
    ComponentName = cms.string('NoPixelCosmicNavigationSchool'),
    noPXF = cms.bool(True),
    noTIB = cms.bool(False),
    noTID = cms.bool(False),
    noTEC = cms.bool(False),
    noTOB = cms.bool(False)
)

process.ckfTrajectoryFilterP5 = cms.ESProducer("TrajectoryFilterESProducer",
    filterPset = cms.PSet(
        minimumNumberOfHits = cms.int32(4),
        minHitsMinPt = cms.int32(3),
        ComponentType = cms.string('CkfBaseTrajectoryFilter'),
        maxLostHits = cms.int32(4),
        maxNumberOfHits = cms.int32(-1),
        maxConsecLostHits = cms.int32(4),
        chargeSignificance = cms.double(-1.0),
        nSigmaMinPt = cms.double(5.0),
        minPt = cms.double(0.5)
    ),
    ComponentName = cms.string('ckfTrajectoryFilterP5')
)

process.groupedCkfTrajectoryBuilderP5 = cms.ESProducer("GroupedCkfTrajectoryBuilderESProducer",
    bestHitOnly = cms.bool(True),
    propagatorAlong = cms.string('PropagatorWithMaterial'),
    trajectoryFilterName = cms.string('ckfTrajectoryFilterP5'),
    maxCand = cms.int32(5),
    ComponentName = cms.string('groupedCkfTrajectoryBuilderP5'),
    propagatorOpposite = cms.string('PropagatorWithMaterialOpposite'),
    lostHitPenalty = cms.double(30.0),
    MeasurementTrackerName = cms.string(''),
    lockHits = cms.bool(True),
    TTRHBuilder = cms.string('WithTrackAngle'),
    foundHitBonus = cms.double(5.0),
    updator = cms.string('KFUpdator'),
    alwaysUseInvalidHits = cms.bool(True),
    requireSeedHitsInRebuild = cms.bool(True),
    estimator = cms.string('Chi2'),
    intermediateCleaning = cms.bool(True),
    minNrOfHitsForRebuild = cms.int32(5)
)

process.CosmicsRoads = cms.ESProducer("RoadMapMakerESProducer",
    ComponentName = cms.string('CosmicsRoads'),
    SeedingType = cms.string('TwoRingSeeds'),
    RingsLabel = cms.string('CosmicsRings'),
    GeometryStructure = cms.string('P5'),
    RoadMapAsciiFile = cms.untracked.string('roads.dat')
)

process.CosmicsRings = cms.ESProducer("RingMakerESProducer",
    DetIdsDumpFileName = cms.untracked.string('tracker_detids.dat'),
    ComponentName = cms.string('CosmicsRings'),
    Configuration = cms.untracked.string('P5'),
    RingAsciiFileName = cms.untracked.string('rings_p5.dat')
)

process.CosmicsKFFittingSmoother = cms.ESProducer("KFFittingSmootherESProducer",
    EstimateCut = cms.double(-1.0),
    Fitter = cms.string('FitterRK'),
    MinNumberOfHits = cms.int32(4),
    Smoother = cms.string('SmootherRK'),
    BreakTrajWith2ConsecutiveMissing = cms.bool(False),
    ComponentName = cms.string('CosmicsKFFittingSmoother'),
    NoInvalidHitsBeginEnd = cms.bool(False),
    RejectTracks = cms.bool(True)
)

process.hltSiStripMatchedRecHits = cms.EDFilter("SiStripRecHitConverter",
    StripCPE = cms.string('StripCPEfromTrackAngle'),
    LazyGetterProducer = cms.string('hltSiStripRawToClustersFacility'),
    stereoRecHits = cms.string('stereoRecHit'),
    Matcher = cms.string('StandardMatcher'),
    matchedRecHits = cms.string('matchedRecHit'),
    Regional = cms.bool(True),
    ClusterProducer = cms.string('hltSiStripClusters'),
    rphiRecHits = cms.string('rphiRecHit'),
    useSiStripQuality = cms.bool(False),
    siStripQualityLabel = cms.string(''),
    MaskBadAPVFibers = cms.bool(False)
)

process.hltCosmicsRPHIrechitfilter = cms.EDFilter("HLTCountNumberOfSingleRecHit",
    src = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit"),
    MaxN = cms.int32(300),
    MinN = cms.int32(0)
)

process.hltCosmicsSTEREOrechitfilter = cms.EDFilter("HLTCountNumberOfSingleRecHit",
    src = cms.InputTag("hltSiStripMatchedRecHits","stereoRecHit"),
    MaxN = cms.int32(300),
    MinN = cms.int32(0)
)

process.hltCosmicsMATCHEDrechitfilter = cms.EDFilter("HLTCountNumberOfMatchedRecHit",
    src = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
    MaxN = cms.int32(300),
    MinN = cms.int32(0)
)

process.hltCosmicsCombinatorialSeedFinder = cms.EDFilter("CtfSpecialSeedGenerator",
    SeedMomentum = cms.double(5.0),
    ErrorRescaling = cms.double(50.0),
    OrderedHitsFactoryPSets = cms.VPSet(cms.PSet(
        ComponentName = cms.string('GenericTripletGenerator'),
        LayerPSet = cms.PSet(
            TOB5 = cms.PSet(
                TTRHBuilder = cms.string('WithTrackAngle'),
                rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit")
            ),
            TOB4 = cms.PSet(
                TTRHBuilder = cms.string('WithTrackAngle'),
                rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit")
            ),
            layerList = cms.vstring('TOB4+TOB5+TOB6', 
                'TOB3+TOB5+TOB6', 
                'TOB3+TOB4+TOB5', 
                'TOB2+TOB4+TOB5', 
                'TOB3+TOB4+TOB6', 
                'TOB2+TOB4+TOB6'),
            TOB6 = cms.PSet(
                TTRHBuilder = cms.string('WithTrackAngle'),
                rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit")
            ),
            TOB3 = cms.PSet(
                TTRHBuilder = cms.string('WithTrackAngle'),
                rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit")
            ),
            TOB2 = cms.PSet(
                matchedRecHits = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
                TTRHBuilder = cms.string('WithTrackAngle')
            )
        ),
        PropagationDirection = cms.string('alongMomentum'),
        NavigationDirection = cms.string('outsideIn')
    ), 
        cms.PSet(
            ComponentName = cms.string('GenericPairGenerator'),
            LayerPSet = cms.PSet(
                TOB5 = cms.PSet(
                    TTRHBuilder = cms.string('WithTrackAngle'),
                    rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit")
                ),
                TOB4 = cms.PSet(
                    TTRHBuilder = cms.string('WithTrackAngle'),
                    rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit")
                ),
                layerList = cms.vstring('TOB5+TOB6', 
                    'TOB4+TOB5'),
                TOB6 = cms.PSet(
                    TTRHBuilder = cms.string('WithTrackAngle'),
                    rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit")
                )
            ),
            PropagationDirection = cms.string('alongMomentum'),
            NavigationDirection = cms.string('outsideIn')
        ), 
        cms.PSet(
            ComponentName = cms.string('GenericPairGenerator'),
            LayerPSet = cms.PSet(
                layerList = cms.vstring('TEC1_pos+TEC2_pos', 
                    'TEC2_pos+TEC3_pos', 
                    'TEC3_pos+TEC4_pos', 
                    'TEC4_pos+TEC5_pos', 
                    'TEC5_pos+TEC6_pos', 
                    'TEC6_pos+TEC7_pos', 
                    'TEC7_pos+TEC8_pos', 
                    'TEC8_pos+TEC9_pos'),
                TEC = cms.PSet(
                    minRing = cms.int32(5),
                    matchedRecHits = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
                    useRingSlector = cms.untracked.bool(True),
                    TTRHBuilder = cms.string('WithTrackAngle'),
                    rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit"),
                    maxRing = cms.int32(7)
                )
            ),
            PropagationDirection = cms.string('alongMomentum'),
            NavigationDirection = cms.string('outsideIn')
        ), 
        cms.PSet(
            ComponentName = cms.string('GenericPairGenerator'),
            LayerPSet = cms.PSet(
                layerList = cms.vstring('TEC1_pos+TEC2_pos', 
                    'TEC2_pos+TEC3_pos', 
                    'TEC3_pos+TEC4_pos', 
                    'TEC4_pos+TEC5_pos', 
                    'TEC5_pos+TEC6_pos', 
                    'TEC6_pos+TEC7_pos', 
                    'TEC7_pos+TEC8_pos', 
                    'TEC8_pos+TEC9_pos'),
                TEC = cms.PSet(
                    minRing = cms.int32(5),
                    matchedRecHits = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
                    useRingSlector = cms.untracked.bool(True),
                    TTRHBuilder = cms.string('WithTrackAngle'),
                    rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit"),
                    maxRing = cms.int32(7)
                )
            ),
            PropagationDirection = cms.string('alongMomentum'),
            NavigationDirection = cms.string('insideOut')
        ), 
        cms.PSet(
            ComponentName = cms.string('GenericPairGenerator'),
            LayerPSet = cms.PSet(
                layerList = cms.vstring('TEC1_neg+TEC2_neg', 
                    'TEC2_neg+TEC3_neg', 
                    'TEC3_neg+TEC4_neg', 
                    'TEC4_neg+TEC5_neg', 
                    'TEC5_neg+TEC6_neg', 
                    'TEC6_neg+TEC7_neg', 
                    'TEC7_neg+TEC8_neg', 
                    'TEC8_neg+TEC9_neg'),
                TEC = cms.PSet(
                    minRing = cms.int32(5),
                    matchedRecHits = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
                    useRingSlector = cms.untracked.bool(True),
                    TTRHBuilder = cms.string('WithTrackAngle'),
                    rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit"),
                    maxRing = cms.int32(7)
                )
            ),
            PropagationDirection = cms.string('alongMomentum'),
            NavigationDirection = cms.string('outsideIn')
        ), 
        cms.PSet(
            ComponentName = cms.string('GenericPairGenerator'),
            LayerPSet = cms.PSet(
                layerList = cms.vstring('TEC1_neg+TEC2_neg', 
                    'TEC2_neg+TEC3_neg', 
                    'TEC3_neg+TEC4_neg', 
                    'TEC4_neg+TEC5_neg', 
                    'TEC5_neg+TEC6_neg', 
                    'TEC6_neg+TEC7_neg', 
                    'TEC7_neg+TEC8_neg', 
                    'TEC8_neg+TEC9_neg'),
                TEC = cms.PSet(
                    minRing = cms.int32(5),
                    matchedRecHits = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
                    useRingSlector = cms.untracked.bool(True),
                    TTRHBuilder = cms.string('WithTrackAngle'),
                    rphiRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit"),
                    maxRing = cms.int32(7)
                )
            ),
            PropagationDirection = cms.string('alongMomentum'),
            NavigationDirection = cms.string('insideOut')
        )),
    PropagationDirection = cms.string('alongMomentum'),
    Charges = cms.vint32(-1, 1),
    ComponentName = cms.string('GlobalRegionProducer'),
    RegionFactoryPSet = cms.PSet(
        ComponentName = cms.string('GlobalRegionProducer'),
        RegionPSet = cms.PSet(
            precise = cms.bool(True),
            originHalfLength = cms.double(15.9),
            originRadius = cms.double(0.2),
            originYPos = cms.double(0.0),
            ptMin = cms.double(0.9),
            originXPos = cms.double(0.0),
            originZPos = cms.double(0.0)
        )
    ),
    MaxNumberOfCosmicClusters = cms.uint32(300),
    UseScintillatorsConstraint = cms.bool(False),
    NavigationDirection = cms.string('outsideIn'),
    TTRHBuilder = cms.string('WithTrackAngle'),
    LowerScintillatorParameters = cms.PSet(
        LenghtInZ = cms.double(100.0),
        GlobalX = cms.double(0.0),
        GlobalZ = cms.double(50.0),
        WidthInX = cms.double(100.0),
        GlobalY = cms.double(-100.0)
    ),
    SeedsFromPositiveY = cms.bool(True),
    doClusterCheck = cms.bool(True),
    CheckHitsAreOnDifferentLayers = cms.bool(False),
    SetMomentum = cms.bool(True),
    ClusterCollectionLabel = cms.InputTag("hltSiStripRawToClustersFacility"),
    UpperScintillatorParameters = cms.PSet(
        LenghtInZ = cms.double(100.0),
        GlobalX = cms.double(0.0),
        GlobalZ = cms.double(50.0),
        WidthInX = cms.double(100.0),
        GlobalY = cms.double(300.0)
    )
)

process.hltTrackerCTFSeedsFilter = cms.EDFilter("HLTCountNumberOfTrajectorySeed",
    src = cms.InputTag("hltCosmicsCombinatorialSeedFinder"),
    MaxN = cms.int32(10000000),
    MinN = cms.int32(1)
)

process.hltCosmicsCkfTrackCandidateMaker = cms.EDFilter("CkfTrackCandidateMaker",
    RedundantSeedCleaner = cms.string('CachingSeedCleanerBySharedInput'),
    TrajectoryCleaner = cms.string('TrajectoryCleanerBySharedHits'),
    SeedLabel = cms.string(''),
    useHitsSplitting = cms.bool(True),
    doSeedingRegionRebuilding = cms.bool(False),
    SeedProducer = cms.string('hltCosmicsCombinatorialSeedFinder'),
    NavigationSchool = cms.string('CosmicNavigationSchool'),
    TrajectoryBuilder = cms.string('groupedCkfTrajectoryBuilderP5'),
    TransientInitialStateEstimatorParameters = cms.PSet(
        propagatorAlongTISE = cms.string('PropagatorWithMaterial'),
        propagatorOppositeTISE = cms.string('PropagatorWithMaterialOpposite')
    )
)

process.hltCosmicsCtfWithMaterialTracks = cms.EDProducer("TrackProducer",
    src = cms.InputTag("hltCosmicsCkfTrackCandidateMaker"),
    clusterRemovalInfo = cms.InputTag(""),
    beamSpot = cms.InputTag("hltOfflineBeamSpot"),
    Fitter = cms.string('CosmicsKFFittingSmoother'),
    useHitsSplitting = cms.bool(False),
    alias = cms.untracked.string(''),
    TrajectoryInEvent = cms.bool(True),
    TTRHBuilder = cms.string('WithTrackAngle'),
    AlgorithmName = cms.string('cosmics'),
    Propagator = cms.string('RungeKuttaTrackerPropagator')
)

process.hltTrackerCTFTracksFilter = cms.EDFilter("HLTCountNumberOfTrack",
    src = cms.InputTag("hltCosmicsCtfWithMaterialTracks"),
    MaxN = cms.int32(1000),
    MinN = cms.int32(1)
)

process.hltTrackerCTFWithPixelHits = cms.EDFilter("HLTTrackWithHits",
    src = cms.InputTag("hltCosmicsCtfWithMaterialTracks"),
    MinN = cms.int32(1),
    MinPXL = cms.int32(2),
    MinBPX = cms.int32(0),
    MaxN = cms.int32(1000),
    MinFPX = cms.int32(0)
)

process.hltPixelLessCosmicsCkfTrackCandidateMaker = cms.EDFilter("CkfTrackCandidateMaker",
    RedundantSeedCleaner = cms.string('CachingSeedCleanerBySharedInput'),
    TrajectoryCleaner = cms.string('TrajectoryCleanerBySharedHits'),
    SeedLabel = cms.string(''),
    useHitsSplitting = cms.bool(True),
    doSeedingRegionRebuilding = cms.bool(False),
    SeedProducer = cms.string('hltCosmicsCombinatorialSeedFinder'),
    NavigationSchool = cms.string('NoPixelCosmicNavigationSchool'),
    TrajectoryBuilder = cms.string('CkfTrajectoryBuilder'),
    TransientInitialStateEstimatorParameters = cms.PSet(
        propagatorAlongTISE = cms.string('PropagatorWithMaterial'),
        propagatorOppositeTISE = cms.string('PropagatorWithMaterialOpposite')
    )
)

process.hltPixelLessCosmicsCtfWithMaterialTracks = cms.EDProducer("TrackProducer",
    src = cms.InputTag("hltPixelLessCosmicsCkfTrackCandidateMaker"),
    clusterRemovalInfo = cms.InputTag(""),
    beamSpot = cms.InputTag("hltOfflineBeamSpot"),
    Fitter = cms.string('CosmicsKFFittingSmoother'),
    useHitsSplitting = cms.bool(False),
    alias = cms.untracked.string(''),
    TrajectoryInEvent = cms.bool(True),
    TTRHBuilder = cms.string('WithTrackAngle'),
    AlgorithmName = cms.string('cosmics'),
    Propagator = cms.string('RungeKuttaTrackerPropagator')
)

process.hltPixelLessTrackerCTFTracksFilter = cms.EDFilter("HLTCountNumberOfTrack",
    src = cms.InputTag("hltPixelLessCosmicsCtfWithMaterialTracks"),
    MaxN = cms.int32(1000),
    MinN = cms.int32(1)
)

process.hltCTFPixelLessPixelPointingTrackFilter = cms.EDFilter("HLTMuonPointingFilter",
    maxZ = cms.double(46.0),
    radius = cms.double(11.0),
    PropagatorName = cms.string('SteppingHelixPropagatorAny'),
    SALabel = cms.string('hltPixelLessCosmicsCtfWithMaterialTracks')
)

process.hltCosmicsSeedFinder = cms.EDFilter("CosmicSeedGenerator",
    originHalfLength = cms.double(90.0),
    originZPosition = cms.double(0.0),
    matchedRecHits = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
    MaxNumberOfCosmicClusters = cms.uint32(300),
    SeedPt = cms.double(5.0),
    TTRHBuilder = cms.string('WithTrackAngle'),
    ptMin = cms.double(0.9),
    rphirecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit"),
    doClusterCheck = cms.bool(True),
    originRadius = cms.double(150.0),
    ClusterCollectionLabel = cms.InputTag("hltSiStripRawToClustersFacility"),
    stereorecHits = cms.InputTag("hltSiStripMatchedRecHits","stereoRecHit")
)

process.hltTrackerCoTFSeedsFilter = cms.EDFilter("HLTCountNumberOfTrajectorySeed",
    src = cms.InputTag("hltCosmicsSeedFinder"),
    MaxN = cms.int32(10000000),
    MinN = cms.int32(1)
)

process.hltCosmicsTrackFinder = cms.EDFilter("CosmicTrackFinder",
    TrajInEvents = cms.bool(True),
    MinHits = cms.int32(4),
    pixelRecHits = cms.InputTag("hltSiPixelRecHits"),
    matchedRecHits = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
    Chi2Cut = cms.double(30.0),
    TTRHBuilder = cms.string('WithTrackAngle'),
    rphirecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit"),
    cosmicSeeds = cms.InputTag("hltCosmicsSeedFinder"),
    stereorecHits = cms.InputTag("hltSiStripMatchedRecHits","stereoRecHit")
)

process.hltTrackerCoTFTracksFilter = cms.EDFilter("HLTCountNumberOfTrack",
    src = cms.InputTag("hltCosmicsTrackFinder"),
    MaxN = cms.int32(1000),
    MinN = cms.int32(1)
)

process.hltCoTFPixelPointingTrackFilter = cms.EDFilter("HLTMuonPointingFilter",
    maxZ = cms.double(46.0),
    radius = cms.double(11.0),
    PropagatorName = cms.string('SteppingHelixPropagatorAny'),
    SALabel = cms.string('hltCosmicsTrackFinder')
)

process.hltCosmicsRoadSearchSeedFinder = cms.EDFilter("RoadSearchSeedFinder",
    OuterSeedRecHitAccessMode = cms.string('STANDARD'),
    pixelRecHits = cms.InputTag("hltSiPixelRecHits"),
    MaximalEndcapImpactParameter = cms.double(1.2),
    MergeSeedsCenterCut_C = cms.double(0.4),
    MergeSeedsCenterCut_B = cms.double(0.25),
    MergeSeedsCenterCut_A = cms.double(0.05),
    MergeSeedsDifferentHitsCut = cms.uint32(1),
    rphiStripRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit"),
    MaximalBarrelImpactParameter = cms.double(0.2),
    doClusterCheck = cms.bool(True),
    stereoStripRecHits = cms.InputTag("hltSiStripMatchedRecHits","stereoRecHit"),
    RoadsLabel = cms.string('CosmicsRoads'),
    ClusterCollectionLabel = cms.InputTag("hltSiStripRawToClustersFacility"),
    OuterSeedRecHitAccessUseStereo = cms.bool(True),
    MaxNumberOfCosmicClusters = cms.uint32(300),
    MinimalReconstructedTransverseMomentum = cms.double(1.5),
    PhiRangeForDetIdLookupInRings = cms.double(0.5),
    Mode = cms.string('STRAIGHT-LINE'),
    MergeSeedsRadiusCut_A = cms.double(0.05),
    InnerSeedRecHitAccessMode = cms.string('STANDARD'),
    InnerSeedRecHitAccessUseStereo = cms.bool(True),
    OuterSeedRecHitAccessUseRPhi = cms.bool(True),
    MergeSeedsRadiusCut_B = cms.double(0.25),
    MergeSeedsRadiusCut_C = cms.double(0.4),
    matchedStripRecHits = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
    InnerSeedRecHitAccessUseRPhi = cms.bool(True)
)

process.hltTrackerRSSeedsFilter = cms.EDFilter("HLTCountNumberOfRoadSearchSeed",
    src = cms.InputTag("hltCosmicsRoadSearchSeedFinder"),
    MaxN = cms.int32(10000000),
    MinN = cms.int32(1)
)

process.hltCosmicsRoadSearchCloudMaker = cms.EDFilter("RoadSearchCloudMaker",
    MinimalFractionOfUsedLayersPerCloud = cms.double(0.3),
    pixelRecHits = cms.InputTag("hltSiPixelRecHits"),
    StraightLineNoBeamSpotCloud = cms.bool(True),
    UsePixelsinRS = cms.bool(False),
    SeedProducer = cms.InputTag("hltCosmicsRoadSearchSeedFinder"),
    DoCloudCleaning = cms.bool(True),
    IncreaseMaxNumberOfConsecutiveMissedLayersPerCloud = cms.uint32(0),
    rphiStripRecHits = cms.InputTag("hltSiStripMatchedRecHits","rphiRecHit"),
    UseStereoRecHits = cms.bool(True),
    ZPhiRoadSize = cms.double(0.06),
    MaximalFractionOfConsecutiveMissedLayersPerCloud = cms.double(0.35),
    stereoStripRecHits = cms.InputTag("hltSiStripMatchedRecHits","stereoRecHit"),
    MaximalFractionOfMissedLayersPerCloud = cms.double(0.8),
    scalefactorRoadSeedWindow = cms.double(150.0),
    MaxDetHitsInCloudPerDetId = cms.uint32(32),
    IncreaseMaxNumberOfMissedLayersPerCloud = cms.uint32(0),
    RoadsLabel = cms.string('CosmicsRoads'),
    MaxRecHitsInCloud = cms.int32(100),
    UseRphiRecHits = cms.bool(True),
    MergingFraction = cms.double(0.8),
    RPhiRoadSize = cms.double(5.0),
    matchedStripRecHits = cms.InputTag("hltSiStripMatchedRecHits","matchedRecHit"),
    MinimumHalfRoad = cms.double(3.3)
)

process.hltCosmicsRSTrackCandidateMaker = cms.EDFilter("RoadSearchTrackCandidateMaker",
    NumHitCut = cms.int32(4),
    InitialVertexErrorXY = cms.double(0.2),
    HitChi2Cut = cms.double(30.0),
    StraightLineNoBeamSpotCloud = cms.bool(True),
    nFoundMin = cms.int32(2),
    MinimumChunkLength = cms.int32(2),
    TTRHBuilder = cms.string('WithTrackAngle'),
    CosmicTrackMerging = cms.bool(True),
    MeasurementTrackerName = cms.string(''),
    CloudProducer = cms.InputTag("hltCosmicsRoadSearchCloudMaker"),
    CosmicSeedPt = cms.double(5.0),
    SplitMatchedHits = cms.bool(True)
)

process.hltCosmicsRSWithMaterialTracks = cms.EDProducer("TrackProducer",
    src = cms.InputTag("hltCosmicsRSTrackCandidateMaker"),
    clusterRemovalInfo = cms.InputTag(""),
    beamSpot = cms.InputTag("hltOfflineBeamSpot"),
    Fitter = cms.string('CosmicsKFFittingSmoother'),
    useHitsSplitting = cms.bool(False),
    alias = cms.untracked.string(''),
    TrajectoryInEvent = cms.bool(True),
    TTRHBuilder = cms.string('WithTrackAngle'),
    AlgorithmName = cms.string('rs'),
    Propagator = cms.string('RungeKuttaTrackerPropagator')
)

process.hltTrackerRSTracksFilter = cms.EDFilter("HLTCountNumberOfTrack",
    src = cms.InputTag("hltCosmicsRSWithMaterialTracks"),
    MaxN = cms.int32(1000),
    MinN = cms.int32(1)
)
process.HLTDoLocalPixelSequence = cms.Sequence(process.hltSiPixelDigis+process.hltSiPixelClusters+process.hltSiPixelRecHits)
process.HLTDoLocalStripSequence = cms.Sequence(process.hltSiStripRawToClustersFacility+process.hltSiStripClusters)
process.HLTDoLocalTrackerSequence = cms.Sequence(process.HLTDoLocalPixelSequence+process.HLTDoLocalStripSequence)
process.HLTEndSequence = cms.Sequence(process.hltBoolEnd)
process.HLTTrackerCosmicsRecHits = cms.Sequence(process.HLTDoLocalTrackerSequence+process.hltSiStripMatchedRecHits)
process.HLTTrackerCosmicsRecHitsFilter = cms.Sequence(process.hltCosmicsRPHIrechitfilter+process.hltCosmicsSTEREOrechitfilter+process.hltCosmicsMATCHEDrechitfilter)
process.HLTTrackerCosmicsCTF = cms.Sequence(process.HLTTrackerCosmicsRecHits+process.HLTTrackerCosmicsRecHitsFilter+process.hltCosmicsCombinatorialSeedFinder+process.hltTrackerCTFSeedsFilter+process.hltCosmicsCkfTrackCandidateMaker+process.hltOfflineBeamSpot+process.hltCosmicsCtfWithMaterialTracks+process.hltTrackerCTFTracksFilter)
process.HLTPixelLessComicsCTF = cms.Sequence(process.HLTTrackerCosmicsRecHits+process.HLTTrackerCosmicsRecHitsFilter+process.hltCosmicsCombinatorialSeedFinder+process.hltTrackerCTFSeedsFilter+process.hltPixelLessCosmicsCkfTrackCandidateMaker+process.hltOfflineBeamSpot+process.hltPixelLessCosmicsCtfWithMaterialTracks+process.hltPixelLessTrackerCTFTracksFilter+process.hltCTFPixelLessPixelPointingTrackFilter)
process.HLTTrackerCosmicsCoTF = cms.Sequence(process.HLTTrackerCosmicsRecHits+process.HLTTrackerCosmicsRecHitsFilter+process.hltCosmicsSeedFinder+process.hltTrackerCoTFSeedsFilter+process.hltCosmicsTrackFinder+process.hltTrackerCoTFTracksFilter)
process.HLTTrackerCosmicsRS = cms.Sequence(process.HLTTrackerCosmicsRecHits+process.HLTTrackerCosmicsRecHitsFilter+process.hltCosmicsRoadSearchSeedFinder+process.hltTrackerRSSeedsFilter+process.hltCosmicsRoadSearchCloudMaker+process.hltCosmicsRSTrackCandidateMaker+process.hltOfflineBeamSpot+process.hltCosmicsRSWithMaterialTracks+process.hltTrackerRSTracksFilter)
process.CandHLTTrackerCosmicCTFWithPxlCount = cms.Path(process.HLTTrackerCosmicsCTF+process.hltTrackerCTFWithPixelHits+process.HLTEndSequence)
process.CandHLTTrackerCTFPxlLessPxlPointing = cms.Path(process.HLTPixelLessComicsCTF+process.HLTEndSequence)
process.CandHLTTrackerCoTFPixelPointing = cms.Path(process.HLTTrackerCosmicsCoTF+process.hltCoTFPixelPointingTrackFilter+process.HLTEndSequence)








process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(200)
)
process.options = cms.untracked.PSet(
    wantSummary = cms.untracked.bool(True)
)


process.GlobalTag.globaltag = 'STARTUP_V4::All'

process.RandomNumberGeneratorService = cms.Service("RandomNumberGeneratorService",
    moduleSeeds = cms.PSet(
        g4SimHits = cms.untracked.uint32(14),
        ecalUnsuppressedDigis = cms.untracked.uint32(1234571),
        muonCSCDigis = cms.untracked.uint32(11223347),
        mix = cms.untracked.uint32(12348),
        hcalUnsuppressedDigis = cms.untracked.uint32(1234571),
        VtxSmeared = cms.untracked.uint32(98765434),
        siPixelDigis = cms.untracked.uint32(1234571),
        muonDTDigis = cms.untracked.uint32(1234571),
        siStripDigis = cms.untracked.uint32(1234571),
        muonRPCDigis = cms.untracked.uint32(1234571),
	simSiPixelDigis = cms.untracked.uint32(1234571),
       simSiStripDigis = cms.untracked.uint32(1234571),
       simEcalUnsuppressedDigis = cms.untracked.uint32(1234571),
       simHcalUnsuppressedDigis = cms.untracked.uint32(1234571),
       simMuonCSCDigis = cms.untracked.uint32(1234571),
       simMuonDTDigis = cms.untracked.uint32(1234571),
       simMuonRPCDigis = cms.untracked.uint32(1234571)

    ),
    sourceSeed = cms.untracked.uint32(789654783)
)
process.randomEngineStateProducer = cms.EDProducer("RandomEngineStateProducer")

process.FEVT = cms.OutputModule("PoolOutputModule",
    fileName = cms.untracked.string('CosmicGenSimRawHLT4T.root'),
    SelectEvents = cms.untracked.PSet(
        SelectEvents = cms.vstring('c')
    ),
    outputCommands = cms.untracked.vstring('keep *', 
        'drop FEDRawDataCollection_*_*_*', 
        'keep FEDRawDataCollection_rawDataCollector__GenSimRawHLT')
)

process.c = cms.Path(process.cosmicInTracker*process.g4SimHits*process.pdigi*process.L1Emulator*process.DigiToRaw)
process.CandHLTTrackerCosmicsCoTF = cms.Path(process.cosmicInTracker+process.HLTTrackerCosmicsCoTF+process.HLTEndSequence)
process.CandHLTTrackerCosmicsRS = cms.Path(process.cosmicInTracker+process.HLTTrackerCosmicsRS+process.HLTEndSequence)
process.CandHLTTrackerCosmicsCTF = cms.Path(process.cosmicInTracker+process.HLTTrackerCosmicsCTF+process.HLTEndSequence)

process.outpath = cms.EndPath(process.FEVT)

process.schedule = cms.Schedule(process.c,process.CandHLTTrackerCosmicsCoTF,process.CandHLTTrackerCosmicsRS,process.CandHLTTrackerCosmicsCTF,process.outpath)

process.CosMuoGenSource.MinP = 4.
#process.cosmicInTracker.minP = 5.
process.cosmicInTracker.doMonitor = False
process.load("PhysicsTools.UtilAlgos.TFileService_cfi")
process.simSiPixelDigis.TofLowerCut = 18.5
process.simSiPixelDigis.TofUpperCut = 43.5
process.simSiStripDigis.CosmicDelayShift = 31.
process.MeasurementTracker.OnDemand = True
process.MeasurementTracker.pixelClusterProducer = 'hltSiPixelClusters'
process.MeasurementTracker.stripClusterProducer = 'hltSiStripClusters'
process.MeasurementTracker.stripLazyGetterProducer = 'hltSiStripRawToClustersFacility'
process.hltTrackerCTFSeedsFilter.MinN = 0
process.hltTrackerCoTFSeedsFilter.MinN = 0
process.hltTrackerRSSeedsFilter.MinN = 0
process.dttfDigis.Open_LUTs = True

