# Semi-visible jet signal production

To create complete gen fragments with specified parameters:
```
python createFragments.py
```

To create a CMSSW config (9_3_X) from a fragment:
```
cmsDriver.py Configuration/GenProduction/ThirteenTeV/SemiVisibleJets/SVJ_mZprime-3000_mDark-20_rinv-0p3_alpha-0p2_TuneCP2_13TeV_pythia8_cff --python_filename step1_GEN.py --mc --eventcontent RAWSIM --datatier GEN --conditions 93X_mc2017_realistic_v3 --beamspot Realistic25ns13TeVEarly2017Collision --step GEN --nThreads 4 --geometry DB:Extended --era Run2_2017 --customise_commands 'process.genParticlesForJetsNoMuNoNu.ignoreParticleIDs.extend([51,52,53]); process.genParticlesForJetsNoNu.ignoreParticleIDs.extend([51,52,53]); process.genCandidatesForMET.ignoreParticleIDs.extend([51,52,53]); process.genParticlesForMETAllVisible.ignoreParticleIDs.extend([51,52,53])' -n 10 --fileout file:step0.root --no_exec
```
(suggested process string corresponding to `--customise_commands`: "DMinvis")

To create scan configs for each year with a grid of signal points:
```
python generateScan.py -y 2016
python generateScan.py -y 2017
python generateScan.py -y 2018
```

For scan configs, an additional argument should be added to the driver command:
```
--customise_commands "process.source.numberEventsInLuminosityBlock = cms.untracked.uint32(200)"
```
