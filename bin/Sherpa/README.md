# Sherpack generation [still preliminary]
Get `sherpack` in one go by gathering commands from the [sherpa tutorial](https://twiki.cern.ch/twiki/bin/view/CMS/SherpaTutorial2019).

The example is as following:
```
./sherpack_generation.sh WJets Run.data_w 9
```
The arguments here are:
```
Process name: WJets
Input Run.dat: Run.dat_w
Number of runing cores: 9 
```
The `sherpack` is placed in **WJets_sherpack** folder.

The example `sherpa` configuration `Run.dat_w` is:
```
(run){
  % general setting
  EVENTS 1M; ERROR 0.99;

  % scales, tags for scale variations
  FSF:=1.; RSF:=1.; QSF:=1.;
  SCALES METS{FSF*MU_F2}{RSF*MU_R2}{QSF*MU_Q2};

  ## % optional: extra tags for custom jet criterion
  ## SHERPA_LDADD MyJetCriterion;
  ## JET_CRITERION FASTJET[A:antikt,R:0.4,y:5];

  % tags for process setup
  NJET:=0; QCUT:=20.;

  % me generator settings
  ME_SIGNAL_GENERATOR Comix Amegic LOOPGEN;
  EVENT_GENERATION_MODE Weighted;
  LOOPGEN:=OpenLoops;

  % exclude tau from lepton container
  MASSIVE[15] 1;

  % collider setup
  BEAM_1 2212; BEAM_ENERGY_1 = 6500.;
  BEAM_2 2212; BEAM_ENERGY_2 = 6500.;
}(run)

(processes){
  Process 93 93 -> 11 -12;
  Order (*,2); CKKW sqr(QCUT/E_CMS);
  End process;
}(processes)

(selector){
  Mass 11 -12 1. E_CMS
  Mass 13 -14 1. E_CMS
  Mass -11 12 1. E_CMS
  Mass -13 14 1. E_CMS
}(selector)
```
