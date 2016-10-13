# Higgs Dalitz Samples
In this directory we keep all cards used for *H &rightarrow; &gamma;\* &gamma; &rightarrow; ll;&gamma;*, aka **Higgs Dalitz decay**.
The samples produced with these cards are listed at [HZg MC twiki page](https://twiki.cern.ch/twiki/bin/view/CMS/HZgMC13TeV).

We use the
[Higgs Characterization](http://feynrules.irmp.ucl.ac.be/wiki/HiggsCharacterisation)
model in Madgraph/amcatnlo for the decay of the Higgs
boson. It is crucial part here, since *H
&rightsarrow; &gamma;\* &gamma;* process does not yet exist in
Pythia 8.  The mass of the *&gamma;\* &rightsarrow;* decay is
restricted to *0 < m(ll) < (50)60 GeV*.  However, the lower cut is
effectively goverened by the masses of leptons: *2m_l < m(ll)*, and
the masses of leptons are not zero in the decay model for that reason.


Gridpack production instructions can be found on
[QuickGuideMadGraph5aMCatNLO twiki](https://twiki.cern.ch/twiki/bin/viewauth/CMS/QuickGuideMadGraph5aMCatNLO). Just
an example, for *ggH125* production you need to run this command from *lxplus*:
```
./submit_gridpack_generation.sh 50000 50000 2nw ggH125_012j_NLO_FXFX_HtoMuMuGamma cards/production/13TeV/higgs/dalitz/ggH125_012j_NLO_FXFX_HtoMuMuGamma/ 1nw
```
It is important that this job is submitted with **2nw/1nw** queues
since it takes long to run.  Below we list the recommended queues for
other samples as well.


## Gluon fusion at NLO
Cards: *ggH120_012j_NLO_FXFX_HtoMuMuGamma*, *ggH125_012j_NLO_FXFX_HtoMuMuGamma*, *ggH130_012j_NLO_FXFX_HtoMuMuGamma*,
*ggH120_012j_NLO_FXFX_HtoElElGamma*, *ggH125_012j_NLO_FXFX_HtoElElGamma*, *ggH130_012j_NLO_FXFX_HtoElElGamma*

  * Use ```2nw/1nw``` queues
  * Production model: **HC**
  * Decay model: **HC**

## Gluon fusion at LO
Cards: *ggH120_MuMuGamma*, *ggH125_MuMuGamma*, *ggH130_MuMuGamma*, *ggH120_ElElGamma*, *ggH125_ElElGamma*, *ggH130_ElElGamma*

  * Use ```2nd/1nd``` queues
  * Production model: **HC**
  * Decay model: **HC**

## Vector Boson Fusion
Cards: *vbfH120*, *vbfH125*, *vbfH130*

  * Use ```2nd/1nd``` queues
  * Production model: **loop_sm-no_b_mass**
  * Decay model: **HC**

## Associated production
Cards for ZH: *ZH120*, *ZH125*, *ZH130*__
Cards for WH: *WH120*, *WH125*, *WH130*

  * Use ```1nw/2nd``` queues
  * Production model: **loop_sm-no_b_mass**
  * Decay model: **HC**

## Higgs to ZGamma
Card: *ggH125_012j_NLO_FXFX_HtoZGammaToMuMuGamma*

  * Use ```2nw/1nw``` queues
  * Production model: **HC**
  * Decay model: **HC**

It is included for cross-check with the official *H &rightarrow;
Z&gamma;* process, whcich was produced with Powheg (H production) +
Pythia 8 (H decay) combination.
