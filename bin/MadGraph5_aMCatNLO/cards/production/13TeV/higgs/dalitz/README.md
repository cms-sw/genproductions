# Higgs Dalitz Samples
In this directory we keep all cards used for Higgs Dalitz decay, H -> gamma* gamma -> mu mu gamma.
The samples produced with these cards are listed at [2][HZg twiki page].

Gridpack production instructions can be found [1][here]. Just an example, for ggH125 production you need to run this command:
```
./submit_gridpack_generation.sh 50000 50000 2nw  ggH125_012j_NLO_FXFX_HtoMuMuGamma cards/production/13TeV/higgs/dalitz/ggH125_012j_NLO_FXFX_HtoMuMuGamma/ 1nw
```
It is important that the job is submitted with 2nw/1nw queues since it takes long to run.
Below we list the recommended queues for other samples as well.

## Gluon fusion at NLO
Cards: *ggH120_012j_NLO_FXFX_HtoMuMuGamma*, *ggH125_012j_NLO_FXFX_HtoMuMuGamma*, *ggH130_012j_NLO_FXFX_HtoMuMuGamma*,
*ggH120_012j_NLO_FXFX_HtoElElGamma*, *ggH125_012j_NLO_FXFX_HtoElElGamma*, *ggH130_012j_NLO_FXFX_HtoElElGamma*

  * Use ```2nw/1nw``` queues
  * It has m(ll) < 60 GeV
  * Masses of leptons are not zero

## Gluon fusion at LO
Cards: *ggH120_MuMuGamma*, *ggH125_MuMuGamma*, *ggH130_MuMuGamma*, *ggH120_ElElGamma*, *ggH125_ElElGamma*, *ggH130_ElElGamma*

Use ```2nd/1nd``` queues

## Vector Boson Fusion
Cards: *vbfH120*, *vbfH125*, *vbfH130*

Use ```2nd/1nd``` queues

## Associated production
Cards for ZH: *ZH120*, *ZH125*, *ZH130*
Cards for WH: *WH120*, *WH125*, *WH130*

Use ```2nw/1nw``` queues


[1]: https://twiki.cern.ch/twiki/bin/viewauth/CMS/QuickGuideMadGraph5aMCatNLO

[2]: https://twiki.cern.ch/twiki/bin/viewauth/CMS/HZgMC13TeV
