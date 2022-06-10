### Gridpacks for H+c and H+b MC samples

* [General Information](#general-information)

----------

#### General Information

This directory contains the configuration cards used
for Higgs+Charm and Higgs+Bottom MC samples
with `MadGraph5_aMCatNLO` at NLO QCD.

In addition to the standard set of MG5 cards
(process card, parameter card and run card),
each sample also requires a patch to the MG5 source code
in order to introduce the scale dependence ("running")
of the charm/bottom Yukawa coupling.
This patch was prepared by MG5 experts,
and it follows the approach used in [hep-ph/1409.5301](https://arxiv.org/abs/1409.5301)
and documented in studies of the bbH subgroup of the HXSWG
(see [Twiki](https://twiki.cern.ch/twiki/bin/view/LHCPhysics/LHCHWGBBH?rev=27#bbh_in_4FS_in_MG5_aMC_NLO)).

The production of the corresponding gridpacks requires using
[this user branch of `genproductions/`](https://github.com/missirol/genproductions/commits/prod_higgsPlusCharm),
in order to introduce small, but necessary, customisations.
These customisations include:

 - support for simulations in the 3-Flavour Scheme (3FS);

 - calculation of scale-variation and PDF weights using `reweight_{scale,pdf} = True` instead of `./aMCatNLO systematics`;
   this is needed because of the MG5 patch to introduce the scale dependence of the Yukawa coupling,
   which is not taken into account when running `./aMCatNLO systematics`.


