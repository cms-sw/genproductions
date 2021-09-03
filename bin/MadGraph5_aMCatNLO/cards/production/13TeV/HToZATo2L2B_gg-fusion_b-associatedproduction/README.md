# Full RunII H/A->ZA/H->llbb 
## GridPacks production for full run2 ULegacy
## Setup Your Enviroment and Prepare Template cards:
This needs 2HDMC, which is a general-purpose calculator for the two-Higgs doublet model.
It allows parametrization of the Higgs potential in many different ways, convenient specification of generic Yukawa sectors, the evaluation of decay widths (including higher-order QCD corrections), theoretical constraints and much more.

You can install Calculators42HDM in a CMSSW release (recommended)
or a conda environment (which requires a few changes to the script), see the [installation instructions](https://github.com/kjaffel/Calculators42HDM/blob/master/README.md).
## GridPacks Preparation:
### Example of Template Cards:
- For each process we have a dir template, whatever changes you need to make it has to be for these cards Only !
```bash
# gg Fusion; LO Loop Induced 4F-scheme
cd run2Template_cards/template_HToZATo2L2B_X_X_X_ggH_TuneCP5_13TeV_pythia8
# b-associated Production; NLO 4F-scheme
cd run2Template_cards/template_HToZATo2L2B_X_X_X_bbH4F_TuneCP5_13TeV-amcatnlo_pythia8
```
``X_X_X`` : MH_MA_tanbeta parameters
### How to Run:
```python
# run a test
./prepare_MG5_cards.py --process bbH --test --templates run2Template_cards --mode H
```
- ``-p``/``process``: bbH or ggH
- ``-m``/``--mode`` : H or A means ( H->ZA or A->ZH)
- ``-q``/``--queue``: condor, condor_spool, slurm or 1nh 
- ``-s``/``--flavourscheme``: Production scheme 4FS, 5FS or None
- ``--customizecards``: default False, param_card.dat will be generated instead !
- ``--templates`` : a directory with run cards for the two processes, each in a subdirectory
- ``--gridpoints``: a directory with the JSON files with (mA, mH) points definitions
- ``--fullsim``: Generate 21 signal mass points per process saved by default in ``fullsim/``
- ``--benchmarks``: Generate 3 benchmarks scenarios for at high and low mass region of (MH, MA) for 5 different tb values, cards stored by default in  ``benchmarks/``
- ``--test`` : Will produce 1 set of cards for each process, saved by default in ``example_cards/``, if none of the 3 above args found the full list of ZAsamples for run2ULegacy will be produced/saved by default in ``PrivateProd_run2``
- ``-pdf``/``--lhapdfsets``  : If you pass ``NNPDF31`` , ``NNPDF31_nnlo_as_0118_nf_4_mc_hessian`` with ``lhaid 325500`` will be used for ``4FS`` and ``NNPDF31_nnlo_as_0118_mc_hessian_pdfas`` with lhaid ``325300`` if no scheme arg found !
If you leave this out, the default will be set to ``$DEFAULT_PDF_SETS`` as shortcuts to have the PDF sets automatically and added to the ``run_card`` at run time to avoid specifying them directly
```
    lhapdf = pdlabel ! PDF set
    $DEFAULT_PDF_SETS = lhaid
    $DEFAULT_PDF_MEMBERS  = reweight_PDF
```
OR pass different ``--lhapdfsets`` with ``--lhaid``
- ``--lhaid``: LHAID number , needed if you want to use different ``--lhapdfsets`` than the one mentionning above !

- Now in the dir cards the `blabla_param_card.dat` doesn't include the decay BR neither the total width for h3 and Z.
You have to overwrite this card for each mass point to avoid madspin launch the automatic computation of the widths ! 
Why you need to do that ? Because of these 2 open issue when using madspin [here](https://answers.launchpad.net/mg5amcnlo/+question/696286) and [here](https://answers.launchpad.net/mg5amcnlo/+question/696148).
- You still need `set_bottomYukawa_coupling_onshell.py` script [can be found here](https://github.com/kjaffel/ZAPrivateProduction/blob/master/set_bottomYukawa_coupling_onshell.py) to overwrite the `param_card.dat` this will set for you the bottom yukawa couplings and compare the decay widths and BR given by MadWidth to 2HDMCalculator. This script is intend to trust 2HDMC compuations in case of divergence of order 0.05 `abs(recalculated_width_fromMadWidth - width_in_the_banner_from2HDMC)/recalculated_width_fromMadWidth`
So Simply run as follow: 
```bash 
cd MG5_aMC_vX_X_X
# compute the decay BR and width using fake ymb for all pdgid mentionned in the madspin card ! 
./bin/mg5_aMC run_madwidths.sh
# set the yukawa coupling to the mb on-shell 
./run_yukawa_to_mbonshell.sh
```
## GridPacks Generation:
Inside the cards output directory (``example_cards`` or ``PrivateProd_run2``) a simple shell script is generated to produce all the gridpacks for each process.
It should be run from the genproductions/bin/MadGraph5_aMCatNLO directory in a clean environment.
```bash
./prepare_example_nlo_gridpacks.sh
./prepare_example_lo_gridpacks.sh
```
