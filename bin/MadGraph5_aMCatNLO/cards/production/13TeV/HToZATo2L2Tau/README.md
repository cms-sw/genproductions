# Full RunII H/A->ZA/H->lltautau 
## GridPacks production for full run2 ULegacy
This README is mutuated from Khawla's HToZATo2L2B repo. Please refer to that for further explanation.
 
## Setup Your Enviroment and Prepare Template cards:
This needs 2HDMC, which is a general-purpose calculator for the two-Higgs doublet model.
It allows parametrization of the Higgs potential in many different ways, convenient specification of generic Yukawa sectors, the evaluation of decay widths (including higher-order QCD corrections), theoretical constraints and much more.

You can install Calculators42HDM in a CMSSW release (recommended)
or a conda environment (which requires a few changes to the script), see the [installation instructions](https://github.com/kjaffel/Calculators42HDM/blob/master/README.md).
## GridPacks Preparation:
### Example of Template Cards:
- For each process we have a dir template, HToZA or AToZH!
```bash
# gg Fusion; LO Loop Induced 4F-scheme
# H -> ZA (mH > mA)
run2Template_cards/template_HToZATo2L2Tau_X_X_X_ggH_TuneCP5_13TeV_pythia8
# A -> ZH (mA > mH)
run2Template_cards/template_AToZHTo2L2Tau_X_X_X_ggH_TuneCP5_13TeV_pythia8
```
``X_X_X`` : MH_MA_tanbeta parameters
### How to Run:
```python
./prepare_MG5_cards.py --process ggH --test --templates run2Template_cards/  --mode H
./prepare_MG5_cards.py --process ggH --test --templates run2Template_cards/  --mode A
```
```bash 
cd ./scripts/
# compute the decay BR and width 
{}/MG5_aMC_vX_X_X/bin/mg5_aMC run_madwidths.sh 
./write_paramcards.sh

```
## GridPacks Generation:
Inside the ``scripts`` directory a simple shell script is generated to produce all the gridpacks for each process.
It should be run from the genproductions/bin/MadGraph5_aMCatNLO directory in a clean environment.
```bash
./prepare_example_lo_htoza_gridpacks.sh
./prepare_example_lo_atozh_gridpacks.sh
```
