# H->ZA->llbb gridpacks production:
   # Default: Preparing Cards 
- You need 2HDMC which is a general-purpose calculator for the two-Higgs doublet model. It allows parametrization of the Higgs potential in many different ways, convenient specification of generic Yukawa sectors, the evaluation of decay widths (including higher-order QCD corrections), theoretical constraints and much more.
- You can either install Calculators42HDM with Conda or in a CMSSW release, details [here](https://github.com/kjaffel/Calculators42HDM#install-with-conda-from-pdavid)
```bash
# We will use CMSSW relase here
# Setup your env
module load gcc/gcc-7.3.0-sl7_amd64 lhapdf/6.1.6-sl7_gcc73
# Install a CMSSW release .eg. CMSSW_10_2_22
cmsrel CMSSW_10_2_22
cmsenv
git cms-init

# Get and execute the install script
wget https://github.com/kjaffel/Calculators42HDM/master/sushi_2hdmc_cmssw.sh
source sushi_2hdmc_cmssw.sh

# Setup github remotes
source first_setup.sh
```
- Run as follow:
```bash
git clone -o origin https://github.com/cms-sw/genproductions.git
cd genproductions
git checkout HToZATo2L2B_run2Cards
git pull

./prepare_MG5_cards.py --order --test

```
- The script will look for;
`example_cards/template_HToZATo2L2B_200_50_1_bbH4F_TuneCP5_13TeV-amcatnlo_pythia8` if `--order NLO` and `example_cards/template_HToZATo2L2B_200_50_1_ggH_TuneCP5_13TeV_pythia8` if `--order LO ` in order to be able to produce the needed .dat cards for the requested signal mass points and tb values.
```
-o : --order  `LO` ggfusion - loop induce or `NLO`  b-associated production
-q : --queue : 1nh

--test   :  will produce 1 set of cards for each process, saved by default in example_cards/ 
                     and 2 bash scripts `prepare_example_{order}_gridpacks.sh` to genrate the gridpacks
-- lhaid :  will be set to `$DEFAULT_PDF_SETS` as shortcuts to have the PDF sets automatically and added to the run_card at run time to avoid specifying them directly
    lhapdf = pdlabel ! PDF set
    $DEFAULT_PDF_SETS = lhaid
    $DEFAULT_PDF_MEMBERS  = reweight_PDF
```
   # Generate gridpacks:
```bash
./prepare_example_nlo_gridpacks.sh
./prepare_example_lo_gridpacks.sh
```
