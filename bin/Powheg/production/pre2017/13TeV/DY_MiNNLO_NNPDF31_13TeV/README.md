# DY MiNNLO gridpack production

## General workflow

```
# check out 
git clone git@github.com:cms-sw/genproductions.git --no-checkout --depth 1
cd genproductions
git sparse-checkout set bin/Powheg bin/Horace bin/Winhac
git checkout
cd -

# set up CMSSW
cmsrel CMSSW_12_3_1
cd CMSSW_12_3_1/src

# link MiNNLO helper script
ln -s ../../genproductions/bin/Powheg/production/pre2017/13TeV/DY_MiNNLO_NNPDF31_13TeV/minnloHelper.sh .

# link Powheg scripts
./minnloHelper.sh INIT

# fetch and compile Powheg source + patches
./minnloHelper.sh COMPILE

# submit integration grids to batch system
./minnloHelper.sh GRIDS

# check resulting cross section
./minnloHelper.sh XS

# pack muon gridpacks
./minnloHelper.sh PACK

# create electron and tau gridpacks
./minnloHelper.sh PACK_LEP
```
