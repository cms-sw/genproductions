### Set up genproductions

Sparse-checkout recommended if you don't need the MadGraph directories.

```
git clone git@github.com:cms-sw/genproductions.git
cd genproductions
git sparse-checkout set bin/Powheg
cd ..
```

### Set up CMSSW and helper script

```
cmsrel CMSSW_12_3_1
cd CMSSW_12_3_1/src

ln -s ../../genproductions/bin/Powheg/production/pre2017/13TeV/HJ_MiNNLO_NNPDF31_13TeV/minnloHelper_HJ.sh .
./minnloHelper_HJ.sh INIT
```

### Produce gridpack

Edit/add input files in `HJ_MiNNLO_NNPDF31_13TeV`, add to `PROCS` in `minnloHelper_HJ.sh`.

```
./minnloHelper_HJ.sh COMPILE
./minnloHelper_HJ.sh GRIDS
./minnloHelper_HJ.sh XS
./minnloHelper_HJ.sh PACK
```