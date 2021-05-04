#!/bin/bash
# arg 1 = NEVENTS
# arg 2 = procid (seed)


scram p CMSSW CMSSW_10_6_7
cd ~/CMSSW_10_6_7
eval  `scram ru -sh`
cmsenv
cd -

tar xvzf /afs/cern.ch/user/g/gumoret/public/X0jj_slc7_amd64_gcc820_CMSSW_10_6_7_my_X0jj.tgz

./runcmsgrid.sh $1 $2 1

cp cmsgrid_final.lhe /afs/cern.ch/user/g/gumoret/condor_output_odd/X0jj_$2.lhe


