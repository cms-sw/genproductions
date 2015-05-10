#!/bin/bash

cp patches/read_dm_params.f   src/Need/
cp patches/mcfm_writelhe.f    src/Need/
cp patches/dm_params.f        src/Inc/
cp patches/montecarlorpp.f    src/Inc/
cp patches/chooser.f          src/Procdep/
cp patches/process.DAT        Bin/
rm -rf src/DM
cp -r patches/DM src/DM
eval `scramv1 runtime -sh`
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/lib/
export LHAPATH=/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/share/LHAPDF/
./Install
make