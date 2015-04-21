#!/bin/bash

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`
basename=BASENAME

cd $LHEWORKDIR/$basename
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/lib/
export LHAPATH=/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5/share/LHAPDF/
sed "s@EVTS@$nevt@g" input_gen.DAT > input_g0.DAT
sed "s@XRAN@$rnum@g" input_g0.DAT  > input.DAT
./mcfm
#Replace the negative so pythia will work
sed "s@-1000022@1000022@g" FILENAME > $LHEWORKDIR/cmsgrid_final.lhe
cd $LHEWORKDIR

ls -l
echo

exit 0