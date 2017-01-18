#!/bin/bash

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

eval `scramv1 runtime -sh`

cd BASEDIR/

GENCOMMAND

mv Out.lhe ../cmsgrid_final.lhe
cd ..
