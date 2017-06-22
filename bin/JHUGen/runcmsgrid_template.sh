#!/bin/bash

# scram_arch_version=${1}
scram_arch_version=slc6_amd64_gcc481
# cmssw_version=${2}
cmssw_version=CMSSW_7_1_28

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`
export SCRAM_ARCH=${scram_arch_version}
scramv1 project CMSSW ${cmssw_version}
cd ${cmssw_version}/src
eval `scramv1 runtime -sh`
cd $LHEWORKDIR

cd BASEDIR/

GENCOMMAND

mv Out.lhe ../cmsgrid_final.lhe
cd ..
