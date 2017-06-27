#!/bin/bash

scram_arch_version=SCRAM_ARCH_VERSION_REPLACE
echo "%MSG-POWHEG SCRAM_ARCH version = $scram_arch_version"

cmssw_version=CMSSW_VERSION_REPLACE
echo "%MSG-POWHEG CMSSW version = $cmssw_version"

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`
export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
source $VO_CMS_SW_DIR/cmsset_default.sh
export SCRAM_ARCH=${scram_arch_version}
scramv1 project CMSSW ${cmssw_version}
cd ${cmssw_version}/src
eval `scramv1 runtime -sh`
cd $LHEWORKDIR

cd BASEDIR/

GENCOMMAND

mv Out.lhe ../cmsgrid_final.lhe
cd ..
