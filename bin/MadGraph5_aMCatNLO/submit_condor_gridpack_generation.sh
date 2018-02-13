#!/bin/bash
source Utilities/source_condor.sh

name=$1
carddir=$2
workqueue="condor_spool"
scram_arch=$3
cmssw_version=$4
bash gridpack_generation.sh ${name} ${carddir} ${workqueue} ALL ${scram_arch} ${cmssw_version}
