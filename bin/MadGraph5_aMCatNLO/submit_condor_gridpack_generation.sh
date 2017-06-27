#!/bin/bash

name=$1
carddir=$2
workqueue="condor"
scram_arch=$3
cmssw_version=$4
bash gridpack_generation.sh ${name} ${carddir} ${workqueue} ${scram_arch} ${cmssw_version}
