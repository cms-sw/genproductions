#!/bin/bash
PTYHONPATH=$PYTHONPATH:/usr/lib64/python2.6/site-packages
source source_condor.sh

name=$1
carddir=$2
workqueue="condor"
scram_arch=$3
cmssw_version=$4
bash gridpack_generation.sh ${name} ${carddir} ${workqueue} ${scram_arch} ${cmssw_version}
