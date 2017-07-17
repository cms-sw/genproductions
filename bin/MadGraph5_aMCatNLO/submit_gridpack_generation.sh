#!/bin/bash

memory=${1}
diskspace=${2}
masterqueue=${3}
name=${4}
carddir=${5}
workqueue=${6}
scram_arch=$7
cmssw_version=$8

bsub -q ${masterqueue} -C 0  -R "rusage[mem=${memory}:pool=${diskspace}]" "export PRODHOME=`pwd`; gridpack_generation.sh ${name} ${carddir} ${workqueue} ${scram_arch} ${cmssw_version}"
