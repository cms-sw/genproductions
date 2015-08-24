#!/bin/bash

memory=${1}
diskspace=${2}
masterqueue=${3}
name=${4}
carddir=${5}
workqueue=${6}
model=${7}
model2=${8}

bsub -q ${masterqueue} -C 0  -R "rusage[mem=${memory}:pool=${diskspace}]" "export PRODHOME=`pwd`; gridpack_generation.sh ${name} ${carddir} ${workqueue} ${model} ${model2}"
