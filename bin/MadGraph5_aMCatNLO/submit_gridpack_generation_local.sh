#!/bin/bash

memory=${1}
masterqueue=${2}
name=${3}
carddir=${4}
workqueue=${5}
model=${6}

bsub -q ${masterqueue} -C 0  -R "rusage[mem=${memory}]" "cd `pwd`; gridpack_generation.sh ${name} ${carddir} ${workqueue} ${model}"
