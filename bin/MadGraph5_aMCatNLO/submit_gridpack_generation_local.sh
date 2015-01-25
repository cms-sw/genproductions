#!/bin/bash

memory=${1}
masterqueue=${2}
carddir=${3}
name=${4}
workqueue=${5}

lsfselect="select[type==SLC6_64]"

bsub -q ${masterqueue} -C 0  -R "${lsfselect} rusage[mem=${memory}]" "cd `pwd`; gridpack_generation.sh ${carddir} ${name} ${workqueue}"
