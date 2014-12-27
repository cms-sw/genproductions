#!/bin/bash

memory=${1}
diskspace=${2}
masterqueue=${3}
carddir=${4}
name=${5}
workqueue=${6}

lsfselect="select[type==SLC6_64]"

bsub -q ${masterqueue} -C 0  -R "${lsfselect} rusage[mem=${memory}:pool=${diskspace}]" "cd `pwd`; gridpack_generation.sh ${carddir} ${name} ${workqueue}"
