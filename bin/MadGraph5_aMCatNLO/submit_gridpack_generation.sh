#!/bin/bash

memory=${1}
diskspace=${2}
masterqueue=${3}
carddir=${4}
name=${5}
workqueue=${6}

lsfselect="select[!noturbo &! fullsmt &! smt &! plus &! spacecharge && type==SLC6_64 &&!(model==a6_48_1091h17_1333 || model==i6_8_61a5h23_1066 || model==ai_intel_8) ]"

#lsfselect="select[qa && type==SLC6_64]"

bsub -q ${masterqueue} -C 0  -R "${lsfselect} rusage[mem=${memory}:pool=${diskspace}]" "export PRODHOME=`pwd`; gridpack_generation.sh ${carddir} ${name} ${workqueue}"
