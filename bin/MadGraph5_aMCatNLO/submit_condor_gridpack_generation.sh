#!/bin/bash
source source_condor.sh

name=$1
carddir=$2
workqueue="condor"
bash gridpack_generation.sh ${name} ${carddir} ${workqueue}
