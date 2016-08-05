#!/bin/bash

name=$1
carddir=$2
workqueue="condor"
bash gridpack_generation.sh ${name} ${carddir} ${workqueue}
