#!/bin/bash

fail_exit() { echo "$@"; exit 1; }

#set -o verbose
EXPECTED_ARGS=3

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` Nevents RandomSeed cpu"
    echo "Example: `basename $0` 1000 1212 cpu" 
    exit 1
fi

echo "   ______________________________________     "
echo "      Running multiple Powheg tarballs        "
echo "              for ttb_nlo_dec                 "
echo "   ______________________________________     "

nevt=${1}
echo "%MSG-POWHEG number of events requested = $nevt"

rnum=${2}
echo "%MSG-POWHEG random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-POWHEG number of cputs for the run = $ncpu"

seed=$rnum
file="cmsgrid"

process="hvq"

# Release to be used to define the environment and the compiler needed
export WORKDIR=`pwd`

# Call python script to run gridpacks for multiple decay channels
python multipack_ttb_nlo_dec.py --nevents $nevt --seed $rnum

echo "Output ready with ${file}_final.lhe at $WORKDIR"
echo "End of job on " `date`
exit 0;
