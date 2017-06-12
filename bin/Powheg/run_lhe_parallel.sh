#!/bin/bash

fail_exit() { echo "$@"; exit 1; }

#set -o verbose
EXPECTED_ARGS=3

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` PowhegTarfile Njobs NeventsPerJob"
    echo "Example: `basename $0` my_ggh_nnpdf30.tgz 10 1000" 
    exit 1
fi

echo "   ______________________________________     "
echo "      Producing Powheg LHE in parallel        "
echo "   ______________________________________     "

tar=${1}
echo "%MSG-POWHEG tar file = $tar"

numj=${2}
echo "%MSG-POWHEG number of jobs = $numj"

nume=${3}
echo "%MSG-POWHEG number of events per job = $nume"

iteration=1
eval `scramv1 runtime -sh`

while [ $iteration -le $numj ];
    do

    echo -e "Submit job ${iteration}\n"
    mkdir -p jobPowheg${iteration}
    cd jobPowheg${iteration}
    tar -xzvf ../${tar}
    export WORKDIR=`pwd`
    touch submit.sh
    echo -e "cd ${WORKDIR}" >> submit.sh
    echo -e 'eval `scramv1 runtime -sh`' >> submit.sh
    echo -e "./runcmsgrid.sh ${nume} ${iteration} 1" >> submit.sh 
    chmod a+x submit.sh 
    bsub -q 1nw -J jobPowheg${iteration} < submit.sh
    cd ..
    iteration=$(( iteration + 1 ))
   done

echo "All jobs submitted"
echo "End of job on " `date`
exit 0;
