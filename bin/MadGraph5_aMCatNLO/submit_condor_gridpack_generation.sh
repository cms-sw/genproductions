#!/bin/bash
source Utilities/source_condor.sh

name=$1
carddir=$2
workqueue="condor"

if [[ "${HOSTNAME}" =~ "lxplus" ]] && [[ ! $(pwd) =~ "/afs/" ]]; then
    echo "You are submitting from lxplus and the local directory is not on AFS."
    echo "Automatically switch to condor spool mode."
    workqueue="condor_spool"
fi

if [[ "${workqueue}" = "condor_spool" ]] && [[ $(pwd) =~ "/afs/" ]]; then
    echo "ERROR: You are using the condor SPOOL option while submitting from AFS." \
    "Spool only makes sense to allow condor submission from a file system unique to the submitter machine." \
    "Please either change directory to e.g. /tmp/user/ or simply do not use spool." \

    echo "Exiting."
    exit 1
fi

scram_arch=$3
cmssw_version=$4
bash gridpack_generation.sh ${name} ${carddir} ${workqueue} ALL ${scram_arch} ${cmssw_version}
