#!/bin/bash
source Utilities/source_condor.sh

name=$1
carddir=$2
workqueue="condor"

# Please define following variable to obtain more debug info during condor job submission
# export CONDOR_DEBUG_PRINT=1

# You can set following variable to keep logs, stdout and sterr for condor jobs (a lot of files)
export CONDOR_DEBUG_OUTPUT_PATH=""
if [ -n "$CONDOR_DEBUG_OUTPUT_PATH" ]; then
  if [ ! -d "$CONDOR_DEBUG_OUTPUT_PATH" ]; then
    echo "ERROR: "$CONDOR_DEBUG_OUTPUT_PATH" directory doesn't exist, please create it before condor run"
    exit 1
  fi
fi

# http://batchdocs.web.cern.ch/batchdocs/local/lsfmigratepractical.html
# espresso     = 20 minutes
# microcentury = 1 hour
# longlunch    = 2 hours
# workday      = 8 hours
# tomorrow     = 1 day
# testmatch    = 3 days
# nextweek     = 1 week
CONDOR_JOB_FLAVOUR="nextweek"

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
