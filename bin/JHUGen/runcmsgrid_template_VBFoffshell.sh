#!/bin/bash

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`
use_gridpack_env=true
if [ -n "$4" ]
  then
  use_gridpack_env=$4
fi

if [ "$use_gridpack_env" = true ]
  then
    if [ -n "$5" ]
      then
        scram_arch_version=${5}
      else
        scram_arch_version=SCRAM_ARCH_VERSION_REPLACE
    fi
    echo "%MSG-MG5 SCRAM_ARCH version = $scram_arch_version"

    if [ -n "$6" ]
      then
        cmssw_version=${6}
      else
        cmssw_version=CMSSW_VERSION_REPLACE
    fi
    echo "%MSG-MG5 CMSSW version = $cmssw_version"
    export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
    source $VO_CMS_SW_DIR/cmsset_default.sh
    export SCRAM_ARCH=${scram_arch_version}
    scramv1 project CMSSW ${cmssw_version}
    cd ${cmssw_version}/src
    eval `scramv1 runtime -sh`
fi

set -euo pipefail

cd $LHEWORKDIR

eval $(JHUGenMELA/MELA/setup.sh env)
cd JHUGenerator/

#seq 1 5 | parallel -j${ncpu} --eta "GENCOMMAND VBFoffsh_run={} ReadCSmax"
#no parallel command on condor
#https://www.gnu.org/software/parallel/parallel_alternatives.html#DIFFERENCES-BETWEEN-xargs-AND-GNU-Parallel
seq 1 5 | xargs -d "\n" -P${ncpu} -I {} bash -c "GENCOMMAND VBFoffsh_run={} ReadCSmax"

(
cat Out_1.lhe      | grep -Ev "</LesHouchesEvents>"
cat Out_{2..4}.lhe | grep -Ev "</?LesHouchesEvents>" | sed -e '/<init>/,+3d'
cat Out_5.lhe      | grep -Ev "<LesHouchesEvents>"   | sed -e '/<init>/,+3d'
) > Out.lhe

mv Out.lhe $LHEWORKDIR/cmsgrid_final.lhe
cd $LHEWORKDIR
