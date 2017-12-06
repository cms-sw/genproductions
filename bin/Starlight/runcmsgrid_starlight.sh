#!/bin/bash

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

FILENAME='slightout'
LHEWORKDIR=`pwd`
use_gridpack_env=false
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
cd $LHEWORKDIR

cd starlightTrunk/build
mv slight.in slightTemplate.in
cat slightTemplate.in | sed -e "s#RNDSEED#${rnum}#g" | sed -e "s#NEVT#${nevt}#g"  > slight.in

echo "*** STARTING STARLIGHT PRODUCTION ***"
./starlight
#remove the spurious random seed output that is non LHE standard 
cp slight.out ${LHEWORKDIR}
cd ${LHEWORKDIR}
echo "***STARLIGHT COMPLETE***"

#now convert the starlight file to a HepMC file
#curl https://raw.githubusercontent.com/kurtejung/genproductions/starlight_dev/bin/Starlight/convert_SL2LHE.C > convert_SL2LHE.C
root -l -b << EOF
.x convert_SL2LHE.C+(1,"slight.out","FILENAME") 
.q
EOF

mv FILENAME.lhe $LHEWORKDIR/cmsgrid_final.lhe
cd $LHEWORKDIR

ls -l
echo

exit 0
