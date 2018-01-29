#!/bin/bash

echo "-------------------"
echo " Running STARLIGHT "
echo "-------------------"

repo=${1}
echo "%MSG-MG5 repository = $repo"

name=${5}
echo "%MSG-MG5 gridpack = $name"

cmsEnergy=${6}
echo "%MSG-MG5 energy = $cmsEnergy"

prodType=${7}
echo "%MSG-MG5 Using Production mode $prodType"

nevt=${2}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${3}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${4}
echo "%MSG-MG5 number of cpus = $ncpu"


cmsEnergyDiv2=$((cmsEnergy/2))

FILENAME='slightout'
LHEWORKDIR=`pwd`
use_gridpack_env=true
if [ -n "$8" ]
  then
  use_gridpack_env=$8
fi

if [ "$use_gridpack_env" = true ]
  then
    if [ -n "$9" ]
      then
        scram_arch_version=${9}
      else
        scram_arch_version=SCRAM_ARCH_VERSION_REPLACE
    fi
    echo "%MSG-MG5 SCRAM_ARCH version = $scram_arch_version"

    if [ -n "$10" ]
      then
        cmssw_version=${10}
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

tar xf $repo/$name

cd starlightTrunk/build
cat slightTemplateForNextProd.in | sed -e "s#RNDSEED#${rnum}#g" | sed -e "s#NEVT#${nevt}#g" | sed -e "s#B1G#${cmsEnergyDiv2}#g" | sed -e "s#B2G#${cmsEnergyDiv2}#g"  > slight.in

echo "*** STARTING STARLIGHT PRODUCTION ***"
if [ $prodType -ge 4 ]; then
./starlight < my.input &> log_${prodType}_${seed}.txt
else
./starlight &> log_${prodType}_${seed}.txt
fi
#remove the spurious random seed output that is non LHE standard 
cp slight.out ${LHEWORKDIR}
cd ${LHEWORKDIR}
echo "***STARLIGHT COMPLETE***"

#now convert the starlight file to a HepMC file
#curl https://raw.githubusercontent.com/kurtejung/genproductions/starlight_dev/bin/Starlight/convert_SL2LHE.C > convert_SL2LHE.C
root -l -b << EOF
.x convert_SL2LHE.C+(1,"slight.out","FILENAME",$cmsEnergyDiv2,$cmsEnergyDiv2) 
.q
EOF

mv FILENAME.lhe $LHEWORKDIR/cmsgrid_final.lhe
cd $LHEWORKDIR

ls -l
echo

exit 0
