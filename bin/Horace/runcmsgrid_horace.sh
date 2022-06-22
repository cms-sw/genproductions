#!/bin/bash

fail_exit() { echo "$@"; exit 1; }

echo "   ______________________________________     "
echo "         Running Horace                       "
echo "   ______________________________________     "

nevt=${1}
echo "%MSG-HORACE number of events requested = $nevt"

rnum=${2}
echo "%MSG-HORACE random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-HORACE number of cputs for the run = $ncpu"


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
cd $LHEWORKDIR
 
seed=$rnum
file="cmsgrid"

# Release to be used to define the environment and the compiler needed
export WORKDIR=`pwd`

# LHAPDF setup
LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`
#if lhapdf6 external is available then above points to lhapdf5 and needs to be overridden
LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
if [ -e $LHAPDF6TOOLFILE ]; then
  LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
fi
#make sure env variable for pdfsets points to the right place
export LHAPDF_DATA_PATH=`$LHAPDFCONFIG --datadir`

# initialize the CMS environment 
card=${WORKDIR}/input

if [[ ! -e ${card} ]]; then
 fail_exit "powheg.input not found!"
fi

cat ${card} | sed -e "s#.*seed.*#${seed} ! seed#g" | sed -e "s#.*number.*#${nevt} ! number of events#g" > run.input
if [ -e sub.dat ]; then
  sed -i -e "s#.*calculate.*#read#g" run.input
fi

cat run.input
./horace < run.input 2>&1 | tee log_horace_${seed}.txt; test $? -eq 0 || fail_exit "horace error: exit code not 0"

cp run/*.evts ${file}_final.lhe

xmllint --stream --noout ${file}_final.lhe > /dev/null 2>&1; test $? -eq 0 || fail_exit "xmllint integrity check failed on pwgevents.lhe"

grep ">        NaN</wgt>" ${file}_final.lhe; test $? -ne 0 || fail_exit "Weights equal to NaN found, there must be a problem in the reweighting"

ls -l ${file}_final.lhe
pwd

# exit 0

if [ -s stat ]; then
  XSECTION=`tac stat | grep "weighted cross section" | awk '{ print $(NF-3) }'`
  XSECUNC=`tac stat | grep "weighted cross section" | awk '{ print $(NF-1) }'`

  head=`cat   cmsgrid_final.lhe | grep -in "<init>" | sed "s@:@ @g" | awk '{print $1+1}' | tail -1`
  tail=`wc -l cmsgrid_final.lhe | awk -v tmp="$head" '{print $1-2-tmp}'`
  tail -${tail} cmsgrid_final.lhe                           >  cmsgrid_final.lhe_tail
  head -${head} cmsgrid_final.lhe                           >  cmsgrid_final.lhe_F
  proclin=`expr $head + 1`
  proc=`sed -n -e ${proclin}p  cmsgrid_final.lhe |  awk '{print $4}'`
  echo "  "$XSECTION"   "$XSECUNC"  1.00000000000E-00 "$proc >>  cmsgrid_final.lhe_F
  echo "</init>"                                           >>  cmsgrid_final.lhe_F
  cat cmsgrid_final.lhe_tail                               >>  cmsgrid_final.lhe_F
  mv cmsgrid_final.lhe_F cmsgrid_final.lhe
fi
#Replace the negative so pythia will work
sed "s@-1000021@ 1000022@g" cmsgrid_final.lhe           > cmsgrid_final.lhe_F1
sed "s@1000021@1000022@g"   cmsgrid_final.lhe_F1          > cmsgrid_final.lhe
cp ${file}_final.lhe ${WORKDIR}/.

echo "Output ready with ${file}_final.lhe at $WORKDIR"
echo "End of job on " `date`
exit 0;
