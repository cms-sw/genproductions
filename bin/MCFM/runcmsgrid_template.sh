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
cd $LHEWORKDIR

basename=BASENAME


LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`
#if lhapdf6 external is available then above points to lhapdf5 and needs to be overridden
LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
if [ -e $LHAPDF6TOOLFILE ]; then
  LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
fi
#make sure env variable for pdfsets points to the right place
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:`$LHAPDFCONFIG --libdir`
export LHAPDF_DATA_PATH=`$LHAPDFCONFIG --datadir`
export LHAPATH=$LHADPDF_DATA_PATH

inputnevt=$( echo ${nevt} \* 1.1 | bc)
inputnevt=${inputnevt%.*}
#sed "s@EVTS@$nevt@g" input_gen.DAT > input_g0.DAT
#sed "s@XRAN@$rnum@g" input_g0.DAT  > input.DAT
sed -i "s/NEVENT/"$inputnevt"/" INPUT.DAT
sed -i "s/SEED/"$rnum"/" INPUT.DAT

./mcfm INPUT.DAT
#Replace the negative so pythia will work
#sed "s@-1000022@1000022@g" FILENAME > $LHEWORKDIR/cmsgrid_final.lhe
lhefiles=($(ls *.lhe))
if [ ${#lhefiles[@]} -ne 1 ];then exit 1; else FILENAME=${lhefiles[0]}; fi
mv $FILENAME $LHEWORKDIR/cmsgrid_final.lhe
fevt=$(grep "<event>" cmsgrid_final.lhe|wc -l)
if [ ${nevt} -ne ${fevt} ]; then python adjlheevent.py ${nevt} ${fevt}; fi

cd $LHEWORKDIR

ls -l
echo

exit 0
