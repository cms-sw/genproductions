#!/bin/bash

# scram_arch_version=${1}
scram_arch_version=slc6_amd64_gcc481
# cmssw_version=${2}
cmssw_version=CMSSW_7_1_28

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`
export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
source $VO_CMS_SW_DIR/cmsset_default.sh
export SCRAM_ARCH=${scram_arch_version}
scramv1 project CMSSW ${cmssw_version}
cd ${cmssw_version}/src
eval `scramv1 runtime -sh`
cd $LHEWORKDIR

basename=BASENAME

cd $LHEWORKDIR/$basename

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


sed "s@EVTS@$nevt@g" input_gen.DAT > input_g0.DAT
sed "s@XRAN@$rnum@g" input_g0.DAT  > input.DAT
./mcfm
#Replace the negative so pythia will work
#sed "s@-1000022@1000022@g" FILENAME > $LHEWORKDIR/cmsgrid_final.lhe
mv FILENAME $LHEWORKDIR/cmsgrid_final.lhe
cd $LHEWORKDIR

ls -l
echo

exit 0