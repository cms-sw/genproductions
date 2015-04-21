#!/bin/bash

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`
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