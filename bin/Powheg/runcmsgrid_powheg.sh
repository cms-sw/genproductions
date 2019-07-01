#!/bin/bash

fail_exit() { echo "$@"; exit 1; }

# #set -o verbose
# EXPECTED_ARGS=3

# if [ $# -ne $EXPECTED_ARGS ]
# then
    # echo "Usage: `basename $0` Nevents RandomSeed cpu"
    # echo "Example: `basename $0` 1000 1212 cpu" 
    # exit 1
# fi

echo "   ______________________________________     "
echo "         Running Powheg                       "
echo "   ______________________________________     "

nevt=${1}
echo "%MSG-POWHEG number of events requested = $nevt"

rnum=${2}
echo "%MSG-POWHEG random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-POWHEG number of cputs for the run = $ncpu"


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

process="PROCESS"

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
myDir=powhegbox_${process}
card=${WORKDIR}/powheg.input

if [[ -e ${myDir} ]]; then
  echo -e "The directory ${myDir} exists! Move the directory to old_${myDir}\n"
  mv ${myDir} old_${myDir}
  mv cmsgrid_final.lhe old_cmsgrid_final.lhe
fi

export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH}
mkdir ${myDir}; cd ${myDir} ;  
export PYTHONPATH=.:${PYTHONPATH}

# force the f77 compiler to be the CMS defined one
#ln -s `which gfortran` f77
#ln -s `which gfortran` g77
export PATH=`pwd`:${PATH}

if [[ -e ${WORKDIR}/pwggrid.dat ]]; then
    cp -p ${WORKDIR}/pwg*.dat .
fi
if [ -e  ${WORKDIR}/vbfnlo.input ]; then
    cp -p ${WORKDIR}/vbfnlo.input .
fi
if [ -e ${WORKDIR}/br.a3_2HDM ]; then
  cp -p ${WORKDIR}/br*2HDM .
fi
if [ -e  ${WORKDIR}/powheg-fh.in ]; then
  cp -p ${WORKDIR}/powheg-fh.in .
fi
### For the W process
if [ -e  ${WORKDIR}/cteq6m ]; then
    cp -p ${WORKDIR}/cteq6m .
fi
### For the bb4l process
if [[ -d ${WORKDIR}/obj-gfortran ]]; then
    ln -s ${WORKDIR}/obj-gfortran .
    cp -p ${WORKDIR}/pwg*.dat .
fi
### For the WW process
if [[ -d ${WORKDIR}/WW_MATRIX ]]; then
    ln -s ${WORKDIR}/WW_MATRIX .
    ln -s ${WORKDIR}/WW_MINLO .
    cp -p ${WORKDIR}/binvalues-WW.top .
fi
### For DYNNLOPS
if [[ -e ${WORKDIR}/DYNNLO_mur1_muf1_3D.top ]]; then
    ln -s ${WORKDIR}/DYNNLO*.top .
    ln -s ${WORKDIR}/MINLO*.top .
    ln -s ${WORKDIR}/list*.txt .
fi
### For the ggHH process
if [[ -e ${WORKDIR}/Virt_full_cHHH_0.0.grid ]]; then
    ln -s ${WORKDIR}/Virt_full_cHHH_* .
    ln -s ${WORKDIR}/creategrid.py .
    cp -p ${WORKDIR}/events.cdf .
fi

if [[ ! -e ${card} ]]; then
 fail_exit "powheg.input not found!"
fi

cat ${card} | sed -e "s#SEED#${seed}#g" | sed -e "s#NEVENTS#${nevt}#g" > powheg.input

# Check if the powheg.input file contains the proper settings to calculate weights

produceWeights="false" 

grep -q "storeinfo_rwgt 1" powheg.input ; test $? -eq 0  || produceWeights="false"
grep -q "pdfreweight 1" powheg.input ; test $? -eq 0 || produceWeights="false"
grep -q "first runx" powheg.input ; test $? -ne 0 || produceWeights="true"

cat powheg.input
../pwhg_main &> log_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"

if [ "$produceWeightsNNLO" == "true" ]; then
    echo -e "\ncomputing weights for NNLOPS\n"
    mv pwgevents.lhe fornnlops
    cp ../nnlopsreweighter.input .
    cp ../HNNLO-11.top .
    cp ../HNNLO-22.top .
    cp ../HNNLO-0505.top .
    ../nnlopsreweighter
    mv fornnlops.nnlo pwgevents.lhe
fi

sed -e "/#new weight/d" -e "/<wgt id='c'>/d" -e "/<weight id='c'>/d" pwgevents.lhe > pwgevents.lhe.tmp
mv pwgevents.lhe.tmp pwgevents.lhe 
cp powheg.input powheg.input.noweight

if [ "$produceWeights" == "true" ]; then

   echo "   ______________________________________     "
   echo "           Running HV_ew reweight             "
   echo "   ______________________________________     "

   cp pwgfullgrid-reg-00*.dat pwgfullgrid-reg.dat
   echo "rwl_add 1" >> powheg.input
   echo "rwl_group_events 2000" >> powheg.input
   echo "lhapdf6maxsets 50" >> powheg.input
   echo "rwl_file 'pwg-rwl.dat'" >> powheg.input
   echo "rwl_format_rwgt 1" >> powheg.input
   sed -i -e "s#select_EW#\#select_EW#g" powheg.input
   echo "select_EW_virt 1" >> powheg.input

   ../pwhg_main &> logrew_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"   

   cat pwgevents-rwgt.lhe | grep -v "Random number generator exit values" > ${file}_final.lhe
else 
   cat pwgevents.lhe | grep -v "Random number generator exit values" > ${file}_final.lhe
fi

rm -rf powheg.input*

echo -e "\n finished computing weights ..\n" 

xmllint --stream --noout ${file}_final.lhe > /dev/null 2>&1; test $? -eq 0 || fail_exit "xmllint integrity check failed on pwgevents.lhe"

grep ">        NaN</wgt>" ${file}_final.lhe; test $? -ne 0 || fail_exit "Weights equal to NaN found, there must be a problem in the reweighting"

# uncomment to avoid problems in production as whole job would fail and problem traced back to neg. PDFs 
# grep ">0.00000E+00</wgt>" ${file}_final.lhe; test $? -ne 0 || fail_exit "Weights equal to 0 found, there must be a problem in the reweighting"

ls -l ${file}_final.lhe
sed -i 's/Input file powheg.input contained:/Process: '$process'\nInput file powheg.input contained:/g' ${file}_final.lhe
pwd

if [ -s pwgstat.dat ]; then
  mv pwgstat.dat pwg-stat.dat
fi

if [ -s pwg-stat.dat ]; then
  if [ "$process" = "b_bbar_4l" ] || [ "$process" = "HWJ_ew" ] || [ "$process" = "HW_ew" ] || [ "$process" = "HZJ_ew" ] || [ "$process" = "HZ_ew" ]; then
    XSECTION=`tac pwg-stat.dat | grep total\ total | awk '{ print $(NF-2) }'`
    XSECUNC=` tac pwg-stat.dat | grep total\ total | awk '{ print $(NF) }'`
  else
    XSECTION=`tac pwg-stat.dat | grep -m1 in\ pb | awk '{ print $(NF-2) }'`
    XSECUNC=` tac pwg-stat.dat | grep -m1 in\ pb | awk '{ print $(NF) }'`
  fi
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
