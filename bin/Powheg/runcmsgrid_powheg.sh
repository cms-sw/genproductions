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
    echo "%MSG-POWHEG SCRAM_ARCH version = $scram_arch_version"

    if [ -n "$6" ]
      then
        cmssw_version=${6}
      else
        cmssw_version=CMSSW_VERSION_REPLACE
    fi
    echo "%MSG-POWHEG CMSSW version = $cmssw_version"
    export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
    source $VO_CMS_SW_DIR/cmsset_default.sh

    eval `scramv1 unsetenv -sh`
    # Make a directory that doesn't overlap
    if [ ! -d $CMSSW_BASE ] && [[ $PWD == ${CMSSW_BASE}/* ]]; then
        base=`basename $PWD`
        cd ${CMSSW_BASE}/..
        if [ ! -d $base ]; then
            mkdir $base
            cd $base
        fi
    fi


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
# local pdf sets
export LHAPDF_DATA_PATH=$PWD/lhapdf:$LHAPDF_DATA_PATH

# initialize the CMS environment 
myDir=powhegbox_${process}
card=${WORKDIR}/powheg.input

if [[ -e ${myDir} ]]; then
  echo -e "The directory ${myDir} exists! Move the directory to old_${myDir}\n"
  mv ${myDir} old_${myDir}
  mv cmsgrid_final.lhe old_cmsgrid_final.lhe
fi

export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:`pwd`/obj-gfortran/proclib/:${LD_LIBRARY_PATH}
mkdir ${myDir}; cd ${myDir} ;  
export PYTHONPATH=.:${PYTHONPATH}

# force the f77 compiler to be the CMS defined one
#ln -s `which gfortran` f77
#ln -s `which gfortran` g77
export PATH=`pwd`:${PATH}

cp -p ${WORKDIR}/pwg*.dat .

if [ "${process}" == "X0jj" ]; then
    cp -p ${WORKDIR}/MadLoopParams.dat .
    for f in `ls ${WORKDIR}/MG5_aMC_v2_6_7/X0jj/SubProcesses/MadLoop5_resources/*`
    do
	ln -sf $f ./
    done
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
### For the ggHH process
if [[ "${process}" == "ggHH" ]]; then
    ln -s ${WORKDIR}/Virt* .
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

if [ "$process" = "Z_ew-BMNNPV" ] || [ "$process" = "W_ew-BMNNP" ]; then
  # Photos matching removes weights, so we will calculate them afterwards
  sed -i '/rwl_file/d' powheg.input
fi

# Check if we are running with the "manyseeds" option
manyseeds="false"
grep -qFx "manyseeds 1" powheg.input ; test $? -ne 0  || manyseeds="true"

if [ "$manyseeds" == "true" ]; then
  if [ "$produceWeights" == "true" ]; then
    fail_exit "Error: not implemented"
  fi

  # With "manyseeds", powheg reads the seed from pwgseeds.dat
  echo $seed > pwgseeds.dat

  # Powheg expects the index of the seed to use as an command line argument
  # Since we dont actually use the parallelization functionality of Powheg,
  # we just write one seed to pwgseeds.dat and always pick seed 1
  cat powheg.input
  ../pwhg_main iwhichseed=1 2>&1 | tee log_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"

  # Rename the produced LHE file to be what the rest of the script expects
  mv pwgevents-0001.lhe pwgevents.lhe

else
  cat powheg.input
  ../pwhg_main 2>&1 | tee log_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
  
fi


if [ "${process}" == "X0jj" ]; then
    # now run reweighting for X0jj process
    # also need to modify powheg.input inbetween these calls
    cp powheg.input powheg.input.noweight
    sed -nir '/compute_rwgt/!p;$acompute_rwgt 1' powheg.input 

    # sm weight
    sed -nir '/lhrwgt_id/!p;$alhrwgt_id '\''sm_weight'\''' powheg.input 
    sed -nir '/MGcosa/!p;$aMGcosa    1d0' powheg.input 
    ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_sm.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0" 
    mv pwgevents-rwgt.lhe pwgevents.lhe
    
    # ps weight
    sed -nir '/lhrwgt_id/!p;$alhrwgt_id '\''ps_weight'\''' powheg.input 
    sed -nir '/MGcosa/!p;$aMGcosa    0d0' powheg.input 
    ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_ps.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"  
    mv pwgevents-rwgt.lhe pwgevents.lhe

    # mm weight
    sed -nir '/lhrwgt_id/!p;$alhrwgt_id '\''mm_weight'\''' powheg.input 
    sed -nir '/MGcosa/!p;$aMGcosa    -0.707107d0' powheg.input 
    ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_mm.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"   
    mv pwgevents-rwgt.lhe pwgevents.lhe
fi

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
if [[ $(grep -c "</event" pwgevents.lhe) -ne ${nevt} ]]; then
    fail_exit "pwhg_main error: Did not produce expected number of events"
fi
cp powheg.input powheg.input.noweight

MINNLO="false"
grep -q "minnlo 1" powheg.input ; test $? -ne 0 || MINNLO="true"

if [ "$MINNLO" == "true" ]; then
    echo -e "\ncomputing MiNNLO extra weights\n"
    if [ "$process" == "Zj" ]; then
        MASSES=(91.0876 91.0976 91.1076 91.1176 91.1276 91.1376 91.1476 91.1576 91.1676 91.1776 91.1876 91.1976 91.2076 91.2176 91.2276 91.2376 91.2476 91.2576 91.2676 91.2776 91.2876 91.1855 91.1897)
        PSMASS=91.1876
        WIDTHS=(2.49333 2.49493 2.4929 2.4952 2.4975)
    elif [ "$process" == "Wj" ]; then
        MASSES=(80.279 80.289 80.299 80.309 80.319 80.329 80.339 80.349 80.359 80.369 80.379 80.389 80.399 80.409 80.419 80.429 80.439 80.449 80.459 80.469 80.479)
        PSMASS=80.379
        WIDTHS=(2.09053 2.09173 2.043 2.085 2.127)
    fi
    counter=1100
    # Mass
    for MASS in ${MASSES[@]}
    do
        echo -e "\n doing mass ${MASS}\n"
        cp powheg.input.noweight powheg.input
        sed -i '/rwl_file/d' powheg.input
        echo "rwl_file '-'" >> powheg.input
        echo "rwl_add 1" >> powheg.input
        echo "<initrwgt>" >> powheg.input
        echo "<weightgroup name='mass_variation'>" >> powheg.input
        echo "<weight id='${counter}'> mass=${MASS} </weight>" >> powheg.input
        echo "</weightgroup>" >> powheg.input
        echo "</initrwgt>" >> powheg.input
        if [ "$process" == "Zj" ]; then
            echo "Zmass ${MASS}" >> powheg.input
            echo "psZmass ${PSMASS}" >> powheg.input
        elif [ "$process" == "Wj" ]; then
            echo "Wmass ${MASS}" >> powheg.input
            echo "psWmass ${PSMASS}" >> powheg.input
        fi
        
        ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_mass_${MASS}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        
        mv pwgevents-rwgt.lhe pwgevents.lhe
        
        counter=$(( counter + 1 ))
    done
    # Width
    for WIDTH in ${WIDTHS[@]}
    do
        echo -e "\n doing width ${WIDTH}\n"
        cp powheg.input.noweight powheg.input
        sed -i '/rwl_file/d' powheg.input
        echo "rwl_file '-'" >> powheg.input
        echo "rwl_add 1" >> powheg.input
        echo "<initrwgt>" >> powheg.input
        echo "<weightgroup name='width_variation'>" >> powheg.input
        echo "<weight id='${counter}'> width=${WIDTH} </weight>" >> powheg.input
        echo "</weightgroup>" >> powheg.input
        echo "</initrwgt>" >> powheg.input
        if [ "$process" == "Zj" ]; then
            echo "Zwidth ${WIDTH}" >> powheg.input
        elif [ "$process" == "Wj" ]; then
            echo "Wwidth ${WIDTH}" >> powheg.input
        fi
        
        ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_width_${WIDTH}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        
        mv pwgevents-rwgt.lhe pwgevents.lhe
        
        counter=$(( counter + 1 ))
    done
    # Q0
    QS=(2.0 1.0 0.5 0.0)
    for Q in ${QS[@]}
    do
        echo -e "\n doing Q0 ${Q}\n"
        cp powheg.input.noweight powheg.input
        sed -i '/rwl_file/d' powheg.input
        sed -i '/Q0/d' powheg.input
        echo "rwl_file '-'" >> powheg.input
        echo "rwl_add 1" >> powheg.input
        echo "<initrwgt>" >> powheg.input
        echo "<weightgroup name='q0_variation'>" >> powheg.input
        echo "<weight id='${counter}'> Q0=${Q} </weight>" >> powheg.input
        echo "</weightgroup>" >> powheg.input
        echo "</initrwgt>" >> powheg.input
        echo "Q0 ${Q}" >> powheg.input
        
        ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_q0_${Q}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        
        mv pwgevents-rwgt.lhe pwgevents.lhe
        
        counter=$(( counter + 1 ))
    done
    # largeptscales
    echo -e "\n doing largeptscales 1\n"
    cp powheg.input.noweight powheg.input
    sed -i '/rwl_file/d' powheg.input
    sed -i '/largeptscales/d' powheg.input
    echo "rwl_file '-'" >> powheg.input
    echo "rwl_add 1" >> powheg.input
    echo "<initrwgt>" >> powheg.input
    echo "<weightgroup name='largeptscales_variation'>" >> powheg.input
    echo "<weight id='${counter}'> largeptscales=1 </weight>" >> powheg.input
    echo "</weightgroup>" >> powheg.input
    echo "</initrwgt>" >> powheg.input
    echo "largeptscales 1" >> powheg.input

    ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_largeptscales_1.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"

    mv pwgevents-rwgt.lhe pwgevents.lhe

    counter=$(( counter + 1 ))
    # sin2theta
    if [ "$process" == "Zj" ]; then
        SINW2S=(0.23151 0.23154 0.23157 0.2230 0.2300 0.2305 0.2310 0.2315 0.2320 0.2325 0.2330)
        for SINW2 in ${SINW2S[@]}
        do
            echo -e "\n doing sthw2 ${SINW2}\n"
            cp powheg.input.noweight powheg.input
            sed -i '/rwl_file/d' powheg.input
            echo "rwl_file '-'" >> powheg.input
            echo "rwl_add 1" >> powheg.input
            echo "<initrwgt>" >> powheg.input
            echo "<weightgroup name='sthw2_variation'>" >> powheg.input
            echo "<weight id='${counter}'> sthw2=${SINW2} </weight>" >> powheg.input
            echo "</weightgroup>" >> powheg.input
            echo "</initrwgt>" >> powheg.input
            echo "sthw2 ${SINW2}" >> powheg.input
            
            ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_sinw2_${SINW2}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
            
            mv pwgevents-rwgt.lhe pwgevents.lhe
            
            counter=$(( counter + 1 ))
        done
    fi
    # CKM
    if [ "$process" == "Wj" ]; then
        echo -e "\n doing CKM Cabibbo\n"
        cp powheg.input.noweight powheg.input
        sed -i '/rwl_file/d' powheg.input
        echo "rwl_file '-'" >> powheg.input
        echo "rwl_add 1" >> powheg.input
        echo "<initrwgt>" >> powheg.input
        echo "<weightgroup name='ckm_variation'>" >> powheg.input
        echo "<weight id='${counter}'> ckm_cabibbo=1 </weight>" >> powheg.input
        echo "</weightgroup>" >> powheg.input
        echo "</initrwgt>" >> powheg.input
        sed -i "s/CKM_Vud .*/CKM_Vud 0.975d0/g" powheg.input
        sed -i "s/CKM_Vus .*/CKM_Vus 0.222d0/g" powheg.input
        sed -i "s/CKM_Vub .*/CKM_Vub 1d-10/g" powheg.input
        sed -i "s/CKM_Vcd .*/CKM_Vcd 0.222d0/g" powheg.input
        sed -i "s/CKM_Vcs .*/CKM_Vcs 0.975d0/g" powheg.input
        sed -i "s/CKM_Vcb .*/CKM_Vcb 1d-10/g" powheg.input
        sed -i "s/CKM_Vtd .*/CKM_Vtd 1d-10/g" powheg.input
        sed -i "s/CKM_Vts .*/CKM_Vts 1d-10/g" powheg.input
        sed -i "s/CKM_Vtb .*/CKM_Vtb 1d0/g" powheg.input
        
        ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_ckm_cabibbo.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        
        mv pwgevents-rwgt.lhe pwgevents.lhe
        
        counter=$(( counter + 1 ))
        
        
        echo -e "\n doing CKM diagonal\n"
        cp powheg.input.noweight powheg.input
        sed -i '/rwl_file/d' powheg.input
        echo "rwl_file '-'" >> powheg.input
        echo "rwl_add 1" >> powheg.input
        echo "<initrwgt>" >> powheg.input
        echo "<weightgroup name='ckm_variation'>" >> powheg.input
        echo "<weight id='${counter}'> ckm_diagonal=1 </weight>" >> powheg.input
        echo "</weightgroup>" >> powheg.input
        echo "</initrwgt>" >> powheg.input
        sed -i "s/CKM_Vud .*/CKM_Vud 1.000/g" powheg.input
        sed -i "s/CKM_Vus .*/CKM_Vus 1d-10/g" powheg.input
        sed -i "s/CKM_Vub .*/CKM_Vub 1d-10/g" powheg.input
        sed -i "s/CKM_Vcd .*/CKM_Vcd 1d-10/g" powheg.input
        sed -i "s/CKM_Vcs .*/CKM_Vcs 1.000/g" powheg.input
        sed -i "s/CKM_Vcb .*/CKM_Vcb 1d-10/g" powheg.input
        sed -i "s/CKM_Vtd .*/CKM_Vtd 1d-10/g" powheg.input
        sed -i "s/CKM_Vts .*/CKM_Vts 1d-10/g" powheg.input
        sed -i "s/CKM_Vtb .*/CKM_Vtb 1.000/g" powheg.input
        
        ../pwhg_main 2>&1 | tee logrew_${process}_${seed}_ckm_diagonal.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        
        mv pwgevents-rwgt.lhe pwgevents.lhe
        
        counter=$(( counter + 1 ))
    fi
fi

if [ "$process" = "Z_ew-BMNNPV" ] || [ "$process" = "W_ew-BMNNP" ]; then
  ../main-PHOTOS-lhef 2>&1 | tee logphotos_${process}_${seed}.txt; test $? -eq 0 || fail_exit "main-PHOTOS-lhef: exit code not 0"
  mv pwgevents.lhe pwgevents_nophotos.lhe
  mv pwgevents_photos.lhe pwgevents.lhe
  
  echo -e "\n doing rwl\n"
  cp powheg.input.noweight powheg.input
  sed -i '/rwl_file/d' powheg.input
  echo "rwl_file 'pwg-rwl.dat'" >> powheg.input
  echo "rwl_add 1" >> powheg.input
  
  ../pwhg_main 2>&1 | tee logrew_${process}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
  mv pwgevents.lhe pwgevents_photos.lhe
  mv pwgevents-rwgt.lhe pwgevents.lhe
fi

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

   ../pwhg_main 2>&1 | tee logrew_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"   

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

