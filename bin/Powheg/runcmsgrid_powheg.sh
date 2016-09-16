#!/bin/bash

fail_exit() { echo "$@"; exit 1; }

#set -o verbose
EXPECTED_ARGS=3

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` Nevents RandomSeed cpu"
    echo "Example: `basename $0` 1000 1212 cpu" 
    exit 1
fi

echo "   ______________________________________     "
echo "         Running Powheg                       "
echo "   ______________________________________     "

nevt=${1}
echo "%MSG-POWHEG number of events requested = $nevt"

rnum=${2}
echo "%MSG-POWHEG random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-POWHEG number of cputs for the run = $ncpu"

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

if [[ ! -e ${card} ]]; then
 fail_exit "powheg.input not found!"
fi

cat ${card} | sed -e "s#SEED#${seed}#g" | sed -e "s#NEVENTS#${nevt}#g" > powheg.input

# Check if the powheg.input file contains the proper settings to calculate weights                                                                                                                           
produceWeights="true" 
grep -q "storeinfo_rwgt 1" powheg.input ; test $? -eq 0  || produceWeights="false"
grep -q "pdfreweight 1" powheg.input ; test $? -eq 0 || produceWeights="false"

if [ "$produceWeights" == "true" ];
then
    cp -p powheg.input powheg.input.orig
    cat <<'EOF' >> powheg.input

lhrwgt_id 'c'
lhrwgt_descr 'muR=0.10000E+01 muF=0.10000E+01'
lhrwgt_group_name 'scale_variation'
lhrwgt_group_combine 'envelope'
EOF
else
    echo -e "\nWarning!! The output will not contain weights!\n"
    produceWeights="false"
fi


cat powheg.input
../pwhg_main &> log_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"

if [ "$produceWeights" == "true" ];
then 
    cp -p pwgevents.lhe pwgevents.lhe.orig
    sed 's/storeinfo_rwgt 1/storeinfo_rwgt 0/g' powheg.input.orig > powheg.input.tmp
    echo -e "\ncompute_rwgt 1\n" >> powheg.input.tmp
    grep -q "storeinfo_rwgt 0" powheg.input.tmp ; test $? -eq 0 || fail_exit "Weights will be re-written!"
    grep -q "compute_rwgt 1" powheg.input.tmp ; test $? -eq 0 || fail_exit "Weights are not calculated!"

    echo -e "\ncomputing weights for 9 scale variations\n"
    iteration=0
    lastfile=2
    array=(1 2 0.5)
    counter=1000
    while [ $iteration -le $lastfile ];
    do
        scale1=${array[$iteration]}
        
        iter=0
        last=2
        while [ $iter -le $last ];
        do
            scale2=${array[$iter]}
            rm -rf powheg.input          
            
            echo -e "\n doing scale ${scale1}, ${scale2}\n"
            sed -e 's/.*renscfact.*/renscfact '$scale1'd0/ ; s/.*facscfact.*/facscfact '$scale2'd0/' powheg.input.tmp > powheg.input

            counter=$(( counter + 1 ))
            echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
            echo -e "lhrwgt_descr 'muR=${scale1} muF=${scale2}'" >> powheg.input
            echo -e "lhrwgt_group_name 'scale_variation'" >> powheg.input
            echo -e "lhrwgt_group_combine 'envelope'" >> powheg.input
    
            ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
            mv pwgevents-rwgt.lhe pwgevents.lhe
            mv powheg.input powheg.input.${scale1}_${scale2}
            iter=$(( iter + 1 ))
        done

        iteration=$(( iteration + 1 ))
   done


    echo -e "\ncomputing weights for 100 NNPDF3.0 nlo variations\n"
    iteration=260001
    lastfile=260100
    counter=2000
    while [ $iteration -le $lastfile ];
    do
        echo -e "\n PDF set ${iteration}"
        sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input

        counter=$(( counter + 1 ))
        echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
        echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
        echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
        echo -e "lhrwgt_group_combine 'gaussian'" >> powheg.input

        ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        mv pwgevents-rwgt.lhe pwgevents.lhe
        mv powheg.input powheg.input.${iteration}
        iteration=$(( iteration + 1 ))
    done


    echo -e "\ncomputing weights for NNPDF 3.0 nlo alphas=0.117 variation\n"
    iteration=265000
    echo -e "\n PDF set ${iteration}"
    sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input
    counter=$(( counter + 1 ))
    echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
    echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
    echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
    echo -e "lhrwgt_group_combine 'gaussian'" >> powheg.input

    ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
    mv pwgevents-rwgt.lhe pwgevents.lhe
    mv powheg.input powheg.input.${iteration}


    echo -e "\ncomputing weights for NNPDF 3.0 nlo alphas=0.119 variation\n"
    iteration=266000
    echo -e "\n PDF set ${iteration}"
    sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input
    counter=$(( counter + 1 ))
    echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
    echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
    echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
    echo -e "lhrwgt_group_combine 'gaussian'" >> powheg.input

    ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
    mv pwgevents-rwgt.lhe pwgevents.lhe
    mv powheg.input powheg.input.${iteration}


    echo -e "\ncomputing weights for 56+1 CT14 nlo PDF variations\n"
    iteration=13100
    lastfile=13156
    counter=3000
    while [ $iteration -le $lastfile ];
    do
        echo -e "\n PDF set ${iteration}"
        sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input
        counter=$(( counter + 1 ))
        echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
        echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
        echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
        echo -e "lhrwgt_group_combine 'hessian'" >> powheg.input

        ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        mv pwgevents-rwgt.lhe pwgevents.lhe
        mv powheg.input powheg.input.${iteration}
        iteration=$(( iteration + 1 ))
    done

    echo -e "\ncomputing weights for CT14 nlo alphas=0.117 variation\n"
    iteration=13164
    echo -e "\n PDF set ${iteration}"
    sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input
    counter=$(( counter + 1 ))
    echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
    echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
    echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
    echo -e "lhrwgt_group_combine 'hessian'" >> powheg.input

    ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
    mv pwgevents-rwgt.lhe pwgevents.lhe
    mv powheg.input powheg.input.${iteration}

    echo -e "\ncomputing weights for CT14 nlo alphas=0.119 variation\n"
    iteration=13166
    echo -e "\n PDF set ${iteration}"
    sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input
    counter=$(( counter + 1 ))
    echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
    echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
    echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
    echo -e "lhrwgt_group_combine 'hessian'" >> powheg.input

    ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
    mv pwgevents-rwgt.lhe pwgevents.lhe
    mv powheg.input powheg.input.${iteration}
 
    echo -e "\ncomputing weights for CT10 nlo central values\n"
    iteration=11000
    echo -e "\n PDF set ${iteration}"
    sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input
    counter=$(( counter + 1 ))
    echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
    echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
    echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
    echo -e "lhrwgt_group_combine 'hessian'" >> powheg.input

    ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
    mv pwgevents-rwgt.lhe pwgevents.lhe
    mv powheg.input powheg.input.${iteration}
 

    echo -e "\ncomputing weights for 50+1 MMHT2014nlo68clas118 PDF variations\n"
    iteration=25200
    lastfile=25250
    counter=4000
    while [ $iteration -le $lastfile ];
    do
        echo -e "\n PDF set ${iteration}"
        sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input
        counter=$(( counter + 1 ))
        echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
        echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
        echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
        echo -e "lhrwgt_group_combine 'hessian'" >> powheg.input

        ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        mv pwgevents-rwgt.lhe pwgevents.lhe
        mv powheg.input powheg.input.${iteration}
        iteration=$(( iteration + 1 ))
    done


    echo -e "\ncomputing weights for 5 MMHT2014nlo68cl 5 alphas variations\n"
    iteration=25260
    lastfile=25264
    while [ $iteration -le $lastfile ];
    do
        echo -e "\n PDF set ${iteration}"
        sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input
        counter=$(( counter + 1 ))
        echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
        echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
        echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
        echo -e "lhrwgt_group_combine 'hessian'" >> powheg.input

        ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        mv pwgevents-rwgt.lhe pwgevents.lhe
        mv powheg.input powheg.input.${iteration}
        iteration=$(( iteration + 1 ))
    done

    echo -e "\ncomputing weights for 56+1 CT14 nnlo PDF variations\n"
    iteration=13000
    lastfile=13056
    counter=5000
    while [ $iteration -le $lastfile ];
    do
        echo -e "\n PDF set ${iteration}"
        sed -e 's/.*lhans1.*/lhans1 '$iteration'/ ; s/.*lhans2.*/lhans2 '$iteration'/' powheg.input.tmp > powheg.input
        counter=$(( counter + 1 ))
        echo -e "\nlhrwgt_id '${counter}'" >> powheg.input
        echo -e "lhrwgt_descr 'PDF set = ${iteration}'" >> powheg.input
        echo -e "lhrwgt_group_name 'PDF_variation'" >> powheg.input
        echo -e "lhrwgt_group_combine 'hessian'" >> powheg.input

        ../pwhg_main &>> reweightlog_${process}_${seed}.txt; test $? -eq 0 || fail_exit "pwhg_main error: exit code not 0"
        mv pwgevents-rwgt.lhe pwgevents.lhe
        mv powheg.input powheg.input.${iteration}
        iteration=$(( iteration + 1 ))
    done


    rm -rf powheg.input*
    sed -e "/#new weight/d" -e "/<wgt id='c'>/d" -e "/<weight id='c'>/d" pwgevents.lhe > pwgevents.lhe.tmp
    mv pwgevents.lhe.tmp pwgevents.lhe 
    echo -e "\n finished computing weights ..\n" 
fi

cat pwgevents.lhe | grep -v "Random number generator exit values" > pwgevents.lhe.bkp
mv pwgevents.lhe.bkp pwgevents.lhe

xmllint --noout pwgevents.lhe > /dev/null 2>&1; test $? -eq 0 || fail_exit "xmllint integrity check failed on pwgevents.lhe"

cp pwgevents.lhe ${file}_final.lhe

ls -l ${file}_final.lhe
sed -i 's/Input file powheg.input contained:/Process: '$process'\nInput file powheg.input contained:/g' ${file}_final.lhe
pwd

if [ -s pwgstat.dat ]; then
  mv pwgstat.dat pwg-stat.dat
fi

if [ -s pwg-stat.dat ]; then
  XSECTION=`cat pwg-stat.dat | grep total | awk '{print $7}'`
  XSECUNC=` cat pwg-stat.dat | grep total | awk '{print $9}'`
  head=`cat   cmsgrid_final.lhe | grep -in "<init>" | sed "s@:@ @g" | awk '{print $1+1}' | tail -1`
  tail=`wc -l cmsgrid_final.lhe | awk -v tmp="$head" '{print $1-2-tmp}'`
  tail -${tail} cmsgrid_final.lhe                           >  cmsgrid_final.lhe_tail
  head -${head} cmsgrid_final.lhe                           >  cmsgrid_final.lhe_F
  echo "  "$XSECTION"   "$XSECUNC"  1.00000000000E-00 10001" >>  cmsgrid_final.lhe_F
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
