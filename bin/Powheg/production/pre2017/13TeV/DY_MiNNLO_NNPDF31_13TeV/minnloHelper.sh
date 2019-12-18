#!/bin/bash
trap "exit" INT

WHAT=$1;
PARAM=$2;
if [ "$#" -lt 1 ]; then
    echo "minnloHelper.sh <OPTION>";
    exit 1;
fi

#PROCS=(WminusJToENu WminusJToMuNu WminusJToTauNu WplusJToENu WplusJToMuNu WplusJToTauNu ZJToEE ZJToMuMu ZJToTauTau)
PROCS=(ZJToEE ZJToMuMu ZJToTauTau)

case $WHAT in

    INIT )
        ln -s genproductions/bin/Powheg/*.py .
        ln -s genproductions/bin/Powheg/*.sh .
        ln -s genproductions/bin/Powheg/patches .
        ln -s genproductions/bin/Powheg/production/pre2017/13TeV/DY_MiNNLO_NNPDF31_13TeV .
    ;;
    
    PRINT )
        for PROC in ${PROCS[@]}
        do
            echo ${PROC}
        done
    ;;
    
    COMPILE )
        for PROC in ${PROCS[@]}
        do
            python ./run_pwg_condor.py -p 0 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-MiNNLO
        done
    ;;

    GRIDS )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 123 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-MiNNLO -q workday --step3pilot &
        done
    ;;
    
    GRIDS1 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 1 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-MiNNLO -q workday &
        done
    ;;
    
    GRIDS2 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 2 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-MiNNLO -q workday &
        done
    ;;
    
    GRIDS3 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 3 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-MiNNLO -q workday --step3pilot &
        done
    ;;
    
    PACK )
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            python ./run_pwg_condor.py -p 9 -m ${PROC:0:1}j -f ${PROC}-powheg-MiNNLO &
        done
    ;;
    
    TEST )
        mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            rm -r ${PROC}; mkdir ${PROC}-MiNNLO; cd ${PROC}-MiNNLO
            tar -xzf ../../${PROC:0:1}j_slc6_amd64_gcc700_CMSSW_10_2_16_${PROC}-powheg-MiNNLO.tgz
            ./runcmsgrid.sh 200 1 1 &
            cd ..
        done
    ;;
    
    PACK_REDUCED )
        mkdir PACK_REDUCED; cd PACK_REDUCED
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            rm -r ${PROC}; mkdir ${PROC}; cd ${PROC}
            tar -xzf ../../${PROC:0:1}j_slc7_amd64_gcc700_CMSSW_10_6_0_${PROC}-powheg-MiNNLO.tgz
            cp ../../pwg-rwl-reduced.dat pwg-rwl.dat
            cp ../../list_nnlo_reduced.txt list_nnlo.txt 
            cp ../../list_minlo_reduced.txt list_minlo.txt
            tar zcf ../${PROC:0:1}j_slc7_amd64_gcc700_CMSSW_10_6_0_${PROC}-powheg-MiNNLO-reducedrwl.tgz *
            cd ..
        done
    ;;
    
    TEST_REDUCED )
        cd PACK_REDUCED; mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            rm -r ${PROC}; mkdir ${PROC}; cd ${PROC}
            tar -xzf ../../${PROC:0:1}j_slc7_amd64_gcc700_CMSSW_10_6_0_${PROC}-powheg-MiNNLO-reducedrwl.tgz
            ./runcmsgrid.sh 10 1 1 &
            cd ..
        done
    ;;
    

esac
