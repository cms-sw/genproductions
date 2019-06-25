#!/bin/bash
trap "exit" INT

WHAT=$1;
if [ "$#" -ne 1 ]; then
    echo "dynnlopsHelper.sh <OPTION>";
    exit 1;
fi

PROCS=(WminusJToENu WminusJToMuNu WminusJToTauNu WplusJToENu WplusJToMuNu WplusJToTauNu ZJToEE ZJToMuMu ZJToTauTau)
#PROCS=(ZJToEE ZJToMuMu ZJToTauTau)
#PROCS=(WplusJToENu-522 WplusJToENu-552)
#PROCS=(WplusJToENu-552)

case $WHAT in

    PRINT )
        for PROC in ${PROCS[@]}
        do
            echo ${PROC}
        done
    ;;
    
    COMPILE )
        for PROC in ${PROCS[@]}
        do
            python ./run_pwg_condor.py -p 0 -i DYNNLOPS_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS
        done
    ;;

    GRIDS )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 123 -i DYNNLOPS_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS -q nextweek --step3pilot &
        done
    ;;
    
    GRIDS1 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 1 -i DYNNLOPS_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS -q tomorrow &
        done
    ;;
    
    GRIDS2 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 2 -i DYNNLOPS_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS -q nextweek &
        done
    ;;
    
    GRIDS3 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 3 -i DYNNLOPS_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS -q tomorrow --step3pilot &
        done
    ;;
    
    COPY_GRIDS )
        OLDDIR=/afs/cern.ch/work/m/mseidel/generator/CMSSW_10_0_5/src/
        for PROC in ${PROCS[@]}
        do
            # integration grids
            cp -n -v ${OLDDIR}/${PROC}-555-powheg/*.dat ${PROC}-powheg-NNLOPS/
            
            # files for NNLO reweighting
            NNLOTOP=none
            if [ ${PROC:0:2} = "Wm" ]; then
                NNLOTOP=WminusJToENu
            fi
            if [ ${PROC:0:2} = "Wp" ]; then
                NNLOTOP=WplusJToENu
            fi
            if [ ${PROC:0:1} = "Z" ]; then
                NNLOTOP=ZJToEE
            fi
            cp -n -v ${OLDDIR}/${NNLOTOP}-555-powheg/DYNNLO* ${PROC}-powheg-NNLOPS/
            cp -n -v ${OLDDIR}/${NNLOTOP}-555-powheg/MINLO*.top ${PROC}-powheg-NNLOPS/
        done
    ;;
    
    PACK )
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            python ./run_pwg_condor.py -p 9 -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS &
        done
    ;;
    
    TEST )
        mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            mkdir ${PROC}; cd ${PROC}
            tar -xzf ../../${PROC:0:1}j_slc7_amd64_gcc700_CMSSW_10_6_0_${PROC}-powheg-NNLOPS.tgz
            ./runcmsgrid.sh 150 1 1 &
            cd ..
        done
    ;;
    

esac
