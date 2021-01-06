#!/bin/bash
trap "exit" INT

WHAT=$1;
PARAM=$2;
if [ "$#" -lt 1 ]; then
    echo "nloewHelper.sh <OPTION>";
    exit 1;
fi

declare -A procmap
procmap[Z]="Z_ew-BMNNPV"
procmap[W]="W_ew-BMNNP"

ARCH=slc6_amd64_gcc700
CMSSW=CMSSW_10_2_23
SUFFIX=powheg-NLOEW-svn3756-j20

PROCS=(ZToMuMu-8TeV-runtest)


case $WHAT in

    INIT )
        ln -s genproductions/bin/Powheg/*.py .
        ln -s genproductions/bin/Powheg/*.sh .
        ln -s genproductions/bin/Powheg/patches .
        ln -s genproductions/bin/Powheg/production/pre2017/13TeV/DY_EW_NNPDF31_13TeV .
    ;;
    
    PRINT )
        for PROC in ${PROCS[@]}
        do
            MPROC=${procmap[${PROC:0:1}]}
            echo ${PROC}
            echo ${MPROC}
        done
    ;;
    
    COMPILE )
        for PROC in ${PROCS[@]}
        do
            MPROC=${procmap[${PROC:0:1}]}
            cmssw-cc6 --command-to-run python ./run_pwg_condor.py -p 0 -i DY_EW_NNPDF31_13TeV/${PROC}-powheg.input -m ${MPROC} -f ${PROC}-${SUFFIX} -d 1
        done
    ;;

    GRIDS )
        for PROC in ${PROCS[@]}
        do
            MPROC=${procmap[${PROC:0:1}]}
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 123 -i DY_EW_NNPDF31_13TeV/${PROC}-powheg.input -m ${MPROC} -f ${PROC}-${SUFFIX} -q 1:longlunch,2:workday,3:longlunch --step3pilot --slc6 -x 3 -j 20
        done
    ;;
    
    XS )
        for PROC in ${PROCS[@]}
        do
            echo ${PROC}
            cat ${PROC}-${SUFFIX}/pwg-st3-0000-stat.dat | grep total
        done
    ;;
    
    PACK6 )
        cmssw-cc6 --command-to-run ./nloewHelper.sh PACK
    ;;
    
    PACK )
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            MPROC=${procmap[${PROC:0:1}]}
            python ./run_pwg_condor.py -p 9 -m ${MPROC} -f ${PROC}-${SUFFIX} 
        done
        ./nloewHelper.sh PACK_REDUCED
        # ./nloewHelper.sh PACK_NORWL
    ;;
    
    TEST6 )
        cmssw-cc6 --command-to-run ./nloewHelper.sh TEST
    ;;
    
    TEST )
        mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            MPROC=${procmap[${PROC:0:1}]}
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            /usr/bin/time -v ./runcmsgrid.sh 20 1 1 &
            cd ..
        done
    ;;
    
    LONGTEST )
        mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            MPROC=${procmap[${PROC:0:1}]}
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            /usr/bin/time -v ./runcmsgrid.sh 800 1 1 &
            cd ..
        done
    ;;
    
    PACK_REDUCED )
        mkdir PACK_REDUCED; cd PACK_REDUCED
        for PROC in ${PROCS[@]}
        do
            MPROC=${procmap[${PROC:0:1}]}
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            cp ../../pwg-rwl-reduced.dat pwg-rwl.dat
            tar zcf ../${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz *
            cd ..
        done
    ;;
    
    TEST_REDUCED6 )
        cmssw-cc6 --command-to-run ./nloewHelper.sh TEST_REDUCED
    ;;
    
    TEST_REDUCED )
        cd PACK_REDUCED; mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            MPROC=${procmap[${PROC:0:1}]}
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz
            ./runcmsgrid.sh 10 1 1 &
            cd ..
        done
    ;;
    

esac
