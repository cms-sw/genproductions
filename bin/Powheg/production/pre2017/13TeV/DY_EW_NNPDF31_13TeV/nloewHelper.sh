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
SUFFIX=powheg-NLOEW-svn3900-j20

PROCS=(ZToMuMu-8TeV-runtest)
PROCS=(ZToMuMu-7TeV-minnlolike ZToMuMu-7TeV-minnlolike-noew ZToMuMu-7TeV-minnlolike-ewho)
# PROCS=(ZToMuMu-7TeV-minnlolike)
# PROCS=(WplusToMuNu-13TeV-minnlolike)
PROCS=(ZToMuMu-8TeV-minnlolike ZToMuMu-8TeV-minnlolike-noew)
PROCS=(ZToMuMu-13TeV-minnlolike ZToMuMu-13TeV-minnlolike-noew)
# PROCS=(ZToMuMu-7TeV-minnlolike-luxqed ZToMuMu-13TeV-minnlolike-luxqed)
PROCS=(ZToMuMu-7TeV-minnlolike ZToMuMu-7TeV-minnlolike-luxqed-phind)
PROCS=(ZToMuMu-7TeV-minnlolike-m20 ZToMuMu-7TeV-minnlolike-m20-noew)
PROCS=(ZToMuMu-7TeV-minnlolike-qedonly ZToMuMu-7TeV-minnlolike-weakonly)
PROCS=(ZToMuMu-7TeV-minnlolike ZToMuMu-7TeV-minnlolike-noew)
PROCS=(WplusToMuNu-13TeV-minnlolike WplusToMuNu-13TeV-minnlolike-noew WminusToMuNu-13TeV-minnlolike WminusToMuNu-13TeV-minnlolike-noew)

case $WHAT in

    SLC6 )
        cmssw-cc6 --command-to-run ./$0 $PARAM
    ;;
    
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
            python ./run_pwg_condor.py -p 0 -i DY_EW_NNPDF31_13TeV/${PROC}-powheg.input -m ${MPROC} -f ${PROC}-${SUFFIX} -d 1
        done
    ;;

    GRIDS )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 123 -i DY_EW_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-${SUFFIX} -q 1:longlunch,2:workday,3:longlunch --step3pilot -x 3 -j 20 --slc ${ARCH:3:1}
        done
    ;;
    
    XS )
        for PROC in ${PROCS[@]}
        do
            echo ${PROC}
            cat ${PROC}-${SUFFIX}/pwg-st3-0000-stat.dat | grep total
        done
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

    COPY )
        for PROC in ${PROCS[@]}
        do
            MPROC=${procmap[${PROC:0:1}]}
            echo ${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            echo ${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz
            echo ${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz
            cp -p -v ${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
            cp -p -v PACK_REDUCED/${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
            cp -p -v PACK_REDUCED/${MPROC}_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
        done
    ;;

esac
