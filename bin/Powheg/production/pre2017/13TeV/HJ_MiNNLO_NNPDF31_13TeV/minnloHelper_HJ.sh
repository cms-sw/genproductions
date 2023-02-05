#!/bin/bash
trap "exit" INT

WHAT=$1;
PARAM=$2;
if [ "$#" -lt 1 ]; then
    echo "minnloHelper_HJ.sh <OPTION>";
    exit 1;
fi

NJOBS=200
SVN=3900
ARCH=slc7_amd64_gcc10
CMSSW=CMSSW_12_3_1
SUFFIX=powheg-MiNNLO31-svn${SVN}-j${NJOBS}

PROCS=(HJ_13TeV-nnpdf31)

case $WHAT in

    SLC6 )
        cmssw-cc6 --command-to-run ./$0 $PARAM
    ;;
    
    INIT )
        ln -s ../../genproductions/bin/Powheg/*.py .
        ln -s ../../genproductions/bin/Powheg/*.sh .
        ln -s ../../genproductions/bin/Powheg/patches .
        ln -s ../../genproductions/bin/Powheg/Templates .
        ln -s ../../genproductions/bin/Powheg/Utilities .
        ln -s ../../genproductions/bin/Powheg/production/pre2017/13TeV/HJ_MiNNLO_NNPDF31_13TeV .
    ;;
    
    PRINT )
        for PROC in ${PROCS[@]}
        do
            echo ${PROC}
        done
    ;;
    
    COMPILE )
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            python ./run_pwg_condor.py -p 0 -i HJ_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m HJ -f ${PROC}-${SUFFIX} -d 1 --svn ${SVN}
        done
    ;;
    
    RECOMPILE )
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}
            cd ${DIR}/POWHEG-BOX/HJ/HJMiNNLO
            make
            cd -
            cp -v ${DIR}/POWHEG-BOX/HJ/HJMiNNLO/pwhg_main ${DIR}/
        done
    ;;
    
    ONESHOT )
        for PROC in ${PROCS[@]}
        do
            python ./run_pwg_condor.py -p f -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m HJ -f ${PROC}-${SUFFIX} -d 1 --svn ${SVN}
        done
    ;;

    GRIDS )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 123 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m HJ -f ${PROC}-${SUFFIX} -q 1:longlunch,2:workday,3:longlunch --step3pilot -x 3 -j ${NJOBS} --slc ${ARCH:3:1}
        done
    ;;
    
    DAG )
        for PROC in ${PROCS[@]}
        do
            if [ ! -f "${PROC}-${SUFFIX}/pwg-st3-0000-stat.dat" ]; then
                condor_submit_dag run_${PROC}-${SUFFIX}.dag
            fi
        done
    ;;
    
    LONGGRIDS )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 123 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m HJ -f ${PROC}-${SUFFIX} -q 1:workday,2:tomorrow,3:longlunch --step3pilot -x 3 -j ${NJOBS}
        done
    ;;
    
    GRIDS1 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 1 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m HJ -f ${PROC}-${SUFFIX} -q workday -j ${NJOBS}
        done
    ;;
    
    GRIDS2 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 2 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m HJ -f ${PROC}-${SUFFIX} -q workday -j ${NJOBS}
        done
    ;;
    
    GRIDS3 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 3 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m HJ -f ${PROC}-${SUFFIX} -q longlunch -j ${NJOBS} --step3pilot &
        done
    ;;
    
    XS )
        for PROC in ${PROCS[@]}
        do
            echo ${PROC}
            cat ${PROC}-${SUFFIX}/pwg-st3-0000-stat.dat | grep total
            cat ${PROC}-${SUFFIX}/pwg-st3-0000-stat.dat | grep suppression
        done
    ;;
    
    PACK )
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            python ./run_pwg_condor.py -p 9 -m HJ -f ${PROC}-${SUFFIX} 
        done
        ./minnloHelper.sh PACK_REDUCED
        ./minnloHelper.sh PACK_NORWL
    ;;
    
    TEST )
        mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            /usr/bin/time -v ./runcmsgrid.sh 20 1 1 &
            cd ..
        done
    ;;
    
    LONGTEST )
        mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            /usr/bin/time -v ./runcmsgrid.sh 800 1 1 &
            cd ..
        done
    ;;
    
    PACK_REDUCED )
        mkdir PACK_REDUCED; cd PACK_REDUCED
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            cp ../../pwg-rwl-reduced.dat pwg-rwl.dat
            tar zcf ../HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz *
            cd ..
        done
    ;;
    
    TEST_REDUCED )
        cd PACK_REDUCED; mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz
            ./runcmsgrid.sh 1000 1 1 &
            cd ..
        done
    ;;
    
    PACK_NORWL )
        mkdir PACK_REDUCED; cd PACK_REDUCED
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}-norwl
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            sed -i '/rwl_file/d' powheg.input
            sed -i '/MINNLO="true"/d' runcmsgrid.sh
            tar zcf ../HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz *
            cd ..
        done
    ;;
    
    TEST_NORWL )
        cd PACK_REDUCED; mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}-norwl
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz
            ./runcmsgrid.sh 5 1 1 &
            cd ..
        done
    ;;
    
    COPY )
        for PROC in ${PROCS[@]}
        do
            echo HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            echo HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz
            echo HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz
            cp -p -v HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
            cp -p -v PACK_REDUCED/HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
            cp -p -v PACK_REDUCED/HJ_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
        done
    ;;
    
    COPY_GRIDS )
        OLDPATH=/afs/cern.ch/work/m/mseidel/generator/CMSSW_10_2_23/src/
        OLDSUFFIX=powheg-MiNNLO31-svn3900-ew-rwl6-j${NJOBS}-st2fix-ana-hoppetweights-ymax20
        for PROC in ${PROCS[@]}
        do
            cp ${OLDPATH}/${PROC}-${OLDSUFFIX}/*.dat ${PROC}-${SUFFIX}/
            cp ${OLDPATH}/${PROC}-${OLDSUFFIX}/*.top ${PROC}-${SUFFIX}/
        done
    ;;

esac
