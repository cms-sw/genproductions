#!/bin/bash
trap "exit" INT

WHAT=$1;
PARAM=$2;
if [ "$#" -lt 1 ]; then
    echo "minnloHelper.sh <OPTION>";
    exit 1;
fi

NJOBS=200
SVN=3900
ARCH=slc7_amd64_gcc10
CMSSW=CMSSW_12_3_1
SUFFIX=powheg-MiNNLO31-svn${SVN}-ew-rwl6-j${NJOBS}-st2fix-ana-hoppetweights-ymax20-pdf3

# PROCS=(ZJToMuMu-suggested-nnpdf31-ncalls-doublefsr-q139 WplusJToMuNu-suggested-nnpdf31-ncalls-doublefsr-q139-ckm WminusJToMuNu-suggested-nnpdf31-ncalls-doublefsr-q139-ckm)

PROCS=(ZJToMuMu-5TeV-suggested-nnpdf31-ncalls-doublefsr-q139 WplusJToMuNu-5TeV-suggested-nnpdf31-ncalls-doublefsr-q139-ckm WminusJToMuNu-5TeV-suggested-nnpdf31-ncalls-doublefsr-q139-ckm)
PROCS=(ZJToMuMu-7TeV-suggested-nnpdf31-ncalls-doublefsr-q139)

#SUFFIX=powheg-MiNNLO31-svn${SVN}-ew-rwl6-j${NJOBS}-st2fix-ana-hoppetweights-ymax20-addmassweights
PROCS=(ZJToMuMu-suggested-nnpdf31-ncalls-doublefsr-q139 WplusJToMuNu-suggested-nnpdf31-ncalls-doublefsr-q139-ckm WminusJToMuNu-suggested-nnpdf31-ncalls-doublefsr-q139-ckm)

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
        ln -s ../../genproductions/bin/Powheg/production/pre2017/13TeV/DY_MiNNLO_NNPDF31_13TeV .
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
            python ./run_pwg_condor.py -p 0 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-${SUFFIX} -d 1 --svn ${SVN}
        done
    ;;
    
    RECOMPILE )
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}
            cd ${DIR}/POWHEG-BOX/${PROC:0:1}j/${PROC:0:1}jMiNNLO
            make
            cd -
            cp -v ${DIR}/POWHEG-BOX/${PROC:0:1}j/${PROC:0:1}jMiNNLO/pwhg_main ${DIR}/
        done
    ;;
    
    ONESHOT )
        for PROC in ${PROCS[@]}
        do
            python ./run_pwg_condor.py -p f -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-${SUFFIX} -d 1 --svn ${SVN}
        done
    ;;

    GRIDS )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 123 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-${SUFFIX} -q 1:longlunch,2:workday,3:longlunch --step3pilot -x 3 -j ${NJOBS} --slc ${ARCH:3:1}
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
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 123 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-${SUFFIX} -q 1:workday,2:tomorrow,3:longlunch --step3pilot -x 3 -j ${NJOBS}
        done
    ;;
    
    GRIDS1 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 1 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-${SUFFIX} -q workday -j ${NJOBS}
        done
    ;;
    
    GRIDS2 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 2 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-${SUFFIX} -q workday -j ${NJOBS}
        done
    ;;
    
    GRIDS3 )
        for PROC in ${PROCS[@]}
        do
            k5reauth -R -- python ./run_pwg_parallel_condor.py -p 3 -i DY_MiNNLO_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-${SUFFIX} -q longlunch -j ${NJOBS} --step3pilot &
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
            cd ${PROC}-${SUFFIX}
            git clone https://gitlab.cern.ch/cms-wmass/lhapdf.git
            cd -
            python ./run_pwg_condor.py -p 9 -m ${PROC:0:1}j -f ${PROC}-${SUFFIX} 
        done
    ;;
    
    TEST )
        mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
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
            tar -xzf ../../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
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
            tar -xzf ../../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            cp ../../pwg-rwl-reduced.dat pwg-rwl.dat
            rm -rf lhapdf
            tar zcf ../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz *
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
            tar -xzf ../../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz
            ./runcmsgrid.sh 10 1 1 &
            cd ..
        done
    ;;
    
    PACK_LEP )
        mkdir PACK_LEP; cd PACK_LEP
        for PROC in ${PROCS[@]}
        do
            for LEP in E Tau
            do
                NEWPROC=${PROC//Mu/$LEP}
                DIR=${NEWPROC}-${SUFFIX}
                rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
                tar -xzf ../../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
                if [ "$LEP" == "E" ]; then
                    LEPID=1
                elif [ "$LEP" == "Tau" ]; then
                    LEPID=3
                fi
                sed -i "s/vdecaymode .*/vdecaymode ${LEPID}/g" powheg.input
                tar zcf ../${PROC:0:1}j_${ARCH}_${CMSSW}_${NEWPROC}-${SUFFIX}.tgz *
                cd ..
            done
        done
    ;;
    
    COPY_LEP )
        cd PACK_LEP
        for PROC in ${PROCS[@]}
        do
            for LEP in E Tau
            do
                NEWPROC=${PROC//Mu/$LEP}
                cp -p -v ${PROC:0:1}j_${ARCH}_${CMSSW}_${NEWPROC}-${SUFFIX}.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
            done
        done
    ;;
    
    PACK_NORWL )
        mkdir PACK_REDUCED; cd PACK_REDUCED
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}-norwl
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            sed -i '/rwl_file/d' powheg.input
            sed -i '/MINNLO="true"/d' runcmsgrid.sh
            rm -rf lhapdf
            tar zcf ../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz *
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
            tar -xzf ../../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz
            ./runcmsgrid.sh 5 1 1 &
            cd ..
        done
    ;;
    
    COPY )
        for PROC in ${PROCS[@]}
        do
            echo ${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            echo ${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz
            echo ${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz
            cp -p -v ${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
            cp -p -v PACK_REDUCED/${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-reducedrwl.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
            cp -p -v PACK_REDUCED/${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-norwl.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
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
    
    MAKE_MASSRWGT )
        mkdir PACK_MASSRWGT; cd PACK_MASSRWGT
        for PROC in ${PROCS[@]}
        do
            DIR=${PROC}-${SUFFIX}
            rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
            tar -xzf ../../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}.tgz
            PROCSPLIT=(`echo ${PROC} | tr "-" "\n"`)
            cp ../../DY_MiNNLO_NNPDF31_13TeV/lheWriter_cfg.py .
            sed -i "s/ZJToMuMu/${PROCSPLIT[0]}/g" lheWriter_cfg.py
            diff ../../DY_MiNNLO_NNPDF31_13TeV/lheWriter_cfg.py lheWriter_cfg.py
            cp ../../DY_MiNNLO_NNPDF31_13TeV/runcmsgrid_addMassWeights.sh runcmsgrid.sh
            sed -i "s/process=.*/process=\"${PROC:0:1}j\"/g" runcmsgrid.sh
            sed -i s/SCRAM_ARCH_VERSION_REPLACE/${ARCH}/g runcmsgrid.sh
            sed -i s/CMSSW_VERSION_REPLACE/${CMSSW}/g runcmsgrid.sh
            diff ../../DY_MiNNLO_NNPDF31_13TeV/runcmsgrid_addMassWeights.sh runcmsgrid.sh
            tar zcf ../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-addmassweights.tgz *
            cd ..
        done
    ;;
    
    MAKE_MASSRWGT_LEP )
        mkdir PACK_MASSRWGT; cd PACK_MASSRWGT
        for PROC in ${PROCS[@]}
        do
            for LEP in E Tau
            do
                NEWPROC=${PROC//Mu/$LEP}
                DIR=${NEWPROC}-${SUFFIX}
                rm -r ${DIR}; mkdir ${DIR}; cd ${DIR}
                tar -xzf ../${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX}-addmassweights.tgz
                if [ "$LEP" == "E" ]; then
                    LEPID=1
                elif [ "$LEP" == "Tau" ]; then
                    LEPID=3
                fi
                sed -i "s/vdecaymode .*/vdecaymode ${LEPID}/g" powheg.input
                PROCSPLIT=(`echo ${PROC} | tr "-" "\n"`)
                NEWPROCSPLIT=(`echo ${NEWPROC} | tr "-" "\n"`)
                sed -i "s/${PROCSPLIT[0]}/${NEWPROCSPLIT[0]}/g" lheWriter_cfg.py
                diff ../../DY_MiNNLO_NNPDF31_13TeV/lheWriter_cfg.py lheWriter_cfg.py
                diff ../../DY_MiNNLO_NNPDF31_13TeV/runcmsgrid.sh runcmsgrid.sh
                tar zcf ../${PROC:0:1}j_${ARCH}_${CMSSW}_${NEWPROC}-${SUFFIX}-addmassweights.tgz *
                cd ..
            done
        done
    ;;
    
    COPY_MASSRWGT )
        cd PACK_MASSRWGT
        for PROC in ${PROCS[@]}
        do
            for LEP in E Mu Tau
            do
                NEWPROC=${PROC//Mu/$LEP}
                cp -p -v ${PROC:0:1}j_${ARCH}_${CMSSW}_${NEWPROC}-${SUFFIX}-addmassweights.tgz /afs/cern.ch/work/m/mseidel/public/MiNNLO-gridpacks/
            done
        done
    ;;
    
    PDF2_PDF3 )
        for PROC in ${PROCS[@]}
        do
            mkdir ${PROC}-${SUFFIX}; cd ${PROC}-${SUFFIX}
            tar -xzf /cvmfs/cms.cern.ch/phys_generator/gridpacks/slc7_amd64_gcc10/13TeV/powheg/Vj_MiNNLO/${PROC:0:1}j_${ARCH}_${CMSSW}_${PROC}-${SUFFIX//-pdf3/}.tgz
            cp ../pwg-rwl.dat .
            cd -
        done
        echo "Now run PACK"
    ;;

esac
