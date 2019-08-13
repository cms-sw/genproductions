#!/bin/bash
trap "exit" INT

WHAT=$1;
if [ "$#" -ne 1 ]; then
    echo "dynnlopsHelper.sh <OPTION>";
    exit 1;
fi

PROCS=(WminusJToENu WminusJToMuNu WminusJToTauNu WplusJToENu WplusJToMuNu WplusJToTauNu ZJToEE ZJToMuMu ZJToTauTau)
EPROCS=(WminusJToENu WplusJToENu ZJToEE)
NEPROCS=(WminusJToMuNu WminusJToTauNu WplusJToMuNu WplusJToTauNu ZJToMuMu ZJToTauTau)

case $WHAT in

    INIT )
        ln -s genproductions/bin/Powheg/*.py .
        ln -s genproductions/bin/Powheg/*.sh .
        ln -s genproductions/bin/Powheg/patches .
        ln -s genproductions/bin/Powheg/production/pre2017/13TeV/DYNNLOPS_NNPDF31_13TeV .
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
            python ./run_pwg_condor.py -p 0 -i DYNNLOPS_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS
        done
    ;;
    
    COMPILE_DYNNLO )
        for PROC in ${EPROCS[@]}
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
    
    DYNNLO )
        for PROC in ${EPROCS[@]}
        do
            python ./run_pwg_condor.py -p 7 -i DYNNLOPS_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS -q nextweek -j 100 -e /eos/cms/store/cmst3/group/wmass/DYNNLO &
        done
    ;;
    
    MINLO )
        for PROC in ${EPROCS[@]}
        do
            python ./run_pwg_condor.py -p 8 -i DYNNLOPS_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS -q nextweek -j 1000  -e /eos/user/m/mseidel/Powheg &
        done
    ;;
    
    MERGE_MINLO )
        for PROC in ${EPROCS[@]}
        do
            python ./run_pwg_condor.py -p 88 -i DYNNLOPS_NNPDF31_13TeV/${PROC}-powheg.input -m ${PROC:0:1}j -f ${PROC}-powheg-NNLOPS -e /eos/user/m/mseidel/Powheg
        done
    ;;
    
    COPY_MINLO )
        #OLDDIR=/afs/cern.ch/work/m/mseidel/generator/CMSSW_10_0_5/src/
        #EOSDIR=/eos/user/m/mseidel/Powheg
        for PROC in ${NEPROCS[@]}
        do
            # integration grids
            #cp -n -v ${OLDDIR}/${PROC}-555-powheg/*.dat ${PROC}-powheg-NNLOPS/
            
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
            #cp -n -v ${OLDDIR}/${NNLOTOP}-555-powheg/DYNNLO* ${PROC}-powheg-NNLOPS/
            cp -n -v ${NNLOTOP}-powheg-NNLOPS/MINLO*.top ${PROC}-powheg-NNLOPS/
        done
    ;;
    
    COPY_DYNNLO )
        EOSDIR=/eos/user/m/mseidel/Powheg
        for PROC in ${PROCS[@]}
        do
            # integration grids
            #cp -n -v ${OLDDIR}/${PROC}-555-powheg/*.dat ${PROC}-powheg-NNLOPS/
            
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
            cp -n -v ${EOSDIR}/old/${NNLOTOP}-555-powheg/DYNNLO* ${PROC}-powheg-NNLOPS/
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
            rm -r ${PROC}; mkdir ${PROC}; cd ${PROC}
            tar -xzf ../../${PROC:0:1}j_slc7_amd64_gcc700_CMSSW_10_6_0_${PROC}-powheg-NNLOPS.tgz
            ./runcmsgrid.sh 150 1 1 &
            cd ..
        done
    ;;
    
    PACK_REDUCED )
        mkdir PACK_REDUCED; cd PACK_REDUCED
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            rm -r ${PROC}; mkdir ${PROC}; cd ${PROC}
            tar -xzf ../../${PROC:0:1}j_slc7_amd64_gcc700_CMSSW_10_6_0_${PROC}-powheg-NNLOPS.tgz
            cp ../../pwg-rwl-reduced.dat pwg-rwl.dat
            cp ../../list_nnlo_reduced.txt list_nnlo.txt 
            cp ../../list_minlo_reduced.txt list_minlo.txt
            tar zcf ../${PROC:0:1}j_slc7_amd64_gcc700_CMSSW_10_6_0_${PROC}-powheg-NNLOPS-reducedrwl.tgz *
            cd ..
        done
    ;;
    
    TEST_REDUCED )
        cd PACK_REDUCED; mkdir TEST; cd TEST
        eval `scramv1 runtime -sh`
        for PROC in ${PROCS[@]}
        do
            rm -r ${PROC}; mkdir ${PROC}; cd ${PROC}
            tar -xzf ../../${PROC:0:1}j_slc7_amd64_gcc700_CMSSW_10_6_0_${PROC}-powheg-NNLOPS-reducedrwl.tgz
            ./runcmsgrid.sh 10 1 1 &
            cd ..
        done
    ;;
    

esac
