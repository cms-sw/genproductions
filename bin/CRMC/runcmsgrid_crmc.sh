#!/bin/sh

fail_exit() { echo "$@"; exit 1; }

reinstall_crmc(){
    echo "Compiling CRMC"
    CMAKE=$([[ $(cmake --version | grep -cE *"n ([3-9]\.)")>0 ]] && echo "cmake" || echo "cmake3")
    cd ${LHEWORKDIR}/${CRMCDIR}
    source /cvmfs/sft.cern.ch/lcg/views/LCG_104/x86_64-el9-gcc13-opt/setup.sh # using LCG for now, FIXME
    sed -i 's/AB-->/AB->/g'  ${LHEWORKDIR}/${CRMCDIR}/src/epos/epos-bas.f #FIXME
    ${CMAKE} -S . -B BUILD -DCMAKE_INSTALL_PREFIX=${LHEWORKDIR}/${CRMCDIR}/build -DCRMC_QGSJETIII=ON -DCRMC_SIBYLL=ON -DCRMC_DPMJET19=ON
    ${CMAKE} --build BUILD --target install --parallel $(nproc)
}

run_crmc(){
    echo "*** STARTING PRODUCTION ***"
    cd ${LHEWORKDIR}/${CRMCDIR}/build/bin
    cp ${LHEWORKDIR}/${CRMCDIR}/build/etc/crmc.param .
    sed -i "s/MinDecayLength  1./MinDecayLength  100./" crmc.param
    
    generator=GENERATOR_REPLACE
    if [ "$generator" = 'eposlhcr' ]; then
      ./crmc -m 0 -i 1 -p 6800 -I 80160 -P -3400 -o lhe -n $nevt -s $rnum -f cmsgrid_final.lhe
    elif [ "$generator" = 'sibyll' ]; then
      ./crmc -m 6 -i 1 -p 6800 -I 80160 -P -3400 -o lhe -n $nevt -s $rnum -f cmsgrid_final.lhe
    elif [ "$generator" = 'dpmjetIII.19' ]; then
      ./crmc -m 12 -i 1 -p 6800 -I 80160 -P -3400 -o lhe -n $nevt -s $rnum -f cmsgrid_final.lhe
    elif [ "$generator" = 'qgsjetIII' ]; then
      echo unpack qgsdat-III.lzma
      xz --format=lzma --decompress ${LHEWORKDIR}/${CRMCDIR}/build/share/crmc/qgsdat-III.lzma
      ./crmc -m 13 -i 1 -p 6800 -I 80160 -P -3400 -o lhe -n $nevt -s $rnum -f cmsgrid_final.lhe
    fi
    mv cmsgrid_final.lhe ${LHEWORKDIR}/cmsgrid_final.lhe
    echo "***MC GENERATION COMPLETED***"
}

echo "   ______________________________________     "
echo "         Running crmc                    "
echo "   ______________________________________     "

nevt=${1}
echo "%MSG-CRMC number of events requested = $nevt"

rnum=${2}
echo "%MSG-CRMC random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-CRMC number of cputs for the run = $ncpu"

LHEWORKDIR=`pwd -P`

use_gridpack_env=false # using LCG for now, FIXME
if [ "$4" = false ]; then
    use_gridpack_env=$4
fi

if [ "$use_gridpack_env" = true ]; then
    if [[ "$5" == *[_]* ]]; then
        scram_arch_version=${5}
    else
        scram_arch_version=SCRAM_ARCH_VERSION_REPLACE
    fi
    echo "%MSG-CRMC SCRAM_ARCH version = $scram_arch_version"

    if [[ "$6" == CMSSW_* ]]; then
        cmssw_version=${6}
    else
        cmssw_version=CMSSW_VERSION_REPLACE
    fi
    echo "%MSG-CRMC CMSSW version = $cmssw_version"

    export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
    source $VO_CMS_SW_DIR/cmsset_default.sh

    eval `scramv1 unsetenv -sh`
    export SCRAM_ARCH=${scram_arch_version}
    scramv1 project CMSSW ${cmssw_version}
    cd ${cmssw_version}/src
    eval `scramv1 runtime -sh`
fi

cd $LHEWORKDIR

CRMCDIR=CRMCDIR_REPLACE

#Install CRMC generator locally
reinstall_crmc

#Run CRMC generator
run_crmc

#Perform test
xmllint --stream --noout ${LHEWORKDIR}/cmsgrid_final.lhe > /dev/null 2>&1; test $? -eq 0 || fail_exit "xmllint integrity check failed on cmsgrid_final.lhe"

echo "Output ready with cmsgrid_final.lhe at $LHEWORKDIR"
echo "End of job on "`date`
exit 0
