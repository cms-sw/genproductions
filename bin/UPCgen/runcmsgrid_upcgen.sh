#!/bin/sh

fail_exit() { echo "$@"; exit 1; }

extract() { echo $(grep "^$@" ${CONFIG} | cut -d '=' -f 2 | cut -d '#' -f 1 | tr -dc '[.[:digit:]]'); }

set_upcgen_config(){
    cp ${CONFIG} ${CONFIG}.orig
    sed -i '/^NEVENTS.*/d' ${CONFIG} ; echo 'NEVENTS '${nevt} >> ${CONFIG}
    sed -i '/^SEED.*/d' ${CONFIG} ; echo 'SEED '${rnum} >> ${CONFIG}
    sed -i '/^PYTHIA_VERSION.*/d' ${CONFIG} ; echo 'PYTHIA_VERSION 8' >> ${CONFIG}
    sed -i '/^USE_ROOT_OUTPUT.*/d' ${CONFIG} ; echo 'USE_ROOT_OUTPUT 0' >> ${CONFIG}
    sed -i '/^USE_HEPMC_OUTPUT.*/d' ${CONFIG} ; echo 'USE_HEPMC_OUTPUT 1' >> ${CONFIG}
    BeamE=$(echo $(extract "SQRTS")/2 | bc)
}

run_upcgen(){
    echo "*** STARTING UPCGEN PRODUCTION ***"
    cd ${LHEWORKDIR}/${UPCGENDIR}/build
    ./upcgen -debug 0 -nthreads $(nproc) 2>&1 | tee upcgen.log; test ${PIPESTATUS[0]} -eq 0 || fail_exit "upcgen error: exit code not 0"
    ${LHEWORKDIR}/macros/convert_UGHEPMC2LHE events.hepmc ${BeamE} ${BeamE} 2>&1 | tee upcgen.log; test ${PIPESTATUS[0]} -eq 0 || fail_exit "convert_SC2LHE error: exit code not 0"
    sed -i '/UPCGEN/a '${UPCGENDIR} events.lhe
    sed -i 's/--/- -/' ${CONFIG}
    sed -i '/UPCGEN/r'${CONFIG} events.lhe
    mv events.lhe ${LHEWORKDIR}/cmsgrid_final.lhe
    echo "***UPCGEN COMPLETE***"
}

echo "   ______________________________________     "
echo "         Running UPCGen                       "
echo "   ______________________________________     "

nevt=${1}
echo "%MSG-UPCGEN number of events requested = $nevt"

rnum=${2}
echo "%MSG-UPCGEN random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-UPCGEN number of cputs for the run = $ncpu"

LHEWORKDIR=`pwd -P`

use_gridpack_env=true
if [ "$4" = false ]; then
    use_gridpack_env=$4
fi

if [ "$use_gridpack_env" = true ]; then
    if [[ "$5" == *[_]* ]]; then
        scram_arch_version=${5}
    else
        scram_arch_version=SCRAM_ARCH_VERSION_REPLACE
    fi
    echo "%MSG-UPCGEN SCRAM_ARCH version = $scram_arch_version"

    if [[ "$6" == CMSSW_* ]]; then
        cmssw_version=${6}
    else
        cmssw_version=CMSSW_VERSION_REPLACE
    fi
    echo "%MSG-UPCGEN CMSSW version = $cmssw_version"

    export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
    source $VO_CMS_SW_DIR/cmsset_default.sh

    # Make a directory that doesn't overlap
    if [[ -d "${CMSSW_BASE}" ]] && [[ "${LHEWORKDIR}" = "${CMSSW_BASE}"/* ]]; then
        cd ${CMSSW_BASE}/..
        TPD=${PWD}/lhe1t2m3p
        [[ ! -d "${TPD}" ]] && mkdir ${TPD}
        cd ${TPD}
        echo "Changed to: "${TPD}
    fi

    eval `scramv1 unsetenv -sh`
    export SCRAM_ARCH=${scram_arch_version}
    scramv1 project CMSSW ${cmssw_version}
    cd ${cmssw_version}/src
    eval `scramv1 runtime -sh`
fi

cd $LHEWORKDIR

UPCGENDIR=UPCGENDIR_REPLACE
CONFIG=${LHEWORKDIR}/${UPCGENDIR}/build/parameters.in

#Set upcgen settings
set_upcgen_config

#Run upcgen generator
run_upcgen

#Perform test
xmllint --stream --noout ${LHEWORKDIR}/cmsgrid_final.lhe > /dev/null 2>&1; test $? -eq 0 || fail_exit "xmllint integrity check failed on cmsgrid_final.lhe"

#Clean up
[[ -d "${TPD}" ]] && rm -rf ${TPD}

echo "Output ready with cmsgrid_final.lhe at $LHEWORKDIR"
echo "End of job on "`date`
exit 0
