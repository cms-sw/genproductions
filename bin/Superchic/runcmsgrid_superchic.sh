#!/bin/sh

fail_exit() { echo "$@"; exit 1; }

set_superchic_config(){
    cp ${CONFIG} ${CONFIG}.orig
    sed -i "/^\*/! s/.*\[out.*/\'out\'  \!  \[outtg\] for input files/" ${CONFIG}
    sed -i "/^\*/! s/.*\[iseed\].*/${rnum}  \! \[iseed\] : Random number seed/" ${CONFIG}
    sed -i "/^\*/! s/.*\[genunw\].*/\.true\.  \! [genunw] : Generate unweighted events/" ${CONFIG}
    sed -i "/^\*/! s/.*\[nev\].*/${nevt}  \! \[nev\] : Number of events/" ${CONFIG}
    sed -i "/^\*/! s/.*\[erec\].*/\'lhe\'  \! [erec] : Event record format/" ${CONFIG}
}

run_superchic(){
    echo "*** STARTING SUPERCHIC PRODUCTION ***"
    LHAPDF_DATA_PATH=${LHAPDF_DATA_PATH}:${LHEWORKDIR}/${SUPERCHICDIR}/lhapdf
    LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${LHEWORKDIR}/${APFELDIR}/build/lib64
    export SUPERCHIC_DATA_PATH=${LHEWORKDIR}/${SUPERCHICDIR}/build/share/SuperChic
    cd ${LHEWORKDIR}/${SUPERCHICDIR}/build/bin
    ./superchic < input.DAT 2>&1 | tee input.log; test ${PIPESTATUS[0]} -eq 0 || fail_exit "superchic error: exit code not 0"
    ${LHEWORKDIR}/macros/convert_SCLHE2LHE evrecs/evrecout.dat 2>&1 | tee upcgen.log; test ${PIPESTATUS[0]} -eq 0 || fail_exit "convert_SCLHE2LHE error: exit code not 0"
    sed -i '/SUPERCHIC/a '${APFELDIR} evrecout_proc.lhe
    sed -i '/SUPERCHIC/a '${SUPERCHICDIR} evrecout_proc.lhe
    sed -i 's/--/- -/' ${CONFIG}
    sed -i '/SUPERCHIC/r'${CONFIG} evrecout_proc.lhe
    mv evrecout_proc.lhe ${LHEWORKDIR}/cmsgrid_final.lhe
    echo "***SUPERCHIC COMPLETE***"
}

echo "   ______________________________________     "
echo "         Running SuperChic                    "
echo "   ______________________________________     "

nevt=${1}
echo "%MSG-SUPERCHIC number of events requested = $nevt"

rnum=${2}
echo "%MSG-SUPERCHIC random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-SUPERCHIC number of cputs for the run = $ncpu"

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
    echo "%MSG-SUPERCHIC SCRAM_ARCH version = $scram_arch_version"

    if [[ "$6" == CMSSW_* ]]; then
        cmssw_version=${6}
    else
        cmssw_version=CMSSW_VERSION_REPLACE
    fi
    echo "%MSG-SUPERCHIC CMSSW version = $cmssw_version"

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

APFELDIR=APFELDIR_REPLACE
SUPERCHICDIR=SUPERCHICDIR_REPLACE
CONFIG=${LHEWORKDIR}/${SUPERCHICDIR}/build/bin/input.DAT

#Set SuperChic settings
set_superchic_config

#Run SuperChic generator
run_superchic

#Perform test
xmllint --stream --noout ${LHEWORKDIR}/cmsgrid_final.lhe > /dev/null 2>&1; test $? -eq 0 || fail_exit "xmllint integrity check failed on cmsgrid_final.lhe"

#Clean up
[[ -d "${TPD}" ]] && rm -rf ${TPD}

echo "Output ready with cmsgrid_final.lhe at $LHEWORKDIR"
echo "End of job on "`date`
exit 0
