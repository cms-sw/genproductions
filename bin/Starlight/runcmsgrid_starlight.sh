#!/bin/sh

fail_exit() { echo "$@"; exit 1; }

extract() { echo $(grep "^$@" ${CONFIG} | cut -d '=' -f 2 | cut -d '#' -f 1 | tr -dc '[.[:digit:]]'); }

read_config(){
    CollE=$(extract "COLL_E")
    Beam1A=$(extract "BEAM_1_A")
    Beam1Z=$(extract "BEAM_1_Z")
    Beam1G=$(extract "BEAM_1_GAMMA")
    Beam2A=$(extract "BEAM_2_A")
    Beam2Z=$(extract "BEAM_2_Z")
    Beam2G=$(extract "BEAM_2_GAMMA")
    GEMin=$(extract "MIN_GAMMA_ENERGY")
    GEMax=$(extract "MAX_GAMMA_ENERGY")
    ProdM=$(extract "PROD_MODE")
    ProdP=$(extract "PROD_PID")
}

get_beam_energy(){
    if [ -n "${Beam1G}" ]; then
        Beam1E=$(echo 0.938272*$Beam1G | bc)
    fi
    if [ -n "${Beam2G}" ]; then
        Beam2E=$(echo 0.938272*$Beam2G | bc)
    fi
    if [ -n "${CollE}" ]; then
        F=$(echo "sqrt(${Beam1A}*${Beam2Z}/${Beam1Z}/${Beam2A})" | bc)
        Beam1E=$(echo $CollE*$F/2 | bc)
        Beam2E=$(echo $CollE/$F/2 | bc)
        Beam1G=$(echo $Beam1E/0.938272 | bc)
        Beam2G=$(echo $Beam2E/0.938272 | bc)
    fi
}

set_starlight_config(){
    cp ${CONFIG} ${CONFIG}.orig
    sed -i '/^BEAM_1_GAMMA.*/d' ${CONFIG} ; echo 'BEAM_1_GAMMA = '${Beam1G} >> ${CONFIG}
    sed -i '/^BEAM_2_GAMMA.*/d' ${CONFIG} ; echo 'BEAM_2_GAMMA = '${Beam2G} >> ${CONFIG}
    sed -i '/^N_EVENTS.*/d' ${CONFIG} ; echo 'N_EVENTS = '${nevt} >> ${CONFIG}
    sed -i '/^RND_SEED.*/d' ${CONFIG} ; echo 'RND_SEED = '${rnum} >> ${CONFIG}
    sed -i '/^PYTHIA_FULL.*/d' ${CONFIG} ; echo 'PYTHIA_FULL_EVENT_RECORD = 0' >> ${CONFIG}
    sed -i '/^OUTPUT_HEADER.*/d' ${CONFIG} ; echo 'OUTPUT_HEADER = 0' >> ${CONFIG}
    sed -i '/^baseFileName.*/d' ${CONFIG} ; echo 'baseFileName = slight' >> ${CONFIG}
}

set_dpmjet_config(){
    #Set minimum and maximum photon energies
    if [ -z "${GEMin}" ]; then
        GEMin=6.0
        printf 'MIN_GAMMA_ENERGY = %.1f\n' "${GEMin}" >> ${CONFIG}
    fi
    if [ -z "${GEMax}" ]; then
        GEMax=600000.0
        printf 'MAX_GAMMA_ENERGY = %.1f\n' "${GEMax}" >> ${CONFIG}
    fi
    #Set DPMJET configuration
    cp ${DPMJETCONFIG} ${DPMJETCONFIG}.orig
    STR=$(printf 'TARPAR         %.1f      %.1f\r' "${Beam1A}" "${Beam1Z}") ; sed -i "s/^TARPAR.*/${STR}/g" ${DPMJETCONFIG}
    STR=$(printf 'ENERGY           %.1f  %.1f\r' "${GEMin}" "${GEMax}") ; sed -i "s/^ENERGY.*/${STR}/g" ${DPMJETCONFIG}
}

run_starlight(){
    echo "*** STARTING STARLIGHT PRODUCTION ***"
    cd ${LHEWORKDIR}/${STARLIGHTDIR}/build
    if [ "$ProdM" -ge 4 ]; then
        ./starlight < my.input 2>&1 | tee slight.log; test $? -eq 0 || fail_exit "starlight error: exit code not 0"
        ${LHEWORKDIR}/macros/convert_SL2LHE slight.out ${Beam1E} ${Beam2E} 0 2>&1 | tee slight.log; test $? -eq 0 || fail_exit "convert_SL2LHE error: exit code not 0"
        sed -i '/STARLIGHT/a '${DPMJETDIR} slight.lhe
    else
        ./starlight 2>&1 | tee slight.log; test $? -eq 0 || fail_exit "starlight error: exit code not 0"
        ${LHEWORKDIR}/macros/convert_SL2LHE slight.out ${Beam1E} ${Beam2E} ${ProdP} 2>&1 | tee slight.log; test $? -eq 0 || fail_exit "convert_SL2LHE error: exit code not 0"
    fi
    sed -i '/STARLIGHT/a '${STARLIGHTDIR} slight.lhe
    sed -i '/STARLIGHT/r'${CONFIG} slight.lhe
    mv slight.lhe ${LHEWORKDIR}/cmsgrid_final.lhe
    echo "***STARLIGHT COMPLETE***"
}

echo "   ______________________________________     "
echo "         Running STARlight                    "
echo "   ______________________________________     "

nevt=${1}
echo "%MSG-STARLIGHT number of events requested = $nevt"

rnum=${2}
echo "%MSG-STARLIGHT random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-STARLIGHT number of cputs for the run = $ncpu"

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
    echo "%MSG-STARLIGHT SCRAM_ARCH version = $scram_arch_version"

    if [[ "$6" == CMSSW_* ]]; then
        cmssw_version=${6}
    else
        cmssw_version=CMSSW_VERSION_REPLACE
    fi
    echo "%MSG-STARLIGHT CMSSW version = $cmssw_version"

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

DPMJETDIR=DPMJETDIR_REPLACE
STARLIGHTDIR=STARLIGHTDIR_REPLACE
CONFIG=${LHEWORKDIR}/${STARLIGHTDIR}/build/slight.in
DPMJETCONFIG=${LHEWORKDIR}/${STARLIGHTDIR}/build/my.input

#Extract parameter settings
read_config

#Compute beam energy
get_beam_energy

#Set STARlight settings
set_starlight_config

#Set DPMJET settings
if [ "$ProdM" -ge 4 ]; then
    set_dpmjet_config
fi

#Run STARlight generator
run_starlight

#Perform test
xmllint --stream --noout ${LHEWORKDIR}/cmsgrid_final.lhe > /dev/null 2>&1; test $? -eq 0 || fail_exit "xmllint integrity check failed on cmsgrid_final.lhe"

#Clean up
[[ -d "${TPD}" ]] && rm -rf ${TPD}

echo "Output ready with cmsgrid_final.lhe at $LHEWORKDIR"
echo "End of job on "`date`
exit 0
