#!/bin/sh

fail_exit() { echo "$@"; exit 1; }

create_setup(){
    echo "Using scram_arch: ${SCRAM_ARCH}"
    echo "Using cmssw_version: ${CMSSW_VERSION}"

    GENDIR=${PRODDIR}/tarball_$RANDOM
    [[ -d "${GENDIR}" ]] && rm -rf ${GENDIR}
    mkdir -p ${GENDIR} && cd ${GENDIR}

    #Set up environment and working directory 
    export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
    source ${VO_CMS_SW_DIR}/cmsset_default.sh
    scram project -n ${CMSSW_VERSION} CMSSW ${CMSSW_VERSION} 
    cd ${CMSSW_VERSION} && mkdir -p work && cd work
    eval `scram runtime -sh`
    WORKDIR=`pwd -P`

    #Copy relevant scripts and code
    cp ${PRODDIR}/runcmsgrid_superchic.sh ${WORKDIR}/runcmsgrid.sh
    chmod 755 ${WORKDIR}/runcmsgrid.sh
}

install_superchic(){
    SUPERCHIC=SuperChic-5.1
    cd ${WORKDIR}

    echo "Downloading "${SUPERCHIC}
    SUPERCHICDIR=${WORKDIR}/superchic_v${SUPERCHIC//[!0-9]/}
    wget --no-verbose --no-check-certificate https://cms-project-generators.web.cern.ch/cms-project-generators/superchic/${SUPERCHIC}.tar.gz
    tar -xzf ${SUPERCHIC}.tar.gz && mv ${SUPERCHIC} ${SUPERCHICDIR}
    rm -f ${SUPERCHIC}.tar.gz

    echo "Compiling ${SUPERCHIC}"
    cd ${SUPERCHICDIR}
    CMAKE=$([[ $(cmake --version | grep -cE *"n ([3-9]\.)")>0 ]] && echo "cmake" || echo "cmake3")
    ${CMAKE} -S . -B BUILD -DCMAKE_INSTALL_PREFIX=${SUPERCHICDIR}/build -DLHAPDF_DIR=$(scram tool tag lhapdf LHAPDF_BASE) -DSUPERCHIC_ENABLE_TESTS=OFF -DSUPERCHIC_ENABLE_FPES=OFF -DSUPERCHIC_ENABLE_DOCS=OFF -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_Fortran_FLAGS="-O2 -g -ffree-line-length-512 -Wno-unused-label -Wno-integer-division -Wno-conversion -Wno-function-elimination"
    ${CMAKE} --build BUILD --target install --parallel $(nproc)
    rm -rf BUILD share

    echo "Compiling macros"
    cp -r ${PRODDIR}/macros ${WORKDIR}/
    cd ${WORKDIR}/macros/
    make -j $(nproc)

    #Set installation parameters
    sed -i 's/SCRAM_ARCH_VERSION_REPLACE/'${SCRAM_ARCH}'/g' ${WORKDIR}/runcmsgrid.sh
    sed -i 's/CMSSW_VERSION_REPLACE/'${CMSSW_VERSION}'/g' ${WORKDIR}/runcmsgrid.sh
    sed -i 's/SUPERCHICDIR_REPLACE/'${SUPERCHICDIR##*/}'/g' ${WORKDIR}/runcmsgrid.sh
}

init_superchic(){
    LHAPDF_DATA_PATH=${LHAPDF_DATA_PATH}:${SUPERCHICDIR}/build/share/SuperChic/SF
    cd ${SUPERCHICDIR}/build/bin/
    cp ${INPUTFILE} input.DAT
    ./init < input.DAT 2>&1 | tee init.log; test ${PIPESTATUS[0]} -eq 0 || fail_exit "superchic error: exit code not 0"
}

make_tarball(){
    #Set tarball name
    prefix=${INPUTFILE##*/} ; prefix=${prefix%%.*} ; prefix=${prefix#*superchic_}
    TARBALL=superchic_${prefix}_${SCRAM_ARCH}_${CMSSW_VERSION}_tarball.tgz

    echo "Creating tarball"
    cd ${WORKDIR}
    tar -czf ${TARBALL} ${SUPERCHICDIR##*/} macros runcmsgrid.sh
    mv ${WORKDIR}/${TARBALL} ${PRODDIR}/
    echo "Tarball created successfully at ${PRODDIR}/${TARBALL}"
}

#Set main directory
PRODDIR=`pwd`
if [[ ! -f "${PRODDIR}/gridpack_generation.sh" ]] ; then
    echo "Cannot locate gridpack_generation.sh in current path"
    exit 1
fi

#Set input file
INPUTFILE=${1}
if ! [[ "${INPUTFILE}" = "${PRODDIR}"* ]]; then
    INPUTFILE=${PRODDIR}/${1}
fi
if [[ ! -f "${INPUTFILE}" ]]; then
    echo "Configuration file ${INPUTFILE} is not valid"
    exit 1
fi
echo "Configuration file: ${INPUTFILE}"

#Set scram_arch
if [ -n "$2" ]; then
    SCRAM_ARCH=${2}
else
    # sync default cmssw with the current OS
    export SYSTEM_RELEASE=`cat /etc/redhat-release`
    if [[ $SYSTEM_RELEASE == *"release 6"* ]]; then
        SCRAM_ARCH=slc6_amd64_gcc700
    elif [[ $SYSTEM_RELEASE == *"release 7"* ]]; then
        SCRAM_ARCH=slc7_amd64_gcc11
    elif [[ $SYSTEM_RELEASE == *"release 8"* ]]; then
        SCRAM_ARCH=el8_amd64_gcc11
    elif [[ $SYSTEM_RELEASE == *"release 9"* ]]; then
        SCRAM_ARCH=el9_amd64_gcc11
    else
        echo "No default scram_arch for current OS"
        exit 1
    fi
fi
export SCRAM_ARCH=${SCRAM_ARCH}

#Set cmssw
if [ -n "$3" ]; then
    CMSSW_VERSION=${3}
else
    if [[ $SYSTEM_RELEASE == *"release 6"* ]]; then
        CMSSW_VERSION=CMSSW_10_3_5
    elif [[ $SYSTEM_RELEASE == *"release 7"* ]]; then
        CMSSW_VERSION=CMSSW_13_0_18
    elif [[ $SYSTEM_RELEASE == *"release 8"* ]]; then
        CMSSW_VERSION=CMSSW_13_0_18_HeavyIon
    elif [[ $SYSTEM_RELEASE == *"release 9"* ]]; then
        CMSSW_VERSION=CMSSW_13_0_18
    else
        echo "No default CMSSW for current OS"
        exit 1
    fi
fi
export CMSSW_VERSION=${CMSSW_VERSION}

#Create set up
create_setup

#Install SuperChic
install_superchic

#Initialize SuperChic
init_superchic

#Create tarball
make_tarball

#Clean up
echo "Removing "${GENDIR}
rm -rf ${GENDIR}

echo "End of job on " `date`
exit 0
