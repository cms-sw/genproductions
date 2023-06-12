#!/bin/sh

create_setup(){
    echo "Using scram_arch: ${SCRAM_ARCH}"
    echo "Using cmssw_version: ${CMSSW_VERSION}"

    GENDIR=${PRODDIR}/tarball
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
    cp ${PRODDIR}/runcmsgrid_starlight.sh ${WORKDIR}/runcmsgrid.sh
    chmod 755 ${WORKDIR}/runcmsgrid.sh
}

install_starlight(){
    DPMJET=dpmjet3.0-5
    STARLIGHT=starlight_r317
    cd ${WORKDIR}

    echo "Downloading "${DPMJET}
    export DPMJETDIR=${WORKDIR}/dpmjet_v${DPMJET//[!0-9]/}
    wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/starlight/${DPMJET}.tar
    tar -xf ${DPMJET}.tar && mv ${DPMJET} ${DPMJETDIR}
    rm -f ${DPMJET}.tar

    echo "Downloading "${STARLIGHT}
    STARLIGHT_VER=${STARLIGHT//[!0-9]/}
    STARLIGHTDIR=${WORKDIR}/starlight_v${STARLIGHT_VER}
    wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/starlight/${STARLIGHT}.tar
    mkdir -p ${STARLIGHTDIR} && tar -xf ${STARLIGHT}.tar -C ${STARLIGHTDIR}
    [[ -d "${STARLIGHTDIR}/trunk" ]] && mv ${STARLIGHTDIR}/trunk/* ${STARLIGHTDIR} && rm -rf ${STARLIGHTDIR}/trunk
    rm -f ${STARLIGHT}.tar

    echo "Patching "${DPMJET}" and "${STARLIGHT}
    patch -ufZs -p1 -i ${PRODDIR}/patches/dpmjet.patch -d ${DPMJETDIR}
    patch -ufZs -p1 -i ${PRODDIR}/patches/starlight_default.patch -d ${STARLIGHTDIR}
    patch -ufZs -p1 -i ${PRODDIR}/patches/starlight_pythia.patch -d ${STARLIGHTDIR}
    if [ "$STARLIGHT_VER" -lt 317 ]; then
        patch -ufZs -p1 -i ${PRODDIR}/patches/starlight_randomgenerator.patch -d ${STARLIGHTDIR}
    fi

    echo "Compiling ${DPMJET}"
    cd ${DPMJETDIR}
    rm -f fpe.o
    make -j8

    echo "Compiling ${STARLIGHT}"
    cd ${STARLIGHTDIR}
    mkdir -p build && cd build
    cp $DPMJETDIR/dpmjet.dat ./
    cp ${STARLIGHTDIR}/config/my.input ./
    export PYTHIADIR=$(scram tool tag pythia8 PYTHIA8_BASE)
    cmake ${STARLIGHTDIR} -DENABLE_DPMJET=ON -DENABLE_PYTHIA=ON
    make -j8

    echo "Compiling macros"
    cp -r ${PRODDIR}/macros ${WORKDIR}/
    cd ${WORKDIR}/macros/
    make -j8

    #Set installation parameters
    sed -i 's/SCRAM_ARCH_VERSION_REPLACE/'${SCRAM_ARCH}'/g' ${WORKDIR}/runcmsgrid.sh
    sed -i 's/CMSSW_VERSION_REPLACE/'${CMSSW_VERSION}'/g' ${WORKDIR}/runcmsgrid.sh
    sed -i 's/STARLIGHTDIR_REPLACE/'${STARLIGHTDIR##*/}'/g' ${WORKDIR}/runcmsgrid.sh
    sed -i 's/DPMJETDIR_REPLACE/'${DPMJETDIR##*/}'/g' ${WORKDIR}/runcmsgrid.sh
}

make_tarball(){
    #Set tarball name
    CONFIG=${1}
    prefix=${CONFIG##*/} ; prefix=${prefix%%.*} ; prefix=${prefix#*starlight_}
    TARBALL=starlight_${prefix}_${SCRAM_ARCH}_${CMSSW_VERSION}_tarball.tgz

    echo "Creating tarball"
    cd ${WORKDIR}
    cp ${CONFIG} ${STARLIGHTDIR}/build/slight.in
    tar -czf ${TARBALL} ${STARLIGHTDIR##*/} ${DPMJETDIR##*/} macros runcmsgrid.sh
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
        CMSSW_VERSION=CMSSW_13_0_7
    elif [[ $SYSTEM_RELEASE == *"release 8"* ]]; then
        CMSSW_VERSION=CMSSW_13_0_7
    else
        echo "No default CMSSW for current OS"
        exit 1
    fi
fi
export CMSSW_VERSION=${CMSSW_VERSION}

#Create set up
create_setup

#Install STARlight
install_starlight

#Create tarball
make_tarball ${INPUTFILE}

#Clean up
echo "Removing "${GENDIR}
rm -rf ${GENDIR}

echo "End of job on " `date`
exit 0
