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
    cp ${PRODDIR}/runcmsgrid_crmc.sh ${WORKDIR}/runcmsgrid.sh
    chmod 755 ${WORKDIR}/runcmsgrid.sh
}

install_crmc(){
    CRMC_VER=v2.2.0
    CRMC=crmc-${CRMC_VER}
    cd ${WORKDIR}

    echo "Downloading "${CRMC}
    CRMCDIR=${WORKDIR}/crmc_v${CRMC//[!0-9]/}
    wget --no-verbose --no-check-certificate https://gitlab.iap.kit.edu/AirShowerPhysics/crmc/-/archive/${CRMC_VER}/${CRMC}.tar.gz
    tar -xzf ${CRMC}.tar.gz && mv ${CRMC} ${CRMCDIR}
    rm -f ${CRMC}.tar.gz

    echo "Compiling ${CRMC}"
    cd ${CRMCDIR}
    source /cvmfs/sft.cern.ch/lcg/views/LCG_104/x86_64-el9-gcc13-opt/setup.sh # using LCG for now, FIXME
    # FIXME - hack to change beam mother index from -1 to 0
    sed -i '1684s/.*/        jmohep(1,nhep)=0/' src/epos/epos-bas.f
    sed -i '1685s/.*/        jmohep(2,nhep)=0/' src/epos/epos-bas.f
    sed -i '1712s/.*/        jmohep(1,nhep)=0/' src/epos/epos-bas.f
    sed -i '1713s/.*/        jmohep(2,nhep)=0/' src/epos/epos-bas.f
    # FIXME - hack to modify comments to fit xml format
    sed -i 's/AB-->/AB->/g' src/epos/epos-bas.f
       
    CMAKE=$([[ $(cmake --version | grep -cE *"n ([3-9]\.)")>0 ]] && echo "cmake" || echo "cmake3")
    ${CMAKE} -S . -B BUILD -DCMAKE_INSTALL_PREFIX=${CRMCDIR}/install -DCRMC_QGSJETIII=ON -DCRMC_SIBYLL=ON -DCRMC_DPMJET19=ON
    ${CMAKE} --build BUILD --target install --parallel $(nproc)
    rm -rf BUILD install


    #Set installation parameters
    sed -i 's/SCRAM_ARCH_VERSION_REPLACE/'${SCRAM_ARCH}'/g' ${WORKDIR}/runcmsgrid.sh
    sed -i 's/CMSSW_VERSION_REPLACE/'${CMSSW_VERSION}'/g' ${WORKDIR}/runcmsgrid.sh
    sed -i 's/GENERATOR_REPLACE/'${generator}'/g' ${WORKDIR}/runcmsgrid.sh
    sed -i 's/CRMCDIR_REPLACE/'${CRMCDIR##*/}'/g' ${WORKDIR}/runcmsgrid.sh
}


make_tarball(){
    #Set tarball name
    TARBALL=crmc_${generator}_${SCRAM_ARCH}_${CMSSW_VERSION}_tarball.tgz

    echo "Creating tarball"
    cd ${WORKDIR}
    tar -czf ${TARBALL} ${CRMCDIR##*/} runcmsgrid.sh
    mv ${WORKDIR}/${TARBALL} ${PRODDIR}/
    echo "Tarball created successfully at ${PRODDIR}/${TARBALL}"
}

#Set main directory
PRODDIR=`pwd`
if [[ ! -f "${PRODDIR}/gridpack_generation.sh" ]] ; then
    echo "Cannot locate gridpack_generation.sh in current path"
    exit 1
fi

#First you need to set the generator:
generator=${1}

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

if [ -z ${generator} ]; then
    echo "generator was not selected"
fi
    
#Create set up
create_setup

#Install crmc
install_crmc

#Create tarball
make_tarball

#Clean up
echo "Removing "${GENDIR}
rm -rf ${GENDIR}

echo "End of job on " `date`
exit 0
