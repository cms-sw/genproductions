#exit on first error
set -e

#First you need to set couple of settings:

# process name
name=${1}

# the configuration card
card=${2}

# number of cpu cores 
ncores=${3}

# sync default cmssw with the current OS 
export SYSTEM_RELEASE=`cat /etc/redhat-release`

# set scram_arch 
if [ -n "$4" ]; then
    scram_arch=${4}
else
    if [[ $SYSTEM_RELEASE == *"release 7"* ]]; then 
        scram_arch=slc7_amd64_gcc700 
    elif [[ $SYSTEM_RELEASE == *"release 8"* ]]; then
        scram_arch=el8_amd64_gcc10
    elif [[ $SYSTEM_RELEASE == *"release 9"* ]]; then
        # no production release yet: https://cmssdt.cern.ch/SDT/cgi-bin/ReleasesXML
        scram_arch=el9_amd64_gcc11
    else 
        echo "No default scram_arch for current OS!"
        if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi        
    fi
fi

#set cmssw 
if [ -n "$5" ]; then
    cmssw_version=${5}
else
    if [[ $SYSTEM_RELEASE == *"release 7"* ]]; then 
        cmssw_version=CMSSW_10_6_38
    elif [[ $SYSTEM_RELEASE == *"release 8"* ]]; then
        cmssw_version=CMSSW_12_4_18
    elif [[ $SYSTEM_RELEASE == *"release 9"* ]]; then
        # no production release yet: https://cmssdt.cern.ch/SDT/cgi-bin/ReleasesXML
	    cmssw_version=CMSSW_13_2_9
    else 
        echo "No default CMSSW for current OS!"
        if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi        
    fi
fi


if [ -z "$PRODHOME" ]; then
    PRODHOME=`pwd`
fi 

if [ ! -z ${CMSSW_BASE} ]; then
    echo "Error: This script must be run in a clean environment as it sets up CMSSW itself.  You already have a CMSSW environment set up for ${CMSSW_VERSION}."
    echo "Please try again from a clean shell."
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
fi

#catch unset variables
set -u

if [ -z ${card} ]; then
    echo "Card not provided"
fi

if [ -z ${name} ]; then
    echo "Process name not provided"
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
fi

function set_env() {
    echo "[sherpack generation] set_env"
    export REALCARD_PATH=`realpath ${card}`

    export SCRAM_ARCH=${scram_arch}
    export RELEASE=${cmssw_version}

    ############################
    #Create a workplace to work#
    ############################
    export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
    set +u
    source $VO_CMS_SW_DIR/cmsset_default.sh
    set -u    

    scram project -n ${name}_workdir CMSSW ${RELEASE} ;
    if [ ! -d ${name}_workdir ]; then  
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then echo "yes here"; return 1; else exit 1; fi
    fi

    cd ${name}_workdir/src
    eval `scram runtime -sh`

    export TOPDIR=$PWD
    git cms-addpkg GeneratorInterface/SherpaInterface
    scram b

    mkdir -p MY/PROJECT/test/
    mkdir -p MY/PROJECT/python/ 
    cd MY/PROJECT/test/ 
    cp $TOPDIR/GeneratorInterface/SherpaInterface/data/*SherpaLibs.sh .
    chmod +x *SherpaLibs.sh

    cp ${REALCARD_PATH} Run.dat_${name}
}

function mkLibs() {
    echo "[sherpack generation] mkLibs"
    pushd $TOPDIR
    eval `scram runtime -sh`
    pushd MY/PROJECT/test/
    ./MakeSherpaLibs.sh -p ${name} -o LBCR -v -m mpirun -M '-n ${ncores}'
    popd
    popd
}

function mkPack() {
    echo "[sherpack generation] mkPack"
    pushd $TOPDIR
    eval `scram runtime -sh`
    pushd MY/PROJECT/test/
    ./PrepareSherpaLibs.sh -p ${name}
    cd ${PRODHOME}
    mkdir ${name}_sherpack
    cp $TOPDIR/MY/PROJECT/test/sherpa_${name}_MASTER* ${name}_sherpack
    cp $TOPDIR/MY/PROJECT/test/Run.dat_${name} ${name}_sherpack
}

echo "-------------------------------------------------------"
echo "---------- sherpack generation configuration ----------"
echo "-------------------------------------------------------"
#!/bin/bash

printf "%20s %-8s\n" "Process name:" ${name}
printf "%20s %-8s\n" "Input Run.dat:" `realpath ${card}`
printf "%20s %-8s\n" "Number of cores:" ${ncores}
printf "%20s %-8s\n" "SCRAM Arch:" ${scram_arch}
printf "%20s %-8s\n" "CMSSW version:" ${cmssw_version}

echo "-------------------------------------------------------"
echo "---------- sherpack generation configuration ----------"
echo "-------------------------------------------------------"
set_env
mkLibs
mkPack
