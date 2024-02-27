#!/bin/bash

create_setup () {
    echo "Starting job on " `date` #Only to display the starting of production date
    echo "Running on " `uname -a` #Only to display the machine where the job is running
    echo "System release " `cat /etc/redhat-release` #And the system release
    
    echo "dataset name: ${DATASETNAME}"
    echo "carddir: ${CARDDIR}"
    echo "queue: ${QUEUE}"
    echo "scram_arch: ${SCRAM_ARCH}"
    echo "cmssw_version: ${CMSSW_VERSION}"

    ########################
    #checking input file 
    ########################

    if [ ! -d ${CARDDIR} ]; then
        echo "${CARDDIR} does not exist"
        exit 1
    fi
    
    if [ ! -e ${CARDDIR}/powheg.input ]; then
        echo "${CARDDIR}/powheg.input does not exist"
        exit 1
    fi

    if [ ! -e ${CARDDIR}/process.dat ]; then
        echo "${CARDDIR}/process.dat does not exist. Please provide the Powheg process name in a card called process.dat."
        exit 1
    else 
        PROCESS=`cat ${CARDDIR}/process.dat`
    fi

    WORKDIR=${POWHEGGENPRODDIR}/${DATASETNAME}/${CMSSW_VERSION}/work 
    if [ -d ${POWHEGGENPRODDIR}/${DATASETNAME} ]; then 
        rm -rf ${POWHEGGENPRODDIR}/${DATASETNAME}/*
    else
        mkdir -p ${POWHEGGENPRODDIR}/${DATASETNAME} 
    fi
    cd ${POWHEGGENPRODDIR}/${DATASETNAME}
    #set up environment and working directory 
    export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
    source ${VO_CMS_SW_DIR}/cmsset_default.sh
    scram project -n ${CMSSW_VERSION} CMSSW ${CMSSW_VERSION} 
    cd ${POWHEGGENPRODDIR}/${DATASETNAME}/${CMSSW_VERSION} ; mkdir -p work ; cd work
    WORKDIR=`pwd -P`
    eval `scram runtime -sh`

    #copy relevant scripts and code 
    for TOCOPY in ${CARDDIR} patches Utilities Templates \
        run_pwg_condor.py make_rwl.py \
        runcmsgrid_powheg.sh runcmsgrid_powhegjhugen.sh run_nnlops.sh ; do 
        cp -rp ${POWHEGGENPRODDIR}/${TOCOPY} ${WORKDIR}/.
    done
 }

compile_process(){
    cd ${WORKDIR}
    # compile the powheg process 
    python3 ./run_pwg_condor.py -d 1 -p 0 -i ${POWHEGGENPRODDIR}/${CARDDIR}/powheg.input -m ${PROCESS} -f ${DATASETNAME} 
}

produce_grids(){
    cd ${WORKDIR}
    # run sampling in one step 
    #python2 ./run_pwg_condor.py -d 1 -p 123 -i ${POWHEGGENPRODDIR}/${CARDDIR}/powheg.input -m ${PROCESS} -f ${DATASETNAME} 
    #python2 ./run_pwg_condor.py -d 1 -p ${STEP} -i ${POWHEGGENPRODDIR}/${CARDDIR}/powheg.input -m ${PROCESS} -f ${DATASETNAME}
    python3 ./run_pwg_condor.py -d 1 -p 01239 -i ${POWHEGGENPRODDIR}/${CARDDIR}/powheg.input -m ${PROCESS} -f ${DATASETNAME} -k 1 

}

make_tarball(){
    cd ${WORKDIR}
    python3 ./run_pwg_condor.py -d 1 -p 9 -i ${POWHEGGENPRODDIR}/${CARDDIR}/powheg.input -m ${PROCESS} -f ${DATASETNAME} -k 1
}

# set up internal pathes and variables 
POWHEGGENPRODDIR=`pwd -P` 
# make sure that this is the path from which gridpack_generation.sh is executed 
if [ ! -f ${POWHEGGENPRODDIR}/gridpack_generation.sh ] ; then 
    echo "Cannot locate gridpack_generation.sh in current path"
    exit 1 
fi 

# read in external settings and assign proper default 
# dataset name 
DATASETNAME=${1}
if [ -z ${DATASETNAME} ]; then  
    echo "Please provide a dataset name"
    exit 1 
fi

# card directory 
CARDDIR=${2}
if [ -z ${CARDDIR} ]; then
    echo "Card directory not provided"
    exit 1 
fi

# which queue
QUEUE=${3}
if [ -z ${QUEUE} ]; then QUEUE=pdmv; fi

#check values of queue:
if [ "${QUEUE}" != "pdmv" ] ; then
    echo "Unknown queue=${QUEUE}"
    exit 1
fi 

# processing options
JOBSTEP=${4}
if [ -z ${JOBSTEP} ]; then JOBSTEP=all; fi

#Check values of jobstep:
if [ "${JOBSTEP}" != "all" ] ; then
    echo "Unknown jobstep=${JOBSTEP}"
    exit 1
fi 

# set scram_arch 
if [ -n "$5" ]; then
    SCRAM_ARCH=${5}
else
    # sync default cmssw with the current OS 
    export SYSTEM_RELEASE=`cat /etc/redhat-release`
    if [[ $SYSTEM_RELEASE == *"release 6"* ]]; then 
        SCRAM_ARCH=slc6_amd64_gcc700 
    elif [[ $SYSTEM_RELEASE == *"release 7"* ]]; then 
        SCRAM_ARCH=slc7_amd64_gcc10 
    elif [[ $SYSTEM_RELEASE == *"release 8"* ]]; then
        scram_arch=el8_amd64_gcc10
    else 
        echo "No default scram_arch for current OS"
        exit 1        
    fi
fi
export SCRAM_ARCH=${SCRAM_ARCH}

#set cmssw 
if [ -n "$6" ]; then
    CMSSW_VERSION=${6}
else
    if [[ $SYSTEM_RELEASE == *"release 6"* ]]; then 
        CMSSW_VERSION=CMSSW_10_2_28 
    elif [[ $SYSTEM_RELEASE == *"release 7"* ]]; then 
        CMSSW_VERSION=CMSSW_12_4_8 
    elif [[ $SYSTEM_RELEASE == *"release 8"* ]]; then 
        CMSSW_VERSION=CMSSW_12_4_11
    else 
        echo "No default CMSSW for current OS"
        exit 1        
    fi
fi
export CMSSW_VERSION=${CMSSW_VERSION}

# create gridpack in split steps 
create_setup

# agrohsje 
# due to a bug in current (23/06/22) Powheg setup (missing files when tarball done separately), running in one go 
# compile process  

#run sampling 
#for STEP in 1 2 3 ; do 
produce_grids 
#done 

#create final tarball 
#make_tarball 

#copy tarball to main path
mv ${WORKDIR}/${PROCESS}_${SCRAM_ARCH}_${CMSSW_VERSION}_*.tgz ${POWHEGGENPRODDIR}/.

exit 0 



