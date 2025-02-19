#!/bin/bash 

while getopts r:a:p: flag
do
    case "${flag}" in
        r) CMSSW_RELEASE=${OPTARG};;
        a) SCRAM_ARCH=${OPTARG};;
        p) PROC=${OPTARG};;
    esac
done

if [ "$CMSSW_RELEASE" = "" ]; then
    echo "Error: no CMSSW release specified. Use -r CMSSW_X_Y_Z"
    exit 1
fi
if [ "$PROC" = "" ]; then
    echo "Error: no Powheg process specified. Use -p proc"
    exit 1
fi

# define test input for each process
case $PROC in
    hvq) INPUT=production/2017/13TeV/TT_hvq/TT_hdamp_NNPDF31_NNLO_inclusive.input;;
    *) echo "Error: process $PROC not defined in test script"; exit 1;;
esac

POWHEG_DIR="$(dirname $(dirname $(realpath $0)))"
BASE_DIR=/tmp/$USER/test_$PROC

echo "CMSSW release: $CMSSW_RELEASE";
echo "Architecture: $SCRAM_ARCH";
echo "Test process: $PROC";
echo "Test config: $INPUT"
echo "Base directory: $BASE_DIR"
echo "Powheg directory: $POWHEG_DIR"

# setup CMSSW
echo "Setting up $CMSSW_RELEASE in $BASE_DIR"
mkdir -p $BASE_DIR
cd $BASE_DIR
source /cvmfs/cms.cern.ch/cmsset_default.sh
/cvmfs/cms.cern.ch/common/scramv1 project CMSSW  $CMSSW_RELEASE
if [ ! -e $CMSSW_RELEASE ]; then
    echo "Error: no CMSSW area, aborting"
    exit 111
fi
cd $CMSSW_RELEASE/src
eval `/cvmfs/cms.cern.ch/common/scramv1 runtime -sh`
ln -s $POWHEG_DIR/* .
# run Powheg
python3 run_pwg_condor.py -p f -i $INPUT -m $PROC -d 1
cp testProd/pwg-stat.dat test/pwg-stat.dat.${PROC}_${SCRAM_ARCH}_${CMSSW_RELEASE}

GRIDPACK=${PROC}_${SCRAM_ARCH}_${CMSSW_RELEASE}_testProd.tgz
if [ ! -f $GRIDPACK ]; then
    echo "Error: gridpack $GRIDPACK not found"
    exit 1
fi

# rm -rf $CMSSW_RELEASE