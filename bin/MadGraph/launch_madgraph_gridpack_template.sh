#!/bin/bash

#set -o verbose

echo "Starting job on " `date`
echo "Running on " `uname -a`
echo "System release " `cat /etc/redhat-release`

export name=NAME
export nevt=NEVENT
export rnum=RANDOM

# Release to be used to define the environment and the compiler needed

export SCRAM_ARCH=slc5_amd64_gcc434
export RELEASE=CMSSW_4_1_3

# initialize the CMS environment 
# !!! To be set according to the local installation !!!
source $VO_CMS_SW_DIR/cmsset_default.sh

# retrieve the wanted gridpack from the official repository 

#srmcp http:

# location of the madgraph tarball and of the output main storage directory

export SOURCE=`pwd`/${name}_gridpack.tar.gz

scram project -n ${name}_${rnum} CMSSW ${RELEASE} ; cd ${name}_${rnum} ; mkdir -p work ; cd work
eval `scram runtime -sh`

# force the f77 compiler to be the CMS defined one

ln -s `which gfortran` f77
ln -s `which gfortran` g77
export PATH=`pwd`:${PATH}

cp ${SOURCE} . ; tar xzf ${name}_gridpack.tar.gz ; rm -f ${name}_gridpack.tar.gz ; cd madevent

# run the production stage

./bin/compile
rm -f Cards/ident_card.dat ; mv Source/MODEL/ident_card.dat Cards
./bin/clean4grid
cd ..
./run.sh ${nevt} ${rnum}

mv events.lhe.gz ${name}_${rnum}_events.lhe.gz

echo ----------------------------------------
echo NOW THIS IS THE EVENTS DIR
echo ----------------------------------------
ls -ltrh

echo "End of job on " `date`

