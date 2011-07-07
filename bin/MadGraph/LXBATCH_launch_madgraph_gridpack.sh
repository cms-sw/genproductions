#!/bin/bash

set -o verbose

echo "Starting job on " `date`
echo "Running on " `uname -a`
echo "System release " `cat /etc/redhat-release`

echo ${1} ${2}

export name=${1} #Put the name corresponding to the needed gridpack in the official repository (without _grid.tar.gz)
export rnum=$2
#export name=W4jets
export RELEASE=CMSSW_4_1_6
#export jobid=${1}
export STOREHOME=/castor/cern.ch/user/l/lenzip/Madgraph/${name}


#retrieve the number of events from CRAB
export nevt=50000

#define the random number generator seed from lumi section for this run
#rnum=`grep 'JobID="'${jobid}'"' $RUNTIME_AREA/arguments.xml | awk -F "\"" '{print $4}' | head -1 `


# retrieve the wanted gridpack from the official repository 
cp /afs/cern.ch/cms/generators/www/slc5_ia32_gcc434/madgraph/V5_1.1/7TeV_Summer11/Gridpacks/${name}_gridpack.tar.gz ./

export SOURCE=`pwd`/${name}_gridpack.tar.gz


scram project -n ${name}_${rnum} CMSSW ${RELEASE} 
cd ${name}_${rnum} 
mkdir -p work  
cd work
eval `scram runtime -sh`


# force the f77 compiler to be the CMS defined one

ln -s `which gfortran` f77
ln -s `which gfortran` g77
export PATH=`pwd`:${PATH}

cp ${SOURCE} . 
tar xzf ${name}_gridpack.tar.gz  
rm -f ${name}_gridpack.tar.gz 
cd madevent

# run the production stage
./bin/compile
./bin/clean4grid


# Cleaning and recompilation of the CERNLIB for possible left-over in Madgraph5

rm -f lib/libcernlib.a
rm -f Source/CERNLIB/*.o
cd Source/CERNLIB/
make
cd ../..





cd ..
./run.sh ${nevt} ${rnum}

gunzip events.lhe.gz
rfmkdir ${STOREHOME}
rfcp events.lhe ${STOREHOME}/events_${name}_nevt${nevt}_seed${rnum}.lhe


echo "End of job on " `date`
exit 0

