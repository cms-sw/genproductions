#!/bin/bash

set -o verbose

echo "Starting job on " `date`
echo "Running on " `uname -a`
echo "System release " `cat /etc/redhat-release`

echo ${1} ${2}

#export name=${1} #Put the name corresponding to the needed gridpack in the official repository (without _grid.tar.gz)
export name=ttbarjets

export jobid=${1}

#retrieve the number of events from CRAB
nevt=`grep 'JobID="'${jobid}'"' $RUNTIME_AREA/arguments.xml | awk -F "\"" '{print $2}' | head -1 `
echo "Number of Events for this CRAB job= $nevt"
#define the random number generator seed from lumi section for this run
rnum=`grep 'JobID="'${jobid}'"' $RUNTIME_AREA/arguments.xml | awk -F "\"" '{print $4}' | head -1 `
echo "random seed used for the run= $rnum"

# retrieve the wanted gridpack from the official repository 

wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc5_ia32_gcc434/madgraph/V5_1.1/7TeV_Summer11/Gridpacks/${name}_gridpack.tar.gz 

export SOURCE=`pwd`/${name}_gridpack.tar.gz

# force the f77 compiler to be the CMS defined one

ln -s `which gfortran` f77
ln -s `which gfortran` g77
export PATH=`pwd`:${PATH}

cp ${SOURCE} . ; tar xzf ${name}_gridpack.tar.gz ; rm -f ${name}_gridpack.tar.gz ; cd madevent

# run the production stage
./bin/compile
./bin/clean4grid
cd ..
./run.sh ${nevt} ${rnum}

cp events.lhe.gz ../events.lhe.gz

echo ----------------------------------------
echo NOW THIS IS THE EVENTS DIR
echo ----------------------------------------
ls -ltrh

#Generate the Framework Job Report
cmsRun -j $RUNTIME_AREA/crab_fjr_$NJob.xml -p pset.py

cd ..

echo "End of job on " `date`


