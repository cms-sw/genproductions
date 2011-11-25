#!/bin/bash

set -o verbose

echo "Starting job on " `date`
echo "Running on " `uname -a`
echo "System release " `cat /etc/redhat-release`

export name=QCD_HT-100To250_8TeV-madgraph
export queue=1nd

# system specific settings, for instance:
# export USE_LSF_STARTER=no

# Release to be used to define the environment and the compiler needed

export PRODHOME=`pwd`
export SCRAM_ARCH=slc5_amd64_gcc434
export RELEASE=CMSSW_4_1_7

# initialize the CMS environment 
# !!! To be set according to the local installation !!!

#source $VO_CMS_SW_DIR/cmsset_default.sh

# location of the madgraph tarball and of the output main storage directory

export SOURCE=${PRODHOME}/MG5v1.3.30_CERN20111121.tgz  

scram project -n ${name}_gridpack CMSSW ${RELEASE} ; cd ${name}_gridpack ; mkdir -p work ; cd work
eval `scram runtime -sh`

# force the f77 compiler to be the CMS defined one

ln -s `which gfortran` f77
ln -s `which gfortran` g77
export PATH=`pwd`:${PATH}

cp ${SOURCE} . ; tar xzf ${SOURCE} ; rm -f `basename ${SOURCE}` ; mv MG5v1.3.30 ${name}_gridpack ; cd ${name}_gridpack

cat Template/Source/run_config.inc | sed -e "s#PBS_QUE = '1nd'#PBS_QUE = '${queue}'#g" > Template/Source/run_config_tmp.inc
mv Template/Source/run_config_tmp.inc Template/Source/run_config.inc

cat Template/bin/run_combine | sed -e "s#-q 1nd#-q ${queue}#g" > Template/bin/run_combine_tmp
mv Template/bin/run_combine_tmp Template/bin/run_combine
chmod +x Template/bin/run_combine

rm Template/Cards/param_card.dat

# set the run cards with the appropriate initial seed

cp ${PRODHOME}/run_card_send.dat Template/Cards/run_card.dat ; dos2unix Template/Cards/run_card.dat

cp ${PRODHOME}/proc_card_mg5_send.dat Template/Cards/proc_card_mg5.dat ; dos2unix Template/Cards/proc_card_mg5.dat

cd Template ; /bin/echo 5 | ./bin/newprocess_mg5 

# Cleaning and recompilation of the CERNLIB for possible left-over in Madgraph5

rm -f lib/libcernlib.a
rm -f Source/CERNLIB/*.o
cd Source/CERNLIB/
make
cd ../..

#find the proper param_card and replace it
model=`grep "import model" Cards/proc_card_mg5.dat | gawk '{print $3}'`
if [ -f Cards/param_card_${model}.dat ]; then
  cp Cards/param_card_${model}.dat Cards/param_card.dat
else
  echo Cards/param_card_${model}.dat not found
  exit 1
fi  

# run the production stage - here you can select for running on multicore or not...

# sequential run
#./bin/generate_events 0 gridpack_${name}

export PATH=`pwd`/bin:${PATH}


# batch run
./bin/generate_events 1 gridpack_${name} gridpack_${name}

# multicore run
#./bin/generate_events 2 3 gridpack_${name}

echo "End of job on " `date`
echo "Will now copy gridpack to local dir"


#srmcp file:///$PWD/gridpack.tar.gz srm://maite.iihe.ac.be:8443/pnfs/iihe/cms/store/user/alkaloge/MadGraph_V5_V1/Tests/7TeV/Gridpacks/7TeV_${name}_grid.tar.gz
exit 0;
