#!/bin/bash

#set -o verbose

echo "Starting job on " `date`
echo "Running on " `uname -a`
echo "System release " `cat /etc/redhat-release`

#First you need to set couple of settings:

# name of the run
name=${1}

# which queue
queue=$2

#________________________________________
# to be set for user spesific
# Release to be used to define the environment and the compiler needed

# working directory
AFSFOLD=/afs/cern.ch/user/a/alkaloge/www/8TeV_Winter12/${name}

AFS_GEN_FOLDER=/afs/cern.ch/cms/CAF/CMSPHYS/PHYS_GENERATOR/alkaloge/${name}
#AFS_GEN_FOLDER=/afs/cern.ch/work/a/alkaloge/${name}
#AFS_GEN_FOLDER=/tmp/alkaloge/${name}

if [ ! -d ${AFS_GEN_FOLDER} ];then
mkdir ${AFS_GEN_FOLDER}
fi
cd $AFS_GEN_FOLDER

export PRODHOME=`pwd`

export SCRAM_ARCH=slc5_amd64_gcc434
export RELEASE=CMSSW_4_1_8_patch4

#export SCRAM_ARCH=slc5_amd64_gcc462
#export RELEASE=CMSSW_5_1_2


# initialize the CMS environment 
export VO_CMS_SW_DIR=/afs/cern.ch/cms
source $VO_CMS_SW_DIR/cmsset_default.sh

# clean the area the working area
if [  -d ${name}_gridpack ] ;then
	rm -rf ${name}_gridpack
fi
#________________________________________
# location of the madgraph tarball

#HTTP_DOWNLOAD="https://cms-project-generators.web.cern.ch/cms-project-generators/slc5_ia32_gcc434/madgraph/V5_1.1/7TeV_Summer11"
HTTP_DOWNLOAD="https://cms-project-generators.web.cern.ch/cms-project-generators/slc5_ia32_gcc434/madgraph/V5_1.4.8/"
#HTTP_DOWNLOAD="http://mon.iihe.ac.be/~alkaloge/Files/"
#wget --no-check-certificate  ${HTTP_DOWNLOAD}/MG5v1.3.30_CERN20111121.tgz
wget --no-check-certificate  ${HTTP_DOWNLOAD}/MG5v1.4.8_CERN_21082012.tar.gz
wget --no-check-certificate http://mon.iihe.ac.be/~alkaloge/8TeV/Cards/${name}_proc_card_mg5.dat
wget --no-check-certificate http://mon.iihe.ac.be/~alkaloge/8TeV/Cards/${name}_run_card.dat


#________________________________________
# Create a workplace to work

scram project -n ${name}_gridpack CMSSW ${RELEASE} ; cd ${name}_gridpack ; mkdir -p work ; cd work
eval `scram runtime -sh`

# force the f77 compiler to be the CMSSW defined ones
ln -s `which gfortran` f77
ln -s `which gfortran` g77
export PATH=`pwd`:${PATH}

# Copy, Unzip and Delete the MadGraph tarball.
MG=MG5v1.4.8_CERN_21082012.tar.gz 
MGSOURCE=${AFS_GEN_FOLDER}/${MG}

mv ${MGSOURCE} . ; tar xzf ${MG} ; cd MG5v1.4.8

#________________________________________
# Some settings for cern caf batch job submission

cat ${AFS_GEN_FOLDER}/${name}_run_card.dat | sed 's/  .false.     = gridpack  !True = setting up the grid pack/  .true.     = gridpack  !True = setting up the grid pack/g' > Template/Cards/run_card.dat

sed -i 's/100000       = nevents ! Number of unweighted events requested/1000       = nevents ! Number of unweighted events requested/g'  Template/Cards/run_card.dat

cp ${AFS_GEN_FOLDER}/${name}_proc_card_mg5.dat Template/Cards/proc_card_mg5.dat

mv Template/bin/internal/addmasses.py Template/bin/internal/addmasses.py.no

#________________________________________
# set the run cards with the appropriate initial seed

cd Template ; /bin/echo 5 | ./bin/newprocess_mg5 

#________________________________________
# modify the cluster to run on lsf with custom queue

sed -e 's/run_mode = 0/run_mode = 1/g' -e 's/cluster_type = condor/cluster_type = lsf/g'  -e 's/cluster_queue = madgraph/cluster_queue = '${queue}'/g' Cards/me5_configuration.txt > me5_temp ; mv me5_temp Cards/me5_configuration.txt
#sed -e 's/run_mode = 0/run_mode = 1/g'   -e 's/cluster_queue = madgraph/cluster_queue = '${queue}'/g' Cards/me5_configuration.txt > me5_temp ; mv me5_temp Cards/me5_configuration.txt


#________________________________________
# Cleaning and recompilation of the CERNLIB for possible left-over in Madgraph5

#rm -f lib/libcernlib.a
#rm -f Source/CERNLIB/*.o
#cd Source/CERNLIB/
#make
#cd ../..

#find the proper param_card and replace it
model=`grep "import model" Cards/proc_card_mg5.dat | gawk '{print $3}'`
if [ -f Cards/param_card_${model}.dat ]; then
  cp Cards/param_card_${model}.dat Cards/param_card.dat
else
  echo Cards/param_card_${model}.dat not found
  return 1
fi



#________________________________________
# run the production stage - here you can select for running on multicore or not...
export PATH=`pwd`/bin:${PATH}

# copy the run card, proc card and param card to afs web folder.
cp -rf Cards/run_card.dat ${AFSFOLD}
cp -rf Cards/proc_card_mg5.dat ${AFSFOLD}
echo "==============================================================================="
cat Cards/proc_card_mg5.dat

echo "==============================================================================="
cat Cards/run_card.dat


# sequential run
#./bin/generate_events 0 gridpack_${name}

# batch run

./bin/generate_events 1 ${name} ${name}


# multicore run
#./bin/generate_events 2 6 ${name} 

ls  -ltrh

echo Please inspect xsec analytic reports for potential problems
#cat out

if [  -f ${name}_gridpack.tar.gz ] ;then
echo "gridpack is created...Will check for empty dirs now..."

mkdir test
cd test
tar -zxf ../${name}_gridpack.tar.gz
mv ../${name}_gridpack.tar.gz ../${name}_gridpack_old.tar.gz
############### Line below will serve to check if all Subprocesses are present, otherwise will correct the missing ones
cd madevent

ls SubProcesses/P*/G* -d > dirs

#cat empty_results | awk -F "results" '{print $1}' > dirs
while read line
do
#cd $line
unset count
count=`cat $line/*results.dat | wc -l`

if [ $count -lt "2" ] ;then
echo WARNING $line/results.dat appears empty
echo $line >> empty_dirs_log
cd $line
cat *results.dat
../madevent <input_app.txt
unset xsec
xsec=`cat results.dat  | awk '{print $1}' | head -1`
cd ../../..
echo missing xsec $xsec for $line >> missing_xsec_report
fi
done<dirs

cd ..
tar -cf ${name}_gridpack.tar madevent/ run.sh
gzip ${name}_gridpack.tar
mv ${name}_gridpack.tar ../.
	
fi

echo "End of job on " `date`


