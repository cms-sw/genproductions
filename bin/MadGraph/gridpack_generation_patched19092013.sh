#!/bin/bash

##########################################################################################
#GENERAL INSTRUCTIONS:                                                                   #
#You should take care of having the following ingredients in order to have this recipe   #
#working: run card and proc card (in a "cards" folder), MadGraph release, this script    #
#all in the same folder!                                                                 #
#Important: Param card is not mandatory for this script                                  #
##########################################################################################

##########################################################################################
#DISCLAIMER:                                                                             #
#This script has been tested in CMSSW_6_2_0_patch1 and there is no guarantee it should work in   #
#another releases.                                                                       #
#To try in another releases you should adapt it taking into account to put the correct   #
#parameters as the architechture, the release names and compatibility issues as decribed #
#in the comments in this script                                                          #
#Additionally, this script depends on the well behaviour of the lxplus machines, so if   #
#one job fails the full set of jobs will fail and then you have to try again             #
#Any issues should be addressed to: cms-madgraph-support-team_at_cernSPAMNOT.ch          #
##########################################################################################

##########################################################################################
#For runnning, the following command should be used                                      #
#bash create_gridpack_template.sh NAME_OF_PRODCUTION QUEUE_SELECTION                     #
#Or you can make this script as an executable and the launch it with                     #
#chmod +x create_gridpack_template.sh                                                    #
#./create_gridpack_template.sh NAME_OF_PRODCUTION QUEUE_SELECTION                        #
#by NAME_OF_PRODUCTION you should use the names of run and proc card                     #
#for example if the cards are bb_100_250_proc_card_mg5.dat and bb_100_250_run_card.dat   #
#NAME_OF_PRODUCTION should be bb_100_250                                                 #
#for QUEUE_SELECTION is commonly used 1nd, but you can take another choice from bqueues  #
##########################################################################################

#set -o verbose

echo "Starting job on " `date` #Only to display the starting of production date
echo "Running on " `uname -a` #Only to display the machine where the job is running
echo "System release " `cat /etc/redhat-release` #And the system release

#First you need to set couple of settings:

# name of the run
name=${1}

# which queue
queue=$2

#________________________________________
# to be set for user spesific
# Release to be used to define the environment and the compiler needed

#For correct running you should place at least the run and proc card in a folder under the name "cards" in the same folder where you are going to run the script

export PRODHOME=`pwd`
AFSFOLD=${PRODHOME}/${name}
# the folder where the script works, I guess
AFS_GEN_FOLDER=${PRODHOME}/${name}
# where to search for datacards, that have to follow a naming code: 
#   ${name}_proc_card_mg5.dat
#   ${name}_run_card.dat
CARDSDIR=${PRODHOME}/cards
# where to find the madgraph tarred distribution
MGDIR=${PRODHOME}/
# madgraph distribution, that has been downloaded
MG=MG5v1.5.11_CERN_23082013_patched19092013.tar.gz

if [ ! -d ${AFS_GEN_FOLDER} ];then
mkdir ${AFS_GEN_FOLDER}
fi
cd $AFS_GEN_FOLDER

export SCRAM_ARCH=slc5_amd64_gcc472 #Here one should select the correct architechture corresponding with the CMSSW release
export RELEASE=CMSSW_6_2_0_patch1 #Here one should select the desired CMSSW release in correspondance with the line below

################################
#Initialize the CMS environment#
################################
export VO_CMS_SW_DIR=/afs/cern.ch/cms
source $VO_CMS_SW_DIR/cmsset_default.sh

#################################
#Clean the area the working area#
#################################
if [  -d ${name}_gridpack ] ;then
	rm -rf ${name}_gridpack
	echo "gridpack ${name}_gridpack exists..."
	exit 1;
fi

##################################
#Location of the madgraph tarball#
##################################
if [ ! -e $MGDIR/$MG ]; then
	echo "$MGDIR/$MG does not exit"
	exit 1;
else
	cp $MGDIR/$MG .	
fi

########################
#Locating the proc card#
########################
if [ ! -e $CARDSDIR/${name}_proc_card_mg5.dat ]; then
	echo $CARDSDIR/${name}_proc_card_mg5.dat " does not exist!"
	exit 1;
else
	cp $CARDSDIR/${name}_proc_card_mg5.dat ${name}_proc_card_mg5.dat
fi
	
#######################
#Locating the run card#
#######################
if [ ! -e $CARDSDIR/${name}_run_card.dat ]; then
	echo $CARDSDIR/${name}_run_card.dat " does not exist!"
	exit 1;
else
	cp $CARDSDIR/${name}_run_card.dat   ${name}_run_card.dat
fi	

#########################
#Locating the param card#
#########################
if [ ! -e $CARDSDIR/${name}_param_card.dat ]; then
	echo $CARDSDIR/${name}_param_card.dat " does not exist!"
	exit 1;
else
	cp $CARDSDIR/${name}_param_card.dat   ${name}_param_card.dat
fi	


############################
#Create a workplace to work#
############################
scram project -n ${name}_gridpack CMSSW ${RELEASE} ; cd ${name}_gridpack ; mkdir -p work ; cd work
eval `scram runtime -sh`

# force the f77 compiler to be the CMSSW defined ones
ln -s `which gfortran` f77
ln -s `which gfortran` g77
export PATH=`pwd`:${PATH}

#############################################
#Copy, Unzip and Delete the MadGraph tarball#
#############################################
MGSOURCE=${AFS_GEN_FOLDER}/${MG}

mv ${MGSOURCE} . ; tar xzf ${MG} ; cd MG5v1.5.11 #Here you have to put the correct name of the folder will be created when the MG tarball is untared, in this specific case "MG5v1.4.8" but if you use another release this may be different
cp -p mgPostProcv2.py ${PRODHOME}/.
#________________________________________
# Some settings for cern caf batch job submission

cat ${AFS_GEN_FOLDER}/${name}_run_card.dat | sed 's/  .false.     = gridpack  !True = setting up the grid pack/  .true.     = gridpack  !True = setting up the grid pack/g' > Template/Cards/run_card.dat

sed -i 's/100000       = nevents ! Number of unweighted events requested/1000       = nevents ! Number of unweighted events requested/g'  Template/Cards/run_card.dat

echo `pwd`
echo cp ${AFS_GEN_FOLDER}/${name}_proc_card_mg5.dat Template/Cards/proc_card_mg5.dat
cp ${AFS_GEN_FOLDER}/${name}_proc_card_mg5.dat Template/Cards/proc_card_mg5.dat
cp ${AFS_GEN_FOLDER}/${name}_param_card.dat Template/Cards/param_card.dat

#mv Template/bin/internal/addmasses.py.no Template/bin/internal/addmasses.py

#________________________________________
# set the run cards with the appropriate initial seed

cd Template ; /bin/echo 5 | ./bin/newprocess_mg5 
cp symmetry.f SubProcesses/

####################################################
#Modify the cluster to run on lsf with custom queue#
####################################################
#Cluster mode: Uncomment to use it
sed -e 's/run_mode = 0/run_mode = 1/g' -e 's/cluster_type = condor/cluster_type = lsf/g'  -e 's/cluster_queue = madgraph/cluster_queue = '${queue}'/g' ../input/mg5_configuration.txt > me5_temp ; mv me5_temp Cards/me5_configuration.txt

#Multicore mode: Uncomment to use it
#sed -e 's/run_mode = 0/run_mode = 2/g' -e 's/nb_core = None/nb_core = 8/g'  ../input/mg5_configuration.txt > me5_temp ; mv me5_temp Cards/me5_configuration.txt

###################################################################################
#Run the production stage - here you can select for running on multicore or not...#
###################################################################################
export PATH=`pwd`/bin:${PATH}

# copy the run card, proc card and param card to afs web folder.
cp -rf Cards/run_card.dat ${AFSFOLD}
cp -rf Cards/proc_card_mg5.dat ${AFSFOLD}
cp -rf Cards/param_card.dat ${AFSFOLD}
echo "==============================================================================="
cat Cards/proc_card_mg5.dat

echo "==============================================================================="
cat Cards/run_card.dat

echo "==============================================================================="
cat Cards/param_card.dat

echo "==============================================================================="
pwd
echo "==============================================================================="

./bin/generate_events -f

ls  -ltrh

#mv *gridpack.tar.gz ${PRODHOME}/${name}_gridpack.tar.gz

echo "Please inspect xsec analytic reports for potential problems"

#####################################
#Section to check empty subprocesses#
#####################################

if [  -f *gridpack.tar.gz ] ;then
echo "gridpack is created...Will check for empty dirs now..."

mkdir test
cd test
tar -zxf ../*gridpack.tar.gz
cp ../*gridpack.tar.gz ../gridpack_old.tar.gz
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
mv ${PRODHOME}/mgPostProcv2.py madevent/bin/.
tar -cf ${name}_gridpack.tar madevent/ run.sh
gzip ${name}_gridpack.tar
mv ${name}_gridpack.tar.gz ../.
        
fi

###############################################
#End of section to check for empy subprocesses#
###############################################

mv ${AFS_GEN_FOLDER}/${name}_gridpack/work/MG5v1.5.11/Template/${name}_gridpack.tar.gz ${PRODHOME}/${name}_gridpack.tar.gz

echo "End of job"
