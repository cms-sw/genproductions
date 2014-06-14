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
#This script has been tested in CMSSW_6_2_11 on slc6 and there is no guarantee it should work in   #
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
#If QUEUE_SELECTION is omitted, then run on local machine only (using multiple cores)    #
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
#MG=MG5v1.5.11_CERN_23082013_patched19092013.tar.gz
#https://launchpad.net/mg5amcnlo/2.0/2.1.0/+download/MG5_aMC_v2.1.0.tar.gz
MG=MG5_aMC_v2.1.1.tar.gz
#MGSOURCE=https://launchpad.net/mg5amcnlo/2.0/2.1.0/+download/${MG}
MGSOURCE=/afs/cern.ch/cms/generators/${MG}
MGBASEDIR=MG5_aMC_v2_1_1

if [ ! -d ${AFS_GEN_FOLDER} ];then
mkdir ${AFS_GEN_FOLDER}
fi
cd $AFS_GEN_FOLDER

export SCRAM_ARCH=slc6_amd64_gcc472 #Here one should select the correct architechture corresponding with the CMSSW release
export RELEASE=CMSSW_6_2_11 #Here one should select the desired CMSSW release in correspondance with the line below

################################
#Initialize the CMS environment#
################################
#export VO_CMS_SW_DIR=/afs/cern.ch/cms
#source $VO_CMS_SW_DIR/cmsset_default.sh

#################################
#Clean the area the working area#
#################################
if [  -d ${name}_gridpack ] ;then
	rm -rf ${name}_gridpack
	echo "gridpack ${name}_gridpack exists..."
	exit 1;
fi


############################
#Create a workplace to work#
############################
scram project -n ${name}_gridpack CMSSW ${RELEASE} ; cd ${name}_gridpack ; mkdir -p work ; cd work
WORKDIR=`pwd`
eval `scram runtime -sh`


#############################################
#Copy, Unzip and Delete the MadGraph tarball#
#############################################
#MGSOURCE=${AFS_GEN_FOLDER}/${MG}

#mv ${MGSOURCE} . ; tar xzf ${MG} ; cd MG5v1.5.11 #Here you have to put the correct name of the folder will be created when the MG tarball is untared, in this specific case "MG5v1.4.8" but if you use another release this may be different
#wget --no-check-certificate ${MGSOURCE}
cp ${MGSOURCE} .
tar xzf ${MG}
patch -l -p0 -i $PRODHOME/patches/mgfixes.patch
cd $MGBASEDIR


echo "set auto_update 0" > mgconfigscript
echo "set automatic_html_opening False" >> mgconfigscript
echo "set lhapdf `echo "$LHAPATH/../../../full/bin/lhapdf-config"`" >> mgconfigscript

if [ -n "$queue" ]; then
    echo "set run_mode  1" >> mgconfigscript
    echo "set cluster_type lsf" >> mgconfigscript
    echo "set cluster_queue $queue" >> mgconfigscript
    echo "set cluster_status_update 60 30" >> mgconfigscript
    echo "set cluster_nb_retry 3" >> mgconfigscript
else
    echo "set run_mode 2" >> mgconfigscript
fi

echo "save options" >> mgconfigscript


./bin/mg5_aMC mgconfigscript



echo `pwd`


########################
#Locating the proc card#
########################
if [ ! -e $CARDSDIR/${name}_proc_card_mg5.dat ]; then
	echo $CARDSDIR/${name}_proc_card_mg5.dat " does not exist!"
	#exit 1;
else
	cp $CARDSDIR/${name}_proc_card_mg5.dat ${name}_proc_card_mg5.dat
fi

./bin/mg5_aMC ${name}_proc_card_mg5.dat

cd $name

#######################
#Locating the run card#
#######################
if [ ! -e $CARDSDIR/${name}_run_card.dat ]; then
echo $CARDSDIR/${name}_run_card.dat " does not exist!"
#exit 1;
else
cp $CARDSDIR/${name}_run_card.dat ./Cards/run_card.dat
fi      

#######################
#Locating the madspin card#
#######################
if [ ! -e $CARDSDIR/${name}_madspin_card.dat ]; then
        echo $CARDSDIR/${name}_madspin_card.dat " does not exist! MadSpin will not be run."
else
        cp $CARDSDIR/${name}_madspin_card.dat ./Cards/madspin_card.dat
fi

echo "shower=OFF" > makegrid.dat
echo "done" >> makegrid.dat
if [ -e $CARDSDIR/${name}_customizecards.dat ]; then
        cat $CARDSDIR/${name}_customizecards.dat >> makegrid.dat
        echo "" >> makegrid.dat
fi
echo "done" >> makegrid.dat

cat makegrid.dat | ./bin/generate_events -n pilotrun

#set to single core mode
echo "mg5_path = ../" >> ./Cards/amcatnlo_configuration.txt
echo "run_mode = 0" >> ./Cards/amcatnlo_configuration.txt

cd $WORKDIR/$MGBASEDIR

#clean unneeded files for generation
$PRODHOME/clean4grid.sh

cd $WORKDIR
mv $MGBASEDIR mgbasedir
tar -czpsf ${name}_tarball.tar.gz mgbasedir/

mv ${name}_tarball.tar.gz ${PRODHOME}/${name}_tarball.tar.gz

echo "End of job"

exit 0
