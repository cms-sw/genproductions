#!/bin/bash

##########################################################################################
#GENERAL INSTRUCTIONS:                                                                   #
#You should take care of having the following ingredients in order to have this recipe   #
#working: run card and proc card (in a "cards" folder), MadGraph release, this script    #
#all in the same folder!                                                                 #
#Important: Param card is not mandatory for this script                                  #
##########################################################################################


##########################################################################################
#For runnning, the following command should be used                                      #
#./create_gridpack_template.sh NAME_OF_PRODCUTION RELATIVE_PATH_TO_CARDS QUEUE_SELECTION #
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

name=${1}

# name of the run
carddir=${2}

# which queue
queue=${3}

if [ -z ${name} ]; then
  echo "Process/card name not provided"
  return 1>/dev/null || exit 1
fi

if [ ! -z ${CMSSW_BASE} ]; then
  echo "Error: This script must be run in a clean environment as it sets up CMSSW itself.  You already have a CMSSW environment set up for ${CMSSW_VERSION}."
  echo "Please try again from a clean shell."
  return 1>/dev/null || exit 1
fi


#________________________________________
# to be set for user spesific
# Release to be used to define the environment and the compiler needed

#For correct running you should place at least the run and proc card in a folder under the name "cards" in the same folder where you are going to run the script

if [ -z "$PRODHOME" ]; then
  PRODHOME=`pwd`
fi 

RUNHOME=`pwd`
AFSFOLD=${PRODHOME}/${name}
# the folder where the script works, I guess
AFS_GEN_FOLDER=${RUNHOME}/${name}
# where to search for datacards, that have to follow a naming code: 
#   ${name}_proc_card_mg5.dat
#   ${name}_run_card.dat
CARDSDIR=${PRODHOME}/${carddir}
# where to find the madgraph tarred distribution


MGBASEDIR=mgbasedir

MG=MG5_aMC_v2.2.2.tar.gz
MGSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$MG

SYSCALC=SysCalc_V1.1.0.tar.gz
SYSCALCSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$SYSCALC

MGBASEDIRORIG=MG5_aMC_v2_2_2

if [ ! -d ${AFS_GEN_FOLDER}/${name}_gridpack ]; then
  #directory doesn't exist, create it and set up environment
  
  if [ ! -d ${AFS_GEN_FOLDER} ]; then
    mkdir ${AFS_GEN_FOLDER}
  fi

  cd $AFS_GEN_FOLDER

#  export SCRAM_ARCH=slc6_amd64_gcc472 #Here one should select the correct architechture corresponding with the CMSSW release
#  export RELEASE=CMSSW_5_3_23_patch1

  export SCRAM_ARCH=slc6_amd64_gcc481
  export RELEASE=CMSSW_7_1_12_patch1


  ############################
  #Create a workplace to work#
  ############################
  scram project -n ${name}_gridpack CMSSW ${RELEASE} ;
  if [ ! -d ${name}_gridpack ]; then  
    return 1>/dev/null || exit 1
  fi
  
  cd ${name}_gridpack ; mkdir -p work ; cd work
  WORKDIR=`pwd`
  eval `scram runtime -sh`


  #############################################
  #Copy, Unzip and Delete the MadGraph tarball#
  #############################################
  wget --no-check-certificate ${MGSOURCE}
  tar xzf ${MG}
  rm $MG

  #############################################
  #Apply any necessary patches on top of official release
  #############################################

  patch -l -p0 -i $PRODHOME/patches/mgfixes.patch
  patch -l -p0 -i $PRODHOME/patches/models.patch

  cd $MGBASEDIRORIG

  LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`

  #if lhapdf6 external is available then above points to lhapdf5 and needs to be overridden
  LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
  if [ -e $LHAPDF6TOOLFILE ]; then
    LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
  fi

  #make sure env variable for pdfsets points to the right place
  export LHAPDF_DATA_PATH=`$LHAPDFCONFIG --datadir`

  LHAPDFINCLUDES=`$LHAPDFCONFIG --incdir`
  LHAPDFLIBS=`$LHAPDFCONFIG --libdir`
  BOOSTINCLUDES=`scram tool tag boost INCLUDE`

  echo "set auto_update 0" > mgconfigscript
  echo "set automatic_html_opening False" >> mgconfigscript
  echo "set output_dependencies internal" >> mgconfigscript
  echo "set lhapdf $LHAPDFCONFIG" >> mgconfigscript

  if [ -n "$queue" ]; then
      echo "set run_mode  1" >> mgconfigscript
      echo "set cluster_type lsf" >> mgconfigscript
      echo "set cluster_queue $queue" >> mgconfigscript
      echo "set cluster_status_update 60 30" >> mgconfigscript
      echo "set cluster_nb_retry 3" >> mgconfigscript
      echo "set cluster_retry_wait 300" >> mgconfigscript 
      if [[ ! "$RUNHOME" =~ ^/afs/.* ]]; then
          echo "local path is not an afs path"
          echo "set cluster_temp_path `echo $RUNHOME`" >> mgconfigscript 
      fi      
  else
      echo "set run_mode 2" >> mgconfigscript
  fi

  echo "save options" >> mgconfigscript

  ./bin/mg5_aMC mgconfigscript

  #get syscalc and compile
  wget --no-check-certificate ${SYSCALCSOURCE}
  tar xzf ${SYSCALC}
  rm $SYSCALC

  cd SysCalc
  sed -i "s#INCLUDES =  -I../include#INCLUDES =  -I../include -I${LHAPDFINCLUDES} -I${BOOSTINCLUDES}#g" src/Makefile
  sed -i "s#LIBS = -lLHAPDF#LIBS = ${LHAPDFLIBS}/libLHAPDF.a #g" src/Makefile
  make

  cd $WORKDIR
  
  if [ "$name" == "interactive" ]; then
    return 0>/dev/null && exit 0
  fi

  echo `pwd`


  if [ -z ${carddir} ]; then
    echo "Card directory not provided"
    return 1>/dev/null || exit 1
  fi

  if [ ! -d $CARDSDIR ]; then
    echo $CARDSDIR " does not exist!"
    return 1>/dev/null || exit 1
  fi  
  
  ########################
  #Locating the proc card#
  ########################
  if [ ! -e $CARDSDIR/${name}_proc_card.dat ]; then
    echo $CARDSDIR/${name}_proc_card.dat " does not exist!"
    return 1>/dev/null || exit 1
  fi

  if [ ! -e $CARDSDIR/${name}_run_card.dat ]; then
    echo $CARDSDIR/${name}_run_card.dat " does not exist!"
    return 1>/dev/null || exit 1
  fi  
  
  cp $CARDSDIR/${name}_proc_card.dat ${name}_proc_card.dat
  

  ########################
  #Run the code-generation step to create the process directory
  ########################

  ./$MGBASEDIRORIG/bin/mg5_aMC ${name}_proc_card.dat
  
else  
  echo "Reusing existing directory assuming generated code already exists"
  echo "WARNING: If you changed the process card you need to clean the folder and run from scratch"
  
  cd $AFS_GEN_FOLDER
  
  WORKDIR=$AFS_GEN_FOLDER/${name}_gridpack/work/
  if [ ! -d ${WORKDIR} ]; then
    echo "Existing directory does not contain expected folder $WORKDIR"
    return 1>/dev/null || exit 1
  fi
  cd $WORKDIR

  eval `scram runtime -sh`

  LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`

  #if lhapdf6 external is available then above points to lhapdf5 and needs to be overridden
  LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
  if [ -e $LHAPDF6TOOLFILE ]; then
    LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
  fi

  #make sure env variable for pdfsets points to the right place
  export LHAPDF_DATA_PATH=`$LHAPDFCONFIG --datadir`  
  

  if [ "$name" == "interactive" ]; then
    return 0>/dev/null && exit 0
  fi
  
  if [ -z ${carddir} ]; then
    echo "Card directory not provided"
    return 1>/dev/null || exit 1
  fi

  if [ ! -d $CARDSDIR ]; then
    echo $CARDSDIR " does not exist!"
    return 1>/dev/null || exit 1
  fi
  
  if [ ! -e $CARDSDIR/${name}_run_card.dat ]; then
    echo $CARDSDIR/${name}_run_card.dat " does not exist!"
    return 1>/dev/null || exit 1
  fi  

fi  

if [ -d gridpack ]; then
  rm -rf gridpack
fi

if [ -d processtmp ]; then
  rm -rf processtmp
fi

if [ ! -d ${name} ]; then
  echo "Process output directory ${name} not found.  Either process generation failed, or the name of the output did not match the process name ${name} provided to the script."
fi

cp -a $name/ processtmp

cd processtmp

#######################
#Locating the run card#
#######################

cp $CARDSDIR/${name}_run_card.dat ./Cards/run_card.dat


#automatically detect NLO mode or LO mode from output directory
isnlo=0
if [ -e ./MCatNLO ]; then
  isnlo=1
fi

if [ "$isnlo" -gt "0" ]; then
#NLO mode  
  #######################
  #Run the integration and generate the grid
  #######################

  if [ -e $CARDSDIR/${name}_madspin_card.dat ]; then
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
  
  echo "mg5_path = ../mgbasedir" >> ./Cards/amcatnlo_configuration.txt

  cd $WORKDIR
  
  mkdir gridpack

  mv processtmp gridpack/process

  cp -a $MGBASEDIRORIG/ gridpack/mgbasedir
  
  cd gridpack

  cp $PRODHOME/runcmsgrid_NLO.sh ./runcmsgrid.sh
  
else
  #LO mode
  #######################
  #Run the integration and generate the grid
  #######################

  echo "done" > makegrid.dat
  echo "set gridpack true" >> makegrid.dat
  if [ -e $CARDSDIR/${name}_customizecards.dat ]; then
          cat $CARDSDIR/${name}_customizecards.dat >> makegrid.dat
          echo "" >> makegrid.dat
  fi
  echo "done" >> makegrid.dat

  cat makegrid.dat | ./bin/generate_events pilotrun

  cd $WORKDIR
  mkdir process
  cd process
  tar -xzf $WORKDIR/processtmp/pilotrun_gridpack.tar.gz
  
  #prepare madspin grids if necessary
  if [ -e $CARDSDIR/${name}_madspin_card.dat ]; then
    echo "import $WORKDIR/processtmp/Events/pilotrun/unweighted_events.lhe.gz" > madspinrun.dat
    cat $CARDSDIR/${name}_madspin_card.dat >> madspinrun.dat
    cat madspinrun.dat | $WORKDIR/$MGBASEDIRORIG/MadSpin/madspin
    rm madspinrun.dat
    rm -rf tmp*
    cp $CARDSDIR/${name}_madspin_card.dat $WORKDIR/process/madspin_card.dat
  fi

  #set to single core mode  
  echo "mg5_path = ../../mgbasedir" >> ./madevent/Cards/me5_configuration.txt
  echo "run_mode = 0" >> ./madevent/Cards/me5_configuration.txt  
    
  cd $WORKDIR
  
  mkdir gridpack
  mv process gridpack/process
  cp -a $MGBASEDIRORIG/ gridpack/mgbasedir

  cd gridpack
  
  cp $PRODHOME/runcmsgrid_LO.sh ./runcmsgrid.sh
  
fi

#clean unneeded files for generation
$PRODHOME/cleangridmore.sh

#create tarball with very aggressive xz settings (trade memory and cpu usage for compression ratio)
echo "Creating tarball"
XZ_OPT="--lzma2=preset=9,dict=512MiB" tar -cJpsf ${name}_tarball.tar.xz mgbasedir process runcmsgrid.sh

mv ${name}_tarball.tar.xz ${PRODHOME}/${name}_tarball.tar.xz

echo "End of job"

return 0>/dev/null && exit 0
