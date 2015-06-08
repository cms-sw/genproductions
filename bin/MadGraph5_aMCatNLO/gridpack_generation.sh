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

#exit on first error
set -e

#First you need to set couple of settings:

name=${1}

# name of the run
carddir=${2}

# which queue
queue=${3}

if [ -z "$PRODHOME" ]; then
  PRODHOME=`pwd`
fi 

if [ ! -z ${CMSSW_BASE} ]; then
  echo "Error: This script must be run in a clean environment as it sets up CMSSW itself.  You already have a CMSSW environment set up for ${CMSSW_VERSION}."
  echo "Please try again from a clean shell."
  if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
fi

#catch unset variables
set -u

if [ -z ${name} ]; then
  echo "Process/card name not provided"
  if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
fi


#________________________________________
# to be set for user spesific
# Release to be used to define the environment and the compiler needed

#For correct running you should place at least the run and proc card in a folder under the name "cards" in the same folder where you are going to run the script

RUNHOME=`pwd`

LOGFILE=${RUNHOME}/${name}.log
if [ "${name}" != "interactive" ]; then
  exec > >(tee ${LOGFILE})
  exec 2>&1
fi

echo "Starting job on " `date` #Only to display the starting of production date
echo "Running on " `uname -a` #Only to display the machine where the job is running
echo "System release " `cat /etc/redhat-release` #And the system release

echo "name: ${name}"
echo "carddir: ${carddir}"
echo "queue: ${queue}"

cd $PRODHOME
git status
echo "Current git revision is:"
git rev-parse HEAD
git diff
cd -

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

#syscalc is a helper tool for madgraph to add scale and pdf variation weights for LO processes
SYSCALC=SysCalc_V1.1.0.tar.gz
SYSCALCSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$SYSCALC

#higgs characterization model NLO, needed for gluon-gluon-higgs effective vertex for gluon fusion production
#or for non-scalar production
HCNLO=HC_NLO_X0_UFO.zip
HCNLOSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$HCNLO

## Single VLQ model
SINGLEVLQ=STP_UFO_freeWidth.tar.gz
SINGLEVLQSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$SINGLEVLQ

#diagonal CKM 
SINGLEVLQ_diagCKM=STP_UFO_freeWidth_diagCKM.zip
SINGLEVLQSOURCE_diagCKM=https://cms-project-generators.web.cern.ch/cms-project-generators/$SINGLEVLQ_diagCKM

## Models for searches of diboson resonances
VVMODEL=dibosonResonanceModel.tar.gz
VVSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$VVMODEL

# Model for search for Z' resonances
ZPRIMEMODEL=topBSM_UFO.zip
ZPRIMESOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/${ZPRIMEMODEL}

## DM Model Vector Mediator
SimplifiedVDM=SimplifiedDM_VectorMediator_UFO.tar.gz
SimplifiedVDMSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/${SimplifiedVDM}

## Type I See Saw Majorana Neutrino
TypeIMajNeutrinoMODEL=typeISeeSaw_MajNeutrino_UFO.tar.gz
TypeIMajNeutrinoSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/${TypeIMajNeutrinoMODEL}

# Model for search for excited top quark (t*)
TOP32MODEL=top32.tgz
TOP32SOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/${TOP32MODEL}

# Model for Z' > VLQ
ZPTOVLQMODEL=onerho.tar.gz
ZPTOVLQSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/${ZPTOVLQMODEL}

## Model for tGamma FCNC                                                                                                                              
TGAMMAMODEL=tqAandG_UFO.zip
TGAMMASOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/${TGAMMAMODEL}

MGBASEDIRORIG=MG5_aMC_v2_2_2

isscratchspace=0

if [ ! -d ${AFS_GEN_FOLDER}/${name}_gridpack ]; then
  #directory doesn't exist, create it and set up environment
  
  if [ ! -d ${AFS_GEN_FOLDER} ]; then
    mkdir ${AFS_GEN_FOLDER}
  fi

  cd $AFS_GEN_FOLDER

#   export SCRAM_ARCH=slc6_amd64_gcc472 #Here one should select the correct architechture corresponding with the CMSSW release
#   export RELEASE=CMSSW_5_3_23_patch1

  export SCRAM_ARCH=slc6_amd64_gcc481
  export RELEASE=CMSSW_7_1_14


  ############################
  #Create a workplace to work#
  ############################
  scram project -n ${name}_gridpack CMSSW ${RELEASE} ;
  if [ ! -d ${name}_gridpack ]; then  
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
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

  #if lhapdf6 external is available then above points to lhapdf5 and needs to be overridden
  LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
    
  if [ -e $LHAPDF6TOOLFILE ]; then
    LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
  else
    LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`
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
      echo "set cluster_nb_retry 5" >> mgconfigscript
      echo "set cluster_retry_wait 300" >> mgconfigscript 
      if [[ ! "$RUNHOME" =~ ^/afs/.* ]]; then
          echo "local path is not an afs path, batch jobs will use worker node scratch space instead of afs"
          echo "set cluster_temp_path `echo $RUNHOME`" >> mgconfigscript 
          isscratchspace=1
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
  cd ..
  
  #get HC nlo & single VLQ models
  wget --no-check-certificate ${HCNLOSOURCE}
  wget --no-check-certificate ${SINGLEVLQSOURCE}
  wget --no-check-certificate ${SINGLEVLQSOURCE_diagCKM}
  cd models
  unzip ../${HCNLO}
  tar -zxvf ../${SINGLEVLQ}
  unzip ../${SINGLEVLQ_diagCKM}
  cd ..

  ## DM Model Vector Mediator
  wget --no-check-certificate ${SimplifiedVDMSOURCE}
  cd models
  tar -zxvf ../${SimplifiedVDM}
  cd ..

  ## Type I See Saw Majorana Neutrino
  wget --no-check-certificate ${TypeIMajNeutrinoSOURCE}
  cd models
  tar -zxvf ../${TypeIMajNeutrinoMODEL}
  cd ..

  #get Diboson model
  wget --no-check-certificate ${VVSOURCE}
  cd models
  tar xvzf ../${VVMODEL}
  cd ..

  #get Z' model
  wget --no-check-certificate -O ${ZPRIMEMODEL} ${ZPRIMESOURCE}
  cd models
  unzip ../${ZPRIMEMODEL}
  cd ..

  #get t* model
  wget --no-check-certificate -O ${TOP32MODEL} ${TOP32SOURCE}
  cd models
  tar -xaf ../${TOP32MODEL}
  cd ..

  #get Z' > VLQ model
  wget --no-check-certificate ${ZPTOVLQSOURCE}
  cd models
  tar xvzf ../${ZPTOVLQMODEL}
  cd ..
  
  #get tGamma FCNC model                                                                                                                              
  wget --no-check-certificate -O ${TGAMMAMODEL} ${TGAMMASOURCE}
  cd models
  unzip ../${TGAMMAMODEL}
  cd ..

  
  cd $WORKDIR
  
  if [ "$name" == "interactive" ]; then
    set +e
    set +u
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 0; else exit 0; fi
  fi

  echo `pwd`


  if [ -z ${carddir} ]; then
    echo "Card directory not provided"
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
  fi

  if [ ! -d $CARDSDIR ]; then
    echo $CARDSDIR " does not exist!"
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
  fi  
  
  ########################
  #Locating the proc card#
  ########################
  if [ ! -e $CARDSDIR/${name}_proc_card.dat ]; then
    echo $CARDSDIR/${name}_proc_card.dat " does not exist!"
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
  fi

  if [ ! -e $CARDSDIR/${name}_run_card.dat ]; then
    echo $CARDSDIR/${name}_run_card.dat " does not exist!"
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
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
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
  fi
  cd $WORKDIR

  eval `scram runtime -sh`

  #LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`

  #if lhapdf6 external is available then above points to lhapdf5 and needs to be overridden
  LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
  if [ -e $LHAPDF6TOOLFILE ]; then
    LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
  else
    LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`
  fi

  #make sure env variable for pdfsets points to the right place
  export LHAPDF_DATA_PATH=`$LHAPDFCONFIG --datadir`  
  

  if [ "$name" == "interactive" ]; then
    set +e
    set +u  
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 0; else exit 0; fi
  else
    echo "Reusing an existing process directory ${name} is not actually supported in production at the moment.  Please clean or move the directory and start from scratch."
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
  fi
  
  if [ -z ${carddir} ]; then
    echo "Card directory not provided"
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
  fi

  if [ ! -d $CARDSDIR ]; then
    echo $CARDSDIR " does not exist!"
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
  fi
  
  if [ ! -e $CARDSDIR/${name}_run_card.dat ]; then
    echo $CARDSDIR/${name}_run_card.dat " does not exist!"
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
  fi  

fi  

if [ -d gridpack ]; then
  rm -rf gridpack
fi

if [ -d processtmp ]; then
  rm -rf processtmp
fi

if [ -d process ]; then
  rm -rf process
fi

if [ ! -d ${name} ]; then
  echo "Process output directory ${name} not found.  Either process generation failed, or the name of the output did not match the process name ${name} provided to the script."
  if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
fi

#make copy of process directory for reuse only if not running on temp scratch space
if [ "$isscratchspace" -gt "0" ]; then
  echo "moving generated process to working directory"
  mv $name processtmp
else
  echo "copying generated process to working directory"
  cp -a $name/ processtmp
fi

cd processtmp

#######################
#Locating the run card#
#######################

echo "copying run_card.dat file"
cp $CARDSDIR/${name}_run_card.dat ./Cards/run_card.dat

#copy provided custom fks params or cuts
if [ -e $CARDSDIR/${name}_cuts.f ]; then
  echo "copying custom cuts.f file"
  cp $CARDSDIR/${name}_cuts.f ./SubProcesses/cuts.f
fi

if [ -e $CARDSDIR/${name}_FKS_params.dat ]; then
  echo "copying custom FKS_params.dat file"
  cp $CARDSDIR/${name}_FKS_params.dat ./Cards/FKS_params.dat
fi

if [ -e $CARDSDIR/${name}_MadLoopParams.dat ]; then
  echo "copying custom MadLoopParams.dat file"
  cp $CARDSDIR/${name}_MadLoopParams.dat ./Cards/MadLoopParams.dat
fi

if [ -e $CARDSDIR/${name}_setscales.f ]; then
  echo "copying custom setscales.f file"
  cp $CARDSDIR/${name}_setscales.f ./SubProcesses/setscales.f
fi

if [ -e $CARDSDIR/${name}_reweight_xsec.f ]; then
  echo "copying custom reweight_xsec.f file"
  cp $CARDSDIR/${name}_reweight_xsec.f ./SubProcesses/reweight_xsec.f
fi

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
  echo "cleaning temporary output"
  mv $WORKDIR/processtmp/pilotrun_gridpack.tar.gz $WORKDIR/
  mv $WORKDIR/processtmp/Events/pilotrun/unweighted_events.lhe.gz $WORKDIR/
  rm -rf processtmp
  mkdir process
  cd process
  echo "unpacking temporary gridpack"
  tar -xzf $WORKDIR/pilotrun_gridpack.tar.gz
  echo "cleaning temporary gridpack"
  rm $WORKDIR/pilotrun_gridpack.tar.gz
  
  #prepare madspin grids if necessary
  if [ -e $CARDSDIR/${name}_madspin_card.dat ]; then
    echo "import $WORKDIR/unweighted_events.lhe.gz" > madspinrun.dat
    cat $CARDSDIR/${name}_madspin_card.dat >> madspinrun.dat
    cat madspinrun.dat | $WORKDIR/$MGBASEDIRORIG/MadSpin/madspin
    rm madspinrun.dat
    rm $WORKDIR/unweighted_events.lhe.gz
    rm -rf tmp*
    cp $CARDSDIR/${name}_madspin_card.dat $WORKDIR/process/madspin_card.dat
  fi

  echo "preparing final gridpack"
  
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

echo "Saving log file"
#copy log file
cp ${LOGFILE} ./gridpack_generation.log

#create tarball with very aggressive xz settings (trade memory and cpu usage for compression ratio)
echo "Creating tarball"
XZ_OPT="--lzma2=preset=9,dict=512MiB" tar -cJpsf ${name}_tarball.tar.xz mgbasedir process runcmsgrid.sh gridpack_generation.log

mv ${name}_tarball.tar.xz ${PRODHOME}/${name}_tarball.tar.xz

echo "Gridpack created successfully at ${PRODHOME}/${name}_tarball.tar.xz"
echo "End of job"

if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 0; else exit 0; fi
