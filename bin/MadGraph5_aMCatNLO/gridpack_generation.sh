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

# Create tarball with very aggressive xz settings.
# (trade memory and cpu usage for compression ratio)
make_tarball () {
    echo "Creating tarball"
    cd $WORKDIR/gridpack

    if [ $iscmsconnect -gt 0 ]; then
        XZ_OPT="--lzma2=preset=2,dict=256MiB"
    else
        XZ_OPT="--lzma2=preset=9,dict=512MiB"
    fi

    mkdir InputCards
    cp $CARDSDIR/* InputCards

    EXTRA_TAR_ARGS=""
    if [ -e $CARDSDIR/${name}_externaltarball.dat ]; then
        EXTRA_TAR_ARGS="${name}_externaltarball.dat header_for_madspin.txt"
    fi
    XZ_OPT="$XZ_OPT" tar -cJpsf ${PRODHOME}/${name}_${scram_arch}_${cmssw_version}_tarball.tar.xz mgbasedir process runcmsgrid.sh gridpack_generation*.log InputCards $EXTRA_TAR_ARGS

    echo "Gridpack created successfully at ${PRODHOME}/${name}_${scram_arch}_${cmssw_version}_tarball.tar.xz"
    echo "End of job"

    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 0; else exit 0; fi
}

make_gridpack () {
    echo "Starting job on " `date` #Only to display the starting of production date
    echo "Running on " `uname -a` #Only to display the machine where the job is running
    echo "System release " `cat /etc/redhat-release` #And the system release
    
    echo "name: ${name}"
    echo "carddir: ${carddir}"
    echo "queue: ${queue}"
    echo "scram_arch: ${scram_arch}"
    echo "cmssw_version: ${cmssw_version}"
    
    # CMS Connect runs git status inside its own script.
    if [ $iscmsconnect -eq 0 ]; then
      cd $PRODHOME
      git status
      echo "Current git revision is:"
      git rev-parse HEAD
      git diff | cat
      cd -
    fi
    
    # where to find the madgraph tarred distribution
    MGBASEDIR=mgbasedir
    
    MG_EXT=".tar.gz"
    MG=MG5_aMC_v2.6.0$MG_EXT
    MGSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$MG
    
    MGBASEDIRORIG=$(echo ${MG%$MG_EXT} | tr "." "_")
    isscratchspace=0
    
    if [ ! -d ${GEN_FOLDER}/${name}_gridpack ]; then
      #directory doesn't exist, create it and set up environment
      
      if [ ! -d ${GEN_FOLDER} ]; then
        mkdir ${GEN_FOLDER}
      fi
    
      cd $GEN_FOLDER
    
    #   export SCRAM_ARCH=slc6_amd64_gcc472 #Here one should select the correct architechture corresponding with the CMSSW release
    #   export RELEASE=CMSSW_5_3_32_patch3
    
      export SCRAM_ARCH=${scram_arch}
      export RELEASE=${cmssw_version}
    
      ############################
      #Create a workplace to work#
      ############################
      scram project -n ${name}_gridpack CMSSW ${RELEASE} ;
      if [ ! -d ${name}_gridpack ]; then  
        if [ "${BASH_SOURCE[0]}" != "${0}" ]; then echo "yes here"; return 1; else exit 1; fi
      fi
      
      cd ${name}_gridpack ; mkdir -p work ; cd work
      WORKDIR=`pwd`
      eval `scram runtime -sh`
    
    
      #############################################
      #Copy, Unzip and Delete the MadGraph tarball#
      #############################################
      wget --no-check-certificate ${MGSOURCE}
      tar xzf ${MG}
      rm "$MG"
    
      #############################################
      #Apply any necessary patches on top of official release
      #############################################
    
      cd $MGBASEDIRORIG
      cat $PRODHOME/patches/*.patch | patch -p1
    
      if [ -e $CARDSDIR/${name}_loop_filter.py ]; then
        echo "Acitvating custom user loop filter"
        cat $CARDSDIR/${name}_loop_filter.py | patch -p1
      fi
    
      LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`
    
      LHAPDFINCLUDES=`$LHAPDFCONFIG --incdir`
      LHAPDFLIBS=`$LHAPDFCONFIG --libdir`
      export BOOSTINCLUDES=`scram tool tag boost INCLUDE`
    
      echo "set auto_update 0" > mgconfigscript
      echo "set automatic_html_opening False" >> mgconfigscript
      if [ $iscmsconnect -gt 0 ]; then
        echo "set output_dependencies internal" >> mgconfigscript
      fi
    #  echo "set output_dependencies internal" >> mgconfigscript
      echo "set lhapdf $LHAPDFCONFIG" >> mgconfigscript
    #   echo "set ninja $PWD/HEPTools/lib" >> mgconfigscript
    
      if [ "$queue" == "local" ]; then
          echo "set run_mode 2" >> mgconfigscript
      else
          #suppress lsf emails
          export LSB_JOB_REPORT_MAIL="N"
      
          echo "set run_mode  1" >> mgconfigscript
          if [ "$queue" == "condor" ]; then
            echo "set cluster_type condor" >> mgconfigscript
            echo "set cluster_queue None" >> mgconfigscript
          else
            echo "set cluster_type lsf" >> mgconfigscript
            #*FIXME* broken in mg_amc 2.4.0
    #         echo "set cluster_queue $queue" >> mgconfigscript
          fi 
          if [ $iscmsconnect -gt 0 ]; then
    	  n_retries=10
    	  long_wait=300
    	  short_wait=120
          else
    	  n_retries=3
    	  long_wait=60
    	  short_wait=30
          fi
          echo "set cluster_status_update $long_wait $short_wait" >> mgconfigscript
          echo "set cluster_nb_retry $n_retries" >> mgconfigscript
          echo "set cluster_retry_wait 300" >> mgconfigscript
          #echo "set cluster_local_path `${LHAPDFCONFIG} --datadir`" >> mgconfigscript 
          if [[ ! "$RUNHOME" =~ ^/afs/.* ]]; then
              echo "local path is not an afs path, batch jobs will use worker node scratch space instead of afs"
              #*FIXME* broken in mg_amc 2.4.0
    #           echo "set cluster_temp_path `echo $RUNHOME`" >> mgconfigscript 
              echo "set cluster_retry_wait 30" >> mgconfigscript 
              isscratchspace=1
          fi      
      fi
    
      echo "save options" >> mgconfigscript
    
      ./bin/mg5_aMC mgconfigscript
    
      #load extra models if needed
      if [ -e $CARDSDIR/${name}_extramodels.dat ]; then
        echo "Loading extra models specified in $CARDSDIR/${name}_extramodels.dat"
        #strip comments
        sed 's:#.*$::g' $CARDSDIR/${name}_extramodels.dat | while read model
        do
          #get needed BSM model
          if [[ $model = *[!\ ]* ]]; then
            echo "Loading extra model $model"
            wget --no-check-certificate https://cms-project-generators.web.cern.ch/cms-project-generators/$model	
            cd models
            if [[ $model == *".zip"* ]]; then
              unzip ../$model
            elif [[ $model == *".tgz"* ]]; then
              tar zxvf ../$model
            elif [[ $model == *".tar"* ]]; then
              tar xavf ../$model
            else 
              echo "A BSM model is specified but it is not in a standard archive (.zip or .tar)"
            fi
            cd ..
          fi
        done
      fi
    
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
      
      #*FIXME* workaround for broken cluster_local_path handling. 
      # This needs to happen before the code-generation step, as fortran templates
      # are modified based on this parameter.
      echo "cluster_local_path = `${LHAPDFCONFIG} --datadir`" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt 
    
      ########################
      #Run the code-generation step to create the process directory
      ########################
    
      sed -i '$ a display multiparticles' ${name}_proc_card.dat
      ./$MGBASEDIRORIG/bin/mg5_aMC ${name}_proc_card.dat
    
      is5FlavorScheme=0
      if tail -n 20 $LOGFILE | grep -q -e "^p *=.*b\~.*b" -e "^p *=.*b.*b\~"; then 
        is5FlavorScheme=1
      fi
    
       #*FIXME* workaround for broken set cluster_queue and run_mode handling
       if [ "$queue" != "condor" ]; then
         echo "cluster_queue = $queue" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
         if [ "$isscratchspace" -gt "0" ]; then
             echo "cluster_temp_path = `echo $RUNHOME`" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
         fi
       elif [ "$queue" == "condor" ]; then
         echo "cluster_queue = None" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
         echo "run_mode = 1" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
         echo "cluster_type = condor" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
       fi
    
      # Previous cluster_local_path setting  gets erased after
      # code-generation mg5_aMC execution, set it up again before the integrate step.
      echo "cluster_local_path = `${LHAPDFCONFIG} --datadir`" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt    
      
      if [ -e $CARDSDIR/${name}_patch_me.sh ]; then
          echo "Patching generated matrix element code with " $CARDSDIR/${name}_patch_me.sh
          /bin/bash "$CARDSDIR/${name}_patch_me.sh" "$WORKDIR/$MGBASEDIRORIG"
      fi;
      
      if [ "${jobstep}" = "CODEGEN" ]; then
          echo "job finished step ${jobstep}, exiting now."
          if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 0; else exit 0; fi
      fi
    
    elif [ "${jobstep}" = "INTEGRATE" ] || [ "${jobstep}" = "ALL" ]; then  
      echo "Reusing existing directory assuming generated code already exists"
      echo "WARNING: If you changed the process card you need to clean the folder and run from scratch"
    
      if [ "$is5FlavorScheme" -eq -1 ]; then
        if cat $LOGFILE_NAME*.log | grep -q -e "^p *=.*b\~.*b" -e "^p *=.*b.*b\~"; then 
            is5FlavorScheme=1
        else
            is5FlavorScheme=0
        fi 
      fi
      
      cd $GEN_FOLDER
      
      if [ ! -d ${WORKDIR} ]; then
        echo "Existing directory does not contain expected folder $WORKDIR"
        if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
      fi
      cd $WORKDIR
    
      eval `scram runtime -sh`
      export BOOSTINCLUDES=`scram tool tag boost INCLUDE`
    
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
        if [ $iscmsconnect -gt 0 ]; then
          echo "Reusing existing process directory - be careful!"
        else
          echo "Reusing an existing process directory ${name} is not actually supported in production at the moment.  Please clean or move the directory and start from scratch."
          if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
        fi
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
    
    #################################
    #Add PDF info and copy run card #
    #################################
    script_dir="${PRODHOME}/Utilities/scripts"
    if [ ! -d "$script_dir" ]; then
      script_dir=$(git rev-parse --show-toplevel)/Utilities/scripts
    fi
    
    prepare_run_card $name $CARDSDIR $is5FlavorScheme $script_dir
    
    #copy provided custom fks params or cuts
    if [ -e $CARDSDIR/${name}_cuts.f ]; then
      echo "copying custom cuts.f file"
      cp $CARDSDIR/${name}_cuts.f ./SubProcesses/cuts.f
    fi
    
    if [ -e $CARDSDIR/${name}_FKS_params.dat ]; then
      echo "copying custom FKS_params.dat file"
      cp $CARDSDIR/${name}_FKS_params.dat ./Cards/FKS_params.dat
    fi
    
    if [ -e $CARDSDIR/${name}_setscales.f ]; then
      echo "copying custom setscales.f file"
      cp $CARDSDIR/${name}_setscales.f ./SubProcesses/setscales.f
    fi
    
    if [ -e $CARDSDIR/${name}_reweight_xsec.f ]; then
      echo "copying custom reweight_xsec.f file"
      cp $CARDSDIR/${name}_reweight_xsec.f ./SubProcesses/reweight_xsec.f
    fi
    
    if [ -e $CARDSDIR/${name}_reweight_card.dat ]; then
      echo "copying custom reweight file"
      cp $CARDSDIR/${name}_reweight_card.dat ./Cards/reweight_card.dat
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
      echo "starting NLO mode"
    
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
      echo "finished pilot run"
    
      if [ -e $CARDSDIR/${name}_externaltarball.dat ]; then
          gunzip ./Events/pilotrun_decayed_1/events.lhe.gz
          sed -n '/<MG5ProcCard>/,/<\/slha>/p' ./Events/pilotrun_decayed_1/events.lhe > header_for_madspin.txt
          mv header_for_madspin.txt $WORKDIR
          gzip ./Events/pilotrun_decayed_1/events.lhe
      fi
      
      echo "mg5_path = ../mgbasedir" >> ./Cards/amcatnlo_configuration.txt
    #   echo "ninja = ../mgbasedir/HEPTools/lib" >> ./Cards/amcatnlo_configuration.txt
      echo "cluster_temp_path = None" >> ./Cards/amcatnlo_configuration.txt
    
      cd $WORKDIR
      
      mkdir gridpack
    
      mv processtmp gridpack/process
    
      cp -a $MGBASEDIRORIG/ gridpack/mgbasedir
      
      cd gridpack
    
      cp $PRODHOME/runcmsgrid_NLO.sh ./runcmsgrid.sh
      
      if [ -e $CARDSDIR/${name}_externaltarball.dat ]; then
        mv $WORKDIR/header_for_madspin.txt . 
      fi
      
    else
      #LO mode
      #######################
      #Run the integration and generate the grid
      #######################
    
      echo "starting LO mode"
    
      echo "done" > makegrid.dat
      echo "set gridpack True" >> makegrid.dat
      if [ -e $CARDSDIR/${name}_customizecards.dat ]; then
              cat $CARDSDIR/${name}_customizecards.dat >> makegrid.dat
              echo "" >> makegrid.dat
      fi
      echo "done" >> makegrid.dat
    
    #   set +e
      cat makegrid.dat | ./bin/generate_events pilotrun
      echo "finished pilot run"
    
      cd $WORKDIR
      
    #   echo "creating debug tarball"
    #   cp ${LOGFILE} ./gridpack_generation.log
    #   DEBUGTARBALL=${name}_debug_tarball.tar.gz
    #   tar -czps --ignore-failed-read -f ${DEBUGTARBALL} processtmp gridpack_generation.log
    #   echo "moving tarball to ${PRODHOME}/${DEBUGTARBALL}"
    #   mv ${DEBUGTARBALL} ${PRODHOME}/${DEBUGTARBALL}
    #   set -e
      
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
      
      # precompile reweighting if necessary
      if [ -e $CARDSDIR/${name}_reweight_card.dat ]; then
          pwd
          echo "preparing reweighting step"
          mkdir -p madevent/Events/pilotrun
          cp $WORKDIR/unweighted_events.lhe.gz madevent/Events/pilotrun
          echo "f2py_compiler=" `which gfortran` >> ./madevent/Cards/me5_configuration.txt
          #need to set library path or f2py won't find libraries
          export LIBRARY_PATH=$LD_LIBRARY_PATH
          cd madevent
          bin/madevent reweight pilotrun
          # Explicitly compile all subprocesses
          for file in $(ls -d rwgt/*/SubProcesses/P*); do
            echo "Compiling subprocess $(basename $file)"
            cd $file
            for i in 2 3; do
                MENUM=$i make matrix${i}py.so >& /dev/null
                echo "Library MENUM=$i compiled with status $?"
            done
            cd -
          done
          cd ..      
      fi
      
      #prepare madspin grids if necessary
      if [ -e $CARDSDIR/${name}_madspin_card.dat ]; then
        echo "import $WORKDIR/unweighted_events.lhe.gz" > madspinrun.dat
        cat $CARDSDIR/${name}_madspin_card.dat >> madspinrun.dat
        cat madspinrun.dat | $WORKDIR/$MGBASEDIRORIG/MadSpin/madspin
        rm madspinrun.dat
        rm -rf tmp*
        cp $CARDSDIR/${name}_madspin_card.dat $WORKDIR/process/madspin_card.dat
      fi
    
      echo "preparing final gridpack"
      
      #set to single core mode
      echo "mg5_path = ../../mgbasedir" >> ./madevent/Cards/me5_configuration.txt
      echo "cluster_temp_path = None" >> ./madevent/Cards/me5_configuration.txt
      echo "run_mode = 0" >> ./madevent/Cards/me5_configuration.txt  
        
      cd $WORKDIR
      
      mkdir gridpack
      mv process gridpack/process
      cp -a $MGBASEDIRORIG/ gridpack/mgbasedir
    
      cd gridpack
      
      cp $PRODHOME/runcmsgrid_LO.sh ./runcmsgrid.sh
    fi
    
    sed -i s/SCRAM_ARCH_VERSION_REPLACE/${scram_arch}/g runcmsgrid.sh
    sed -i s/CMSSW_VERSION_REPLACE/${cmssw_version}/g runcmsgrid.sh
    
    pdfExtraArgs=""
    if [ $is5FlavorScheme -eq 1 ]; then
      pdfExtraArgs+="--is5FlavorScheme "
    fi 
    
    pdfSysArgs=$(python ${script_dir}/getMG5_aMC_PDFInputs.py -f systematics -c 2017 $pdfExtraArgs)
    sed -i s/PDF_SETS_REPLACE/${pdfSysArgs}/g runcmsgrid.sh
    
    
    #clean unneeded files for generation
    ${helpers_dir}/cleangridmore.sh
    
    #
    #Plan to decay events from external tarball?
    # 
    
    if [ -e $CARDSDIR/${name}_externaltarball.dat ]; then
        echo "Locating the external tarball"
        cp $CARDSDIR/${name}_externaltarball.dat .
        source $CARDSDIR/${name}_externaltarball.dat
        echo $EXTERNAL_TARBALL 
        cp $EXTERNAL_TARBALL .
        tarname=$(basename $EXTERNAL_TARBALL)
        mkdir external_tarball
        cd external_tarball
        tar -xvaf ../$tarname
        cd ..
        rm $tarname
    fi
}

#exit on first error
set -e

#First you need to set couple of settings:

name=${1}

# name of the run
carddir=${2}

# which queue
queue=${3}

# processing options
jobstep=${4}

if [ -n "$5" ]
  then
    scram_arch=${5}
  else
    scram_arch=slc6_amd64_gcc481
fi

if [ -n "$6" ]
  then
    cmssw_version=${6}
  else
    cmssw_version=CMSSW_7_1_30
fi
 
# jobstep can be 'ALL','CODEGEN', 'INTEGRATE', 'MADSPIN'

if [ -z "$PRODHOME" ]; then
  PRODHOME=`pwd`
fi 

# Folder structure is different on CMSConnect
helpers_dir=${PRODHOME}/Utilities
if [ ! -d "$helpers_dir" ]; then
    helpers_dir=$(git rev-parse --show-toplevel)/bin/MadGraph5_aMCatNLO/Utilities
fi
source ${helpers_dir}/gridpack_helpers.sh 


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

if [ -z ${queue} ]; then
  queue=local
fi

if [ -z ${jobstep} ]; then
  jobstep=ALL
fi

#Check values of jobstep:
if [ "${jobstep}" == "ALL" ] || [ "${jobstep}" == "CODEGEN" ] || [ "${jobstep}" == "INTEGRATE" ] || [ "${jobstep}" == "MADSPIN" ]; then
    echo "Running gridpack generation step ${jobstep}"
else
    echo "No Valid Job Step specified, exiting "
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
fi 

# @TODO: MADSPIN hasn't been split from INTEGRATE step yet. Just exit for now.
if  [ "${jobstep}" == "MADSPIN" ]; then
    echo "MADSPIN hasn't been split from INTEGRATE step yet. Doing nothing. "
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 0; else exit 0; fi
fi 

#For correct running you should place at least the run and proc card in a folder under the name "cards" in the same folder where you are going to run the script
RUNHOME=`pwd`
LOGFILE=${RUNHOME}/${name}.log
LOGFILE_NAME=${LOGFILE/.log/}

# where to search for datacards, that have to follow a naming code: 
#   ${name}_proc_card_mg5.dat
#   ${name}_run_card.dat
CARDSDIR=${PRODHOME}/${carddir}
# the folder where the script works, I guess
GEN_FOLDER=${RUNHOME}/${name}

WORKDIR=$GEN_FOLDER/${name}_gridpack/work/

is5FlavorScheme=-1
if [ -z ${iscmsconnect:+x} ]; then iscmsconnect=0; fi

if [ "${name}" != "interactive" ]; then
    set -o pipefail
    make_gridpack | tee $LOGFILE

    echo "Saving log file(s)"
    cd $WORKDIR/gridpack
    for i in ${LOGFILE_NAME}*.log; do 
        cp $i ${i/$LOGFILE_NAME/gridpack_generation}
    done
else
    make_gridpack
fi

make_tarball
