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
    cp $CARDSDIR/${name}*.* InputCards

    if [ -e $CARDSDIR/BIAS ]; then
      cp -r $CARDSDIR/BIAS InputCards/
    fi

    EXTRA_TAR_ARGS=""
    if [ -e $CARDSDIR/${name}_externaltarball.dat ]; then
        EXTRA_TAR_ARGS="external_tarball header_for_madspin.txt "
    fi
    ### include merge.pl script for LO event merging 
    if [ -e merge.pl ]; then
        EXTRA_TAR_ARGS+="merge.pl "
    fi
    XZ_OPT="$XZ_OPT" tar -cJpf ${PRODHOME}/${name}_${scram_arch}_${cmssw_version}_tarball.tar.xz mgbasedir process runcmsgrid.sh gridpack_generation*.log InputCards $EXTRA_TAR_ARGS

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

    ########################
    #Locating the proc card#
    ########################

    if [ ! -d $CARDSDIR ]; then
      echo $CARDSDIR " does not exist!"
      if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
    fi

    if [ ! -e $CARDSDIR/${name}_proc_card.dat ]; then
      echo $CARDSDIR/${name}_proc_card.dat " does not exist!"
      if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
    fi

    if [ ! -e $CARDSDIR/${name}_run_card.dat ]; then
      echo $CARDSDIR/${name}_run_card.dat " does not exist!"
      if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
    fi
    
    # avoid compute_widths in customizecards 
    if [ -e $CARDSDIR/${name}_customizecards.dat ]; then
        if grep -F "compute_widths" $CARDSDIR/${name}_customizecards.dat ; then
            echo "<<compute_widths X>> is used in your customizecards.dat"
            echo "This could be problematic from time to time, so instead use <<set decay wX AUTO>> for width computations. Please take a look at \"compute_widths\" under \"Troubleshooting_and_Suggestions\" section."
            echo "https://twiki.cern.ch/twiki/bin/view/CMS/QuickGuideMadGraph5aMCatNLO#Troubleshooting_and_Suggestions"
            exit 1;
        fi
    fi

    # avoid compute_widths in customizecards 
    if [ -e $CARDSDIR/${name}_madspin_card.dat ]; then
        if grep -F "Nevents_for_max_weigth" $CARDSDIR/${name}_madspin_card.dat ; then
            echo "Nevents_for_max_weigth typo is fixed to Nevents_for_max_weight in MGv2.7.x releases."
            echo "$CARDSDIR/${name}_madspin_card.dat contains Nevents_for_max_weigth instead of Nevents_for_max_weight."
            echo "Please correct the typo."
            exit 1;
        fi
    fi

    # avoid characters in weight names that potentially corrupt lhe header 
    if [ -e $CARDSDIR/${name}_reweight_card.dat ]; then
        for weightname in `grep rwgt_name $CARDSDIR/${name}_reweight_card.dat` ; do
            if [[ "$weightname" == *"rwgt_name="* ]]; then
                remove="rwgt_name="
                weightname=${weightname#*$remove}
            else
                continue
            fi
            if [[ $weightname == *['!'@#\$%^\&*()\+\[\]{}]* ]]; then 
                echo " Please remove problematic characters from weight name: $weightname"  
                exit 1;    
            fi
        done 
    fi

    # CMS Connect runs git status inside its own script.
    if [ $iscmsconnect -eq 0 ]; then
      cd $PRODHOME
      if [ -x "$(command -v git)" ]; then
        git status
        echo "Current git revision is:"
        git rev-parse HEAD
        git diff | cat
      fi
      cd -
    fi
    
    # where to find the madgraph tarred distribution
    MGBASEDIR=mgbasedir
    
    MG_EXT=".tar.gz"
    MG=MG5_aMC_v2.9.18$MG_EXT
    MGSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$MG
    
    MGBASEDIRORIG=$(echo ${MG%$MG_EXT} | tr "." "_")
    isscratchspace=0
    
    if [ ! -d ${GEN_FOLDER}/${name}_gridpack ]; then
      #directory doesn't exist, create it and set up environment
      
      if [ ! -d ${GEN_FOLDER} ]; then
        mkdir ${GEN_FOLDER}
      fi
    
      cd $GEN_FOLDER
    
      export SCRAM_ARCH=${scram_arch}
      export RELEASE=${cmssw_version}
    
      ############################
      #Create a workplace to work#
      ############################
      export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
      set +u
      source $VO_CMS_SW_DIR/cmsset_default.sh
      set -u

      scram project -n ${name}_gridpack CMSSW ${RELEASE} ;
      if [ ! -d ${name}_gridpack ]; then  
        if [ "${BASH_SOURCE[0]}" != "${0}" ]; then echo "yes here"; return 1; else exit 1; fi
      fi
      
      cd ${name}_gridpack ; mkdir -p work ; cd work
      WORKDIR=`pwd`
      eval `scram runtime -sh`

      if [[ $queue == *"condor"* ]]; then
        echo "Use HTCondor for gridpack generation"
        source ${PRODHOME}/Utilities/source_condor.sh
      fi

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
      cp -r $PRODHOME/PLUGIN/CMS_CLUSTER/ PLUGIN/ 
      # Intended for expert use only!
      if ls $CARDSDIR/${name}*.patch; then
        echo "    WARNING: Applying custom user patch. I hope you know what you're doing!"
        cat $CARDSDIR/${name}*.patch | patch -p1
      fi

      # Copy bias module (cp3.irmp.ucl.ac.be/projects/madgraph/wiki/LOEventGenerationBias)
      # Expected structure: 
      # $CARDSDIR/BIAS/{module_name}/...
      #     .../makefile (mandatory)
      #     .../{module_name}.f (mandatory)
      #     .../bias_dependencies (optional)
      if [ -e $CARDSDIR/BIAS ]; then
        echo "copying bias module folder. Current dir:"
        pwd
        ls -lrth
        cp -r $CARDSDIR/BIAS/* $MGBASEDIRORIG/Template/LO/Source/BIAS
      fi
    
      LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`
    
      LHAPDFINCLUDES=`$LHAPDFCONFIG --incdir`
      LHAPDFLIBS=`$LHAPDFCONFIG --libdir`
    
      echo "set auto_update 0" > mgconfigscript
      echo "set automatic_html_opening False" >> mgconfigscript
      echo "set auto_convert_model True" >> mgconfigscript
      if [ $iscmsconnect -gt 0 ]; then
        echo "set output_dependencies internal" >> mgconfigscript
      fi
    #  echo "set output_dependencies internal" >> mgconfigscript
      echo "set lhapdf_py3 $LHAPDFCONFIG" >> mgconfigscript
    #   echo "set ninja $PWD/HEPTools/lib" >> mgconfigscript
    
      if [ "$queue" == "local" ]; then
          echo "set run_mode 2" >> mgconfigscript
      elif [ "$queue" == "pdmv" ]; then
          echo "set run_mode 2" >> mgconfigscript
	  echo "set nb_core $NB_CORE" >> mgconfigscript
      else
          #suppress lsf emails
          export LSB_JOB_REPORT_MAIL="N"
      
          echo "set run_mode  1" >> mgconfigscript
          if [ "$queue" == "condor" ]; then
            echo "set cluster_type cms_condor" >> mgconfigscript
            echo "set cluster_queue None" >> mgconfigscript
          elif [ "$queue" == "condor_spool" ]; then
            echo "set cluster_type cms_condor_spool" >> mgconfigscript
            echo "set cluster_queue None" >> mgconfigscript
          else
            echo "set cluster_type cms_lsf" >> mgconfigscript
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
    
      echo "save options --all" >> mgconfigscript
    
      ./bin/mg5_aMC mgconfigscript
    
      #load extra models if needed
      if [ -e $CARDSDIR/${name}_extramodels.dat ]; then
        echo "Loading extra models specified in $CARDSDIR/${name}_extramodels.dat"
        #strip comments
        sed 's:#.*$::g' $CARDSDIR/${name}_extramodels.dat | while read -r model || [ -n "$model" ]
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

      cp $CARDSDIR/${name}_proc_card.dat ${name}_proc_card.dat
      
      #*FIXME* workaround for broken cluster_local_path & lhapdf_py3 handling.
      # This needs to happen before the code-generation step, as fortran templates
      # are modified based on this parameter.
      echo "cluster_local_path = `${LHAPDFCONFIG} --datadir`" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt 
      echo "lhapdf_py3 = $LHAPDFCONFIG" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
    
      ########################
      #Run the code-generation step to create the process directory
      ########################
    
      sed -i '$ a display multiparticles' ${name}_proc_card.dat

      #check if MadSTR plugin is needed (DR/DS removal,  https://arxiv.org/pdf/1907.04898.pdf)
      runMadSTR=`grep "istr" $CARDSDIR/${name}_run_card.dat | grep -v "#" |  cut -d  "="  -f 1`
      if [ -z ${runMadSTR} ] ; then
          runMadSTR=0 # plugin settings not found in run_card
      else
          if [ "${runMadSTR}" -lt 1 ] || [ "${runMadSTR}" -gt 6 ] ; then
              echo "istr should be between 1 and 6" # wrong settings 
              exit 1
	  fi
      fi
      if [  "$runMadSTR" == 0 ]; then 
	  ./$MGBASEDIRORIG/bin/mg5_aMC ${name}_proc_card.dat # normal run without plugin 
      else
	  echo "Invoke MadSTR plugin when starting MG5_aMC@NLO" 
	  cp -r $PRODHOME/PLUGIN/MadSTR $MGBASEDIRORIG/PLUGIN/ # copy plugin 
          ./$MGBASEDIRORIG/bin/mg5_aMC --mode=MadSTR ${name}_proc_card.dat # run invoking MadSTR plugin
      fi
	
      is5FlavorScheme=0
      if tail -n 999 $LOGFILE | grep -q -e "^p *=.*b\~.*b" -e "^p *=.*b.*b\~"; then 
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
         echo "cluster_type = cms_condor" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
       elif [ "$queue" == "condor_spool" ]; then
         echo "cluster_queue = None" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
         echo "run_mode = 1" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
         echo "cluster_type = cms_condor_spool" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
       fi
    
      # Previous settings get erased after
      # code-generation mg5_aMC execution, set it up again before the integrate step.
      echo "cluster_local_path = `${LHAPDFCONFIG} --datadir`" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
      echo "lhapdf_py3 = $LHAPDFCONFIG" >> ./$MGBASEDIRORIG/input/mg5_configuration.txt
      
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
        if grep -q -e "^p *=.*b\~.*b" -e "^p *=.*b.*b\~" $LOGFILE_NAME*.log; then 
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

      # need to source condor once more if the codegen & integrate steps are separated
      if [[ $queue == *"condor"* ]]; then
        echo "Use HTCondor for gridpack generation"
        source ${PRODHOME}/Utilities/source_condor.sh
      fi

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
    
    #automatically detect NLO mode or LO mode from output directory
    isnlo=0
    if [ -e ./MCatNLO ]; then
      isnlo=1
    fi
    
    #################################
    #Add PDF info and copy run card #
    #################################
    script_dir="${PRODHOME}/Utilities/scripts"
    if [ ! -d "$script_dir" ]; then
      if ! [ -x "$(command -v git)" ]; then
        script_dir=${PRODHOME%genproductions*}/genproductions/Utilities/scripts
      else
        script_dir=$(git rev-parse --show-toplevel)/Utilities/scripts
      fi
    fi
    
    prepare_run_card $name $CARDSDIR $is5FlavorScheme $script_dir $isnlo
    
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
   
    if [ -e $CARDSDIR/${name}_param_card.dat ]; then
      echo "copying custom params file"
      cp $CARDSDIR/${name}_param_card.dat ./Cards/param_card.dat
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
      echo "reweight=OFF" >> makegrid.dat
      echo "done" >> makegrid.dat
      if [ -e $CARDSDIR/${name}_customizecards.dat ]; then
              cat $CARDSDIR/${name}_customizecards.dat | sed '/^$/d;/^#.*$/d' >> makegrid.dat
              echo "" >> makegrid.dat
      fi
      echo "done" >> makegrid.dat

      cat makegrid.dat | ./bin/generate_events -n pilotrun
      # Run this step separately in debug mode since it gives so many problems
      if [ -e $CARDSDIR/${name}_reweight_card.dat ]; then
          echo "preparing reweighting step"
          prepare_reweight $isnlo $WORKDIR $scram_arch $CARDSDIR/${name}_reweight_card.dat
	  extract_width $isnlo $WORKDIR $CARDSDIR ${name}
      fi
      
      echo "finished pilot run"
      cd $WORKDIR/processtmp
    
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
              cat $CARDSDIR/${name}_customizecards.dat | sed '/^$/d;/^#.*$/d' >> makegrid.dat
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
      rm -rf processtmp
      mkdir process
      cd process
      echo "unpacking temporary gridpack"
      tar -xzf $WORKDIR/pilotrun_gridpack.tar.gz
      echo "cleaning temporary gridpack"
      rm $WORKDIR/pilotrun_gridpack.tar.gz

      # as of mg29x, it does not generate any event if 'True = gridpack' in the run card
      # generate a few events manually
      ./run.sh 1000 234567 # nevents seed
      mv events.lhe.gz $WORKDIR/unweighted_events.lhe.gz

      # precompile reweighting if necessary
      if [ -e $CARDSDIR/${name}_reweight_card.dat ]; then
          echo "preparing reweighting step"
          prepare_reweight $isnlo $WORKDIR $scram_arch $CARDSDIR/${name}_reweight_card.dat
	  extract_width $isnlo $WORKDIR $CARDSDIR ${name}
      fi
      
      #prepare madspin grids if necessary
      if [ -e $CARDSDIR/${name}_madspin_card.dat ]; then
        echo "import $WORKDIR/unweighted_events.lhe.gz" > madspinrun.dat
        cat $CARDSDIR/${name}_madspin_card.dat >> madspinrun.dat
        $WORKDIR/$MGBASEDIRORIG/MadSpin/madspin madspinrun.dat 
        rm madspinrun.dat
        rm -rf tmp*
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
    
    pdfSysArgs=$(python3 ${script_dir}/getMG5_aMC_PDFInputs.py -f systematics -c run3 $pdfExtraArgs)
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

    # copy merge.pl from Utilities to allow merging LO events
    cd $WORKDIR/gridpack
    cp $PRODHOME/Utilities/merge.pl . 

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

# sync default cmssw with the current OS 
export SYSTEM_RELEASE=`cat /etc/redhat-release`
echo $SYSTEM_RELEASE

# set scram_arch 
if [ -n "$5" ]; then
    scram_arch=${5}
else
    if [[ $SYSTEM_RELEASE == *"release 7"* ]]; then 
        scram_arch=slc7_amd64_gcc10 
    elif [[ $SYSTEM_RELEASE == *"release 8"* ]]; then
        scram_arch=el8_amd64_gcc10
    elif [[ $SYSTEM_RELEASE == *"release 9"* ]]; then
        scram_arch=el9_amd64_gcc11
    else 
        echo "No default scram_arch for current OS!"
        if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi        
    fi
fi

#set cmssw 
if [ -n "$6" ]; then
    cmssw_version=${6}
else
    if [[ $SYSTEM_RELEASE == *"release 7"* ]]; then 
        cmssw_version=CMSSW_12_4_8
    elif [[ $SYSTEM_RELEASE == *"release 8"* ]]; then
        cmssw_version=CMSSW_12_4_8
    elif [[ $SYSTEM_RELEASE == *"release 9"* ]]; then
	cmssw_version=CMSSW_13_2_9
    else 
        echo "No default CMSSW for current OS!"
        if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi        
    fi
fi
 
# jobstep can be 'ALL','CODEGEN', 'INTEGRATE', 'MADSPIN'

if [ -z "$PRODHOME" ]; then
  PRODHOME=`pwd`
fi 

# Folder structure is different on CMSConnect
helpers_dir=${PRODHOME%genproductions*}/genproductions/Utilities
helpers_file=${helpers_dir}/gridpack_helpers.sh
if [ ! -f "$helpers_file" ]; then
  if [ -f "${PRODHOME}/Utilities/gridpack_helpers.sh" ]; then
    helpers_dir=${PRODHOME}/Utilities
  else
    helpers_dir=$(git rev-parse --show-toplevel)/bin/MadGraph5_aMCatNLO/Utilities
  fi
  helpers_file=${helpers_dir}/gridpack_helpers.sh
fi
source ${helpers_file}


if [ ! -z ${CMSSW_BASE} ]; then
  echo "Error: This script must be run in a clean environment as it sets up CMSSW itself.  You already have a CMSSW environment set up for ${CMSSW_VERSION}."
  echo "Please try again from a clean shell."
  if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
fi

#catch unset variables
set -u

if [ -z ${carddir} ]; then
    echo "Card directory not provided"
fi

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
    if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 1; else exit 1; fi
fi 

#For correct running you should place at least the run and proc card in a folder under the name "cards" in the same folder where you are going to run the script
RUNHOME=`pwd`

if [[ `uname -a` == *"lxplus"* ]]; then
  if [[ $RUNHOME == *"/eos/home-"* ]]; then
      echo "Running in /eos/home-X/~ which is not really stable. Use /eos/user/X/ instead."
      exit 1;
      #Preventing the use of /eos/home-X/ solely based on experience! Might not be a REAL problem.
  fi
fi

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
    # Make PIPESTATUS inherit make_gridpack return/exit codes
    set -o pipefail
    # Do not exit main shell if make_gridpack fails. We want to return rather than exit if we are sourcing this script.
    set +e
    make_gridpack |& tee $LOGFILE
    pipe_status=$PIPESTATUS
    # tee above will create a subshell, so exit calls inside function will just affect that subshell instance and return the exitcode in this shell.
    # This breaks cases when the calls inside make_gridpack try to exit the main shell with some error, hence not having the gridpack directory.
    # or not wanting to create a tarball.
    # Explicitely return or exit accordingly if make_gridpack returned an error.
    if [ $pipe_status -ne 0 ]; then
      if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return $pipe_status; else exit $pipe_status; fi
    fi
    # Also, step CODEGEN will try to exit with 0, but that will only affect the subshell.
    # Explicitely exit the main shell (or return 0 if sourced) if jobstep == CODEGEN.
    if [ "$jobstep" == "CODEGEN" ]; then
      if [ "${BASH_SOURCE[0]}" != "${0}" ]; then return 0; else exit 0; fi
    fi
    # Re-enable set -e
    set -e

    echo "Saving log file(s)"
    cd $WORKDIR/gridpack
    for i in ${LOGFILE_NAME}*.log; do 
        cp $i ${i/$LOGFILE_NAME/gridpack_generation}
    done
else
    make_gridpack
fi

make_tarball
