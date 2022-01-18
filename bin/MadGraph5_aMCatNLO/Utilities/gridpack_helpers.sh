set_run_card_pdf () {
    name=$1
    CARDSDIR=$2
    is5FlavorScheme=$3
    script_dir=$4

    pdfExtraArgs=""
    if [ $is5FlavorScheme -eq 1 ]; then
        pdfExtraArgs+="--is5FlavorScheme "
    fi

    if grep -q -e "\$DEFAULT_PDF_SETS" $CARDSDIR/${name}_run_card.dat; then
        local central_set=$(python ${script_dir}/getMG5_aMC_PDFInputs.py -f "central" -c 2017 $pdfExtraArgs)
        echo "INFO: Using default PDF sets for 2017 production"

        sed "s/\$DEFAULT_PDF_SETS/${central_set}/g" $CARDSDIR/${name}_run_card.dat > ./Cards/run_card.dat
        sed -i "s/ *\$DEFAULT_PDF_MEMBERS.*=.*//g" ./Cards/run_card.dat
    elif grep -q -e "\$DEFAULT_PDF_SETS" -e "\$DEFAULT_PDF_MEMBERS" $CARDSDIR/${name}_run_card.dat; then
        local central_set=$(python ${script_dir}/getMG5_aMC_PDFInputs.py -f "central" -c 2016 $pdfExtraArgs)

        echo "INFO: Using default PDF sets for 2017 production"
        sed "s/\$DEFAULT_2016_PDF_SETS/${central_set}/g" $CARDSDIR/${name}_run_card.dat > ./Cards/run_card.dat
        sed -i "s/ *\$DEFAULT_PDF_MEMBERS.*=.*//g" ./Cards/run_card.dat
    else
        cat << EOF

        WARNING: You've chosen not to use the PDF sets recommended for 2017 production!
        If this isn't intentional, and you prefer to use the recommended sets,
        insert the following lines into your process-name_run_card.dat:

            '\$DEFAULT_PDF_SETS = lhaid'
            '\$DEFAULT_PDF_MEMBERS = reweight_PDF'
        
EOF
        echo "copying run_card.dat file"
        cp $CARDSDIR/${name}_run_card.dat ./Cards/run_card.dat
   fi
}

# Make some replacements in run card (mostly related to PDF)
# and copy to correct directory
# Args: <process_name> <cards directory> <is5FlavorScheme> 
prepare_run_card () {
    name=$1
    CARDSDIR=$2
    is5FlavorScheme=$3
    script_dir=$4
    isnlo=$5

    set_run_card_pdf $name $CARDSDIR $is5FlavorScheme $script_dir

    # Set maxjetflavor according to PDF scheme
    nFlavorScheme=5
    if [ $is5FlavorScheme -ne 1 ]; then
        nFlavorScheme=4
    fi

    if grep -Fxq "maxjetflavor" ./Cards/run_card.dat ; then
        sed -i "s/.*maxjetflavor.*/${nFlavorScheme}\ =\ maxjetflavor/" ./Cards/run_card.dat 
    else
        echo "${nFlavorScheme} = maxjetflavor" >> ./Cards/run_card.dat 
    fi

    if [ "$isnlo" -gt "0" ]; then # nlo mode
	echo "False = reweight_scale" >> ./Cards/run_card.dat 
	echo "False = reweight_PDF" >> ./Cards/run_card.dat 
	echo "True = store_rwgt_info" >> ./Cards/run_card.dat 
    else # lo mode 
	echo "None = systematics_program" >> ./Cards/run_card.dat 
	echo "True = use_syst" >> ./Cards/run_card.dat 
    fi
    
}

# Run reweight step, explicitly compiling subprocesses
prepare_reweight () { 
    isnlo=$1
    WORKDIR=$2 
    scram_arch=$3
    reweight_card=$4

    if ! head -20 $reweight_card | grep -q -e "change rwgt_dir \(\.\/\)\?rwgt"; then
        echo "ERROR: Reweight card must contain the line"
        echo "    'change rwgt_dir ./rwgt'"
        echo "Refer to examples in genproductions repository."
        echo "Exciting..."
        exit 1
    fi
    
    echo "preparing reweighting step"
    if [ "$isnlo" -gt "0" ]; then
        cd $WORKDIR/processtmp
        config=./Cards/amcatnlo_configuration.txt
    else
        cd $WORKDIR/process
	mkdir -p madevent/Events/pilotrun
        cp $WORKDIR/unweighted_events.lhe.gz madevent/Events/pilotrun
        cd madevent
        config=./madevent/Cards/me5_configuration.txt
    fi

    # No longer necessary in gcc6
    if [[ ${scram_arch} == *"gcc48"* ]]; then
        echo "f2py_compiler=" `which gfortran` >> $config
        #need to set library path or f2py won't find libraries
        export LIBRARY_PATH=$LD_LIBRARY_PATH
    fi

    # Use f2py2 instead of f2py to install a py2 version of "rwgt2py"
    # (occurs in CMSSW_10_6_19 where default f2py points to a py3 version)
    if [ -e $(readlink -f `which f2py`)2 ]; then
        echo "f2py_compiler="$(readlink -f `which f2py`)2 >> $config
    fi

    if [ "$isnlo" -gt "0" ]; then
        # Needed to get around python import errors
        rwgt_dir="$WORKDIR/process/rwgt"
        export PYTHONPATH=$rwgt_dir:$PYTHONPATH
        echo "0" | ./bin/aMCatNLO --debug reweight pilotrun
    else
        echo "0" | ./bin/madevent --debug reweight pilotrun
    fi

    # Explicitly compile all subprocesses to avoid
    # compilation on the cluster
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
}

# Extract decay width in reweighting
extract_width () {

    isnlo=$1
    wd=$2
    cdir=$3
    name=$4
        
    hasauto=$(grep "auto" ${cdir}/${name}_reweight_card.dat | egrep -v "#")
    
    if [[ ${hasauto} != "" ]]; then
      echo "extract computed widths and rewrite reweight card"

      if [ "$isnlo" -gt "0" ]; then
        cp ${wd}/processtmp/Events/pilotrun/events.lhe.gz temp.lhe.gz
        gzip -d temp.lhe.gz
	mgv=$(ls | grep 'MG5_aMC_v')
        ./${mgv}/Template/LO/bin/internal/extract_banner-pl temp.lhe banner.txt
      else
        cp ${wd}/process/madevent/Events/pilotrun/unweighted_events.lhe.gz temp.lhe.gz
        gzip -d temp.lhe.gz
        ./madevent/bin/internal/extract_banner-pl temp.lhe banner.txt
      fi
        IFSd=$IFS; IFS=$'\n'
        pd=($(grep "rwgt_" banner.txt | grep "decay" | sed "s%.*decay %%g" | sed "s% # orig.*%%g"))
        IFS=$IFSd
        rm banner.txt temp.lhe
        c=0
        rm -rf ${cdir}/${name}_reweight_card_temp.dat
        while IFS= read -r line; do
          linem=${line}
          hasauto=$(echo ${line} | grep "auto" | egrep -v "#")
          if [[ ${hasauto} != "" ]]; then
            t=(${pd[${c}]})
            linem=$(echo $linem | sed "s%auto%${t[1]}%g")
            c=$[$c+1]
          fi
          echo "$linem" >> ${cdir}/${name}_reweight_card_temp.dat
        done < ${cdir}/${name}_reweight_card.dat
        cp ${cdir}/${name}_reweight_card_temp.dat ${wd}/process/reweight_card.dat
        mv ${cdir}/${name}_reweight_card_temp.dat ${wd}/process/madevent/Cards/reweight_card.dat
    fi  
}
