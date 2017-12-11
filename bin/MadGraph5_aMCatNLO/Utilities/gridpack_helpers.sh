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
    set_run_card_pdf $1 $2 $3 $4

    # Set maxjetflavor according to PDF scheme
    is5FlavorScheme=$3
    nFlavorScheme=5
    if [ $is5FlavorScheme -ne 1 ]; then
        nFlavorScheme=4
    fi

    if grep -Fxq "maxjetflavor" ./Cards/run_card.dat ; then
        sed -i "s/.*maxjetflavor.*/${nFlavorScheme}\ =\ maxjetflavor/" ./Cards/run_card.dat 
    else
        echo "${nFlavorScheme} = maxjetflavor" >> ./Cards/run_card.dat 
    fi
}
