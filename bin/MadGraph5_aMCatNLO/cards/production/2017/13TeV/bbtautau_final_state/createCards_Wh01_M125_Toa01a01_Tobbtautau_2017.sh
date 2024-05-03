masspoints=(15  30  45  50  60)

template_dir="Wh01_M125_Toa01a01_Tobbtautau/" 

# loop over all masspoints
for mp in "${masspoints[@]}"
do  
    dir="Wh01_M125_Toa01a01_M${mp}_Tobbtautau/"
    dir="$template_dir$dir"
 
    mkdir -p "$dir"
    cp "${template_dir}Wh01_M125_Toa01a01_Tobbtautau_extramodels.dat" "${dir}Wh01_M125_Toa01a01_M${mp}_Tobbtautau_extramodels.dat"
    cp "${template_dir}Wh01_M125_Toa01a01_Tobbtautau_proc_card.dat" "${dir}Wh01_M125_Toa01a01_M${mp}_Tobbtautau_proc_card.dat" 
    cp "${template_dir}Wh01_M125_Toa01a01_Tobbtautau_run_card.dat" "${dir}Wh01_M125_Toa01a01_M${mp}_Tobbtautau_run_card.dat" 
    cp "${template_dir}Wh01_M125_Toa01a01_Tobbtautau_customizecards.dat" "${dir}Wh01_M125_Toa01a01_M${mp}_Tobbtautau_customizecards.dat" 
    
    cd ${dir}
    sed -i "s/A0MASS/M${mp}/g" "Wh01_M125_Toa01a01_M${mp}_Tobbtautau_proc_card.dat"
    sed -i "s/A0MASS/${mp}/g" "Wh01_M125_Toa01a01_M${mp}_Tobbtautau_customizecards.dat"
    cd ../../
    echo the cards are created for $dir 
done
