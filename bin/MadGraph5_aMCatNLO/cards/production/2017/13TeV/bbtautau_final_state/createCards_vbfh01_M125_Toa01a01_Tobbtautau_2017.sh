masspoints=(12 15 20 25 30 35 40 45 50 55 60)

template_dir="vbfh01_M125_Toa01a01_Tobbtautau/" 

# loop over all masspoints
for mp in "${masspoints[@]}"
do  
    dir="vbfh01_M125_Toa01a01_M${mp}_Tobbtautau/"
    dir="$template_dir$dir"
 
    mkdir -p "$dir"
    cp "${template_dir}vbfh01_M125_Toa01a01_Tobbtautau_extramodels.dat" "${dir}vbfh01_M125_Toa01a01_M${mp}_Tobbtautau_extramodels.dat"
    cp "${template_dir}vbfh01_M125_Toa01a01_Tobbtautau_proc_card.dat" "${dir}vbfh01_M125_Toa01a01_M${mp}_Tobbtautau_proc_card.dat" 
    cp "${template_dir}vbfh01_M125_Toa01a01_Tobbtautau_run_card.dat" "${dir}vbfh01_M125_Toa01a01_M${mp}_Tobbtautau_run_card.dat" 
    cp "${template_dir}vbfh01_M125_Toa01a01_Tobbtautau_customizecards.dat" "${dir}vbfh01_M125_Toa01a01_M${mp}_Tobbtautau_customizecards.dat" 
    
    cd ${dir}
    sed -i "s/A0MASS/M${mp}/g" "vbfh01_M125_Toa01a01_M${mp}_Tobbtautau_proc_card.dat"
    sed -i "s/A0MASS/${mp}/g" "vbfh01_M125_Toa01a01_M${mp}_Tobbtautau_customizecards.dat"
    cd ../../
    echo the cards are created for $dir 
done
