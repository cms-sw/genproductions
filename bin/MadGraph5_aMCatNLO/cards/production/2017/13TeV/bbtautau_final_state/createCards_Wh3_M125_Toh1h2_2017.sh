input="non_cascade_masspoints.txt"

template_dir="Wh3_M125_Toh1h2/non_cascade/" 

# loop over all masspoints
while IFS=", " read m1 m2;
do  
    dir="Wh3_M125_Toh1h2_M${m1}_M${m2}/"
    dir="$template_dir$dir"
 
    mkdir -p "$dir"
    cp "${template_dir}Wh3_M125_Toh1h2_extramodels.dat" "${dir}Wh3_M125_Toh1h2_M${m1}_M${m2}_extramodels.dat"
    cp "${template_dir}Wh3_M125_Toh1h2_proc_card.dat" "${dir}Wh3_M125_Toh1h2_M${m1}_M${m2}_proc_card.dat" 
    cp "${template_dir}Wh3_M125_Toh1h2_run_card.dat" "${dir}Wh3_M125_Toh1h2_M${m1}_M${m2}_run_card.dat" 
    cp "${template_dir}Wh3_M125_Toh1h2_customizecards.dat" "${dir}Wh3_M125_Toh1h2_M${m1}_M${m2}_customizecards.dat" 
    
    cd ${dir}
    sed -i "s/h1MASS/M${m1}/g" "Wh3_M125_Toh1h2_M${m1}_M${m2}_proc_card.dat"
    sed -i "s/h2MASS/M${m2}/g" "Wh3_M125_Toh1h2_M${m1}_M${m2}_proc_card.dat"
    sed -i "s/h1MASS/${m1}/g" "Wh3_M125_Toh1h2_M${m1}_M${m2}_customizecards.dat"
    sed -i "s/h2MASS/${m2}/g" "Wh3_M125_Toh1h2_M${m1}_M${m2}_customizecards.dat"
    cd ../../../
    echo the cards are created for $dir 
done < "$input"


