input="cascade_masspoints.txt"

template_dir="ggh3_M125_Toh1h2/cascade/" 

# loop over all masspoints
while IFS=", " read m1 m2;
do  
    dir="ggh3_M125_Toh1h2_M${m1}_M${m2}_h2_Toh1h1/"
    dir="$template_dir$dir"
 
    mkdir -p "$dir"
    cp "${template_dir}ggh3_M125_Toh1h2_h2_Toh1h1_extramodels.dat" "${dir}ggh3_M125_Toh1h2_M${m1}_M${m2}_h2_Toh1h1_extramodels.dat"
    cp "${template_dir}ggh3_M125_Toh1h2_h2_Toh1h1_proc_card.dat" "${dir}ggh3_M125_Toh1h2_M${m1}_M${m2}_h2_Toh1h1_proc_card.dat" 
    cp "${template_dir}ggh3_M125_Toh1h2_h2_Toh1h1_run_card.dat" "${dir}ggh3_M125_Toh1h2_M${m1}_M${m2}_h2_Toh1h1_run_card.dat" 
    cp "${template_dir}ggh3_M125_Toh1h2_h2_Toh1h1_customizecards.dat" "${dir}ggh3_M125_Toh1h2_M${m1}_M${m2}_h2_Toh1h1_customizecards.dat" 
    
    cd ${dir}
    sed -i "s/h1MASS/M${m1}/g" "ggh3_M125_Toh1h2_M${m1}_M${m2}_h2_Toh1h1_proc_card.dat"
    sed -i "s/h2MASS/M${m2}/g" "ggh3_M125_Toh1h2_M${m1}_M${m2}_h2_Toh1h1_proc_card.dat"
    sed -i "s/h1MASS/${m1}/g" "ggh3_M125_Toh1h2_M${m1}_M${m2}_h2_Toh1h1_customizecards.dat"
    sed -i "s/h2MASS/${m2}/g" "ggh3_M125_Toh1h2_M${m1}_M${m2}_h2_Toh1h1_customizecards.dat"
    cd ../../../
    echo the cards are created for $dir 
done < "$input"


