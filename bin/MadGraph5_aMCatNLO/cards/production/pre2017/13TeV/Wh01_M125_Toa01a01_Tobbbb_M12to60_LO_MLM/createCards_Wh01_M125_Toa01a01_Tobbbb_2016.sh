masspoints=(12 15 20 25 30 40 50 60)
base_dir="pre2017/13TeV"
#template_dir="${base_dir}/Wh01_M125_Toa01a01_Tobbbb/"
template_dir="Wh01_M125_Toa01a01_Tobbbb_LO_MLM/" 

# loop over all masspoints
for mp in "${masspoints[@]}"
do
    dir="Wh01_M125_Toa01a01_Tobbbb_M${mp}/"
    mkdir -p "$dir"
    cp "${template_dir}Wh01_M125_Toa01a01_Tobbbb_extramodels.dat" "${dir}Wh01_M125_Toa01a01_Tobbbb_M${mp}_extramodels.dat"
    cp "${template_dir}Wh01_M125_Toa01a01_Tobbbb_proc_card.dat" "${dir}Wh01_M125_Toa01a01_Tobbbb_M${mp}_proc_card.dat" 
    cp "${template_dir}Wh01_M125_Toa01a01_Tobbbb_run_card.dat" "${dir}Wh01_M125_Toa01a01_Tobbbb_M${mp}_run_card.dat" 
    cp "${template_dir}Wh01_M125_Toa01a01_Tobbbb_customizecards.dat" "${dir}Wh01_M125_Toa01a01_Tobbbb_M${mp}_customizecards.dat" 
    
    cd ${dir}
    sed -i "s/A0MASS/M${mp}/g" "Wh01_M125_Toa01a01_Tobbbb_M${mp}_proc_card.dat"
    sed -i "s/A0MASS/${mp}/g" "Wh01_M125_Toa01a01_Tobbbb_M${mp}_customizecards.dat"
    cd ../
done
