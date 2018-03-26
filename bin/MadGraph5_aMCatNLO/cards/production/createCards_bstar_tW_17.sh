masspoints=( 700 800 900 1000 1100 1200 1400 1600 1800 2000 2200 2400 2600 2800 3000)
couplings=( "Left" "Right" )
base_dir="2017/13TeV"
template_dir="${base_dir}/bstar_tW_Left_M1200"

# loop over all masspoints
for handedness in "${couplings[@]}"
do
    for mp in "${masspoints[@]}"
    do
	dir="${base_dir}bstar_tW_${handedness}_M${mp}/"
	mkdir -p "$dir"
	cp "${template_dir}bstar_tW_Left_M1200_extramodels.dat" "${dir}bstar_tW_${handedness}_M${mp}_extramodels.dat"
	cp "${template_dir}bstar_tW_Left_M1200_proc_card.dat" "${dir}bstar_tW_${handedness}_M${mp}_proc_card.dat"
	sed -i "s/Left_M1200/${handedness}_M${mp}/g" "${dir}bstar_tW_${handedness}_M${mp}_proc_card.dat"
	cp "${template_dir}bstar_tW_Left_M1200_run_card.dat" "${dir}bstar_tW_${handedness}_M${mp}_run_card.dat"
	root -l -b -q "writeCards.C(${mp},\"${handedness}\",\"${dir}\")"
    done
done