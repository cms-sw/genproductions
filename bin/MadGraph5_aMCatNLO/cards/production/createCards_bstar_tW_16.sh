# script to create production cards for 2016 bstar from example card (bstar_tW_Left_M700)

masspoints=( 700 800 900 1000 1100 )
couplings=( "Left" "Right" )
base_dir="pre2017/13TeV"
template_dir="${base_dir}/bstar_tW_Left_M700/"

# loop over all masspoints
for handedness in "${couplings[@]}"
do
    for mp in "${masspoints[@]}"
    do
	dir="${base_dir}/bstar_tW_${handedness}_M${mp}/"
	mkdir -p "$dir"
	cp "${template_dir}bstar_tW_Left_M700_extramodels.dat" "${dir}bstar_tW_${handedness}_M${mp}_extramodels.dat"
	cp "${template_dir}bstar_tW_Left_M700_proc_card.dat" "${dir}bstar_tW_${handedness}_M${mp}_proc_card.dat"
	sed -i "s/Left_M1200/${handedness}_M${mp}/g" "${dir}bstar_tW_${handedness}_M${mp}_proc_card.dat"
	cp "${template_dir}bstar_tW_Left_M700_run_card.dat" "${dir}bstar_tW_${handedness}_M${mp}_run_card.dat"
	root -l -b -q "writeCards.C(${mp},\"${handedness}\",\"${dir}\")"
    done
done