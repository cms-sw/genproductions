# script to create production cards for 2016 bstar from example card (bstar_tW_Left_M700)

masspoints=(600 800 1200 1300 1400 1500 1600 1700 1800 2000 2200 2400)
base_dir="2017/13TeV/BprimeTToTW_LH/"
template_dir="2017/13TeV/BprimeTToTW_LH/BprimeTToTW_LH_narrowLO_M1000/"

# loop over all masspoints
for mp in "${masspoints[@]}"
do
    dir="${base_dir}BprimeTToTW_LH_narrowLO_M${mp}/"
    mkdir -p "$dir"
    cp "${template_dir}BprimeTToTW_LH_narrowLO_M1000_extramodels.dat" "${dir}BprimeTToTW_LH_narrowLO_M${mp}_extramodels.dat"
    cp "${template_dir}BprimeTToTW_LH_narrowLO_M1000_proc_card.dat" "${dir}BprimeTToTW_LH_narrowLO_M${mp}_proc_card.dat"
    sed -i "s/M1000/M${mp}/g" "${dir}BprimeTToTW_LH_narrowLO_M${mp}_proc_card.dat"
    cp "${template_dir}BprimeTToTW_LH_narrowLO_M1000_run_card.dat" "${dir}BprimeTToTW_LH_narrowLO_M${mp}_run_card.dat"
    cp "${template_dir}BprimeTToTW_LH_narrowLO_M1000_madspin_card.dat" "${dir}BprimeTToTW_LH_narrowLO_M${mp}_madspin_card.dat"
    #cp "${template_dir}BprimeTToTW_LH_narrowLO_M1000_param_card.dat" "${dir}BprimeTToTW_LH_narrowLO_M${mp}_param_card.dat"
    root -l -b -q "${base_dir}/writeCards.C(${mp},\"${dir}\")"
done
