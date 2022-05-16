# script to create production cards for 2016 bstar from example card (bstar_tW_Left_M700)

masspoints=(700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
base_dir="2017/13TeV/PairVLQ_b13b13_Incl_NLO_narrow/"
template_dir="2017/13TeV/PairVLQ_b13b13_Incl_NLO_narrow/PairVLQ_b13b13_Incl_NLO_narrow_M1000"

# loop over all masspoints
for mp in "${masspoints[@]}"
do
    dir="${base_dir}/PairVLQ_b13b13_Incl_NLO_narrow_M${mp}/"
    mkdir -p "$dir"
    cp "${template_dir}PairVLQ_b13b13_Incl_NLO_narrow_M1000_extramodels.dat" "${dir}PairVLQ_b13b13_Incl_NLO_narrow_M${mp}_extramodels.dat"
    cp "${template_dir}PairVLQ_b13b13_Incl_NLO_narrow_M1000_proc_card.dat" "${dir}PairVLQ_b13b13_Incl_NLO_narrow_M${mp}_proc_card.dat"
    sed -i "s/M1000/_M${mp}/g" "${dir}PairVLQ_b13b13_Incl_NLO_narrow_M${mp}_proc_card.dat"
    cp "${template_dir}PairVLQ_b13b13_Incl_NLO_narrow_M1000_run_card.dat" "${dir}PairVLQ_b13b13_Incl_NLO_narrow_M${mp}_run_card.dat"
    cp "${template_dir}PairVLQ_b13b13_Incl_NLO_narrow_M1000_madspin_card.dat" "${dir}PairVLQ_b13b13_Incl_NLO_narrow_M${mp}_madspin_card.dat"
    cp "${template_dir}PairVLQ_b13b13_Incl_NLO_narrow_M1000_param_card.dat" "${dir}PairVLQ_b13b13_Incl_NLO_narrow_M${mp}_param_card.dat"
    root -l -b -q "writeCards.C(${mp},\"${dir}\")"
done
