# script to create production cards
# run this inside 13p6TeV/pair_VLQ/PairVLQ_t23t23_Incl_NLO_narrow/

masspoints=(1300 1400 1500 1600 1700 1800 1900 2000)
template_dir="PairVLQ_t23t23_Incl_NLO_narrow_M1200/"

# loop over all masspoints
for mp in "${masspoints[@]}"
do
    dir="PairVLQ_t23t23_Incl_NLO_narrow_M${mp}/"
    mkdir -p "$dir"
    cp "${template_dir}PairVLQ_t23t23_Incl_NLO_narrow_M1200_extramodels.dat" "${dir}PairVLQ_t23t23_Incl_NLO_narrow_M${mp}_extramodels.dat"
    cp "${template_dir}PairVLQ_t23t23_Incl_NLO_narrow_M1200_proc_card.dat" "${dir}PairVLQ_t23t23_Incl_NLO_narrow_M${mp}_proc_card.dat"
    sed -i "s/M1200/M${mp}/g" "${dir}PairVLQ_t23t23_Incl_NLO_narrow_M${mp}_proc_card.dat"
    cp "${template_dir}PairVLQ_t23t23_Incl_NLO_narrow_M1200_customizecards.dat" "${dir}PairVLQ_t23t23_Incl_NLO_narrow_M${mp}_customizecards.dat"
    sed -i "s/6000006 1200/6000006 ${mp}/g" "${dir}PairVLQ_t23t23_Incl_NLO_narrow_M${mp}_customizecards.dat"
    cp "${template_dir}PairVLQ_t23t23_Incl_NLO_narrow_M1200_run_card.dat" "${dir}PairVLQ_t23t23_Incl_NLO_narrow_M${mp}_run_card.dat"
    cp "${template_dir}PairVLQ_t23t23_Incl_NLO_narrow_M1200_madspin_card.dat" "${dir}PairVLQ_t23t23_Incl_NLO_narrow_M${mp}_madspin_card.dat"
    cp "${template_dir}PairVLQ_t23t23_Incl_NLO_narrow_M1200_param_card.dat" "${dir}PairVLQ_t23t23_Incl_NLO_narrow_M${mp}_param_card.dat"
done
