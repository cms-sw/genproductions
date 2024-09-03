# script to create production cards
# run this inside 13p6TeV/pair_VLQ/PairVLQ_t23t23_Incl_NLO_narrow/

masspoints=(600 800 1000 1200 1400 1600)
template_dir="PairVLQ_t23t23_toptautau_LO_narrow_M400/"

# loop over all masspoints
for mp in "${masspoints[@]}"
do
    dir="PairVLQ_t23t23_toptautau_LO_narrow_M${mp}/"
    mkdir -p "$dir"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_M400_extramodels.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_M${mp}_extramodels.dat"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_M400_proc_card.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_M${mp}_proc_card.dat"
    sed -i "s/M400/M${mp}/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_M${mp}_proc_card.dat"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_M400_customizecards.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_M${mp}_customizecards.dat"
    sed -i "s/9000006 400/9000006 ${mp}/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_M${mp}_customizecards.dat"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_M400_run_card.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_M${mp}_run_card.dat"
done
