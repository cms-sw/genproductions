# script to create production cards
# run this inside 13p6TeV/pair_VLQ/PairVLQ_t23t23_Incl_NLO_narrow/

masspoints=(700 1000 1300 1600)
template_dir="PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400/"

dir="PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB400/"
mkdir -p "$dir"
cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_extramodels.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB400_extramodels.dat"
cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_proc_card.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB400_proc_card.dat"
sed -i "s/MXi2000/MXi3000/g" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB400_proc_card.dat"
cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_customizecards.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB400_customizecards.dat"
sed -i "s/900085 2000/900085 3000/g" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB400_customizecards.dat"
cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_run_card.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB400_run_card.dat"

# loop over all masspoints
for mp in "${masspoints[@]}"
do
    dir="PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB${mp}/"
    mkdir -p "$dir"
    cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_extramodels.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB${mp}_extramodels.dat"
    cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_proc_card.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB${mp}_proc_card.dat"
    sed -i "s/MB400/MB${mp}/g" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB${mp}_proc_card.dat"
    cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_customizecards.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB${mp}_customizecards.dat"
    sed -i "s/9000005 400/9000005 ${mp}/g" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB${mp}_customizecards.dat"
    cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_run_card.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB${mp}_run_card.dat"

    dir="PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB${mp}/"
    mkdir -p "$dir"
    cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_extramodels.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB${mp}_extramodels.dat"
    cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_proc_card.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB${mp}_proc_card.dat"
    sed -i "s/MB400/MB${mp}/g" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB${mp}_proc_card.dat"
    sed -i "s/MXi2000/MXi3000/g" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB${mp}_proc_card.dat"
    cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_customizecards.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB${mp}_customizecards.dat"
    sed -i "s/9000005 400/9000005 ${mp}/g" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB${mp}_customizecards.dat"
    sed -i "s/900085 2000/900085 3000/g" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB${mp}_customizecards.dat"
    cp "${template_dir}PairVLQ_b13b13_btautau_LO_narrow_MXi2000_MB400_run_card.dat" "${dir}PairVLQ_b13b13_btautau_LO_narrow_MXi3000_MB${mp}_run_card.dat"


done
