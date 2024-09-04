# script to create production cards
# run this inside 13p6TeV/pair_VLQ/PairVLQ_t23t23_Incl_NLO_narrow/

masspoints=(700 1000 1300 1600)
template_dir="PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400/"

dir="PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT400/"
mkdir -p "$dir"
cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_extramodels.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT400_extramodels.dat"
cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_proc_card.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT400_proc_card.dat"
sed -i "s/MXi2000/MXi3000/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT400_proc_card.dat"
cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_customizecards.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT400_customizecards.dat"
sed -i "s/9000035 2000/9000035 3000/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT400_customizecards.dat"
cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_run_card.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT400_run_card.dat"

# loop over all masspoints
for mp in "${masspoints[@]}"
do
    dir="PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT${mp}/"
    mkdir -p "$dir"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_extramodels.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT${mp}_extramodels.dat"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_proc_card.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT${mp}_proc_card.dat"
    sed -i "s/MT400/MT${mp}/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT${mp}_proc_card.dat"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_customizecards.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT${mp}_customizecards.dat"
    sed -i "s/9000006 400/9000006 ${mp}/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT${mp}_customizecards.dat"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_run_card.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT${mp}_run_card.dat"

    dir="PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT${mp}/"
    mkdir -p "$dir"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_extramodels.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT${mp}_extramodels.dat"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_proc_card.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT${mp}_proc_card.dat"
    sed -i "s/MT400/MT${mp}/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT${mp}_proc_card.dat"
    sed -i "s/MXi2000/MXi3000/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT${mp}_proc_card.dat"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_customizecards.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT${mp}_customizecards.dat"
    sed -i "s/9000006 400/9000006 ${mp}/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT${mp}_customizecards.dat"
    sed -i "s/9000035 2000/9000035 3000/g" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT${mp}_customizecards.dat"
    cp "${template_dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi2000_MT400_run_card.dat" "${dir}PairVLQ_t23t23_toptautau_LO_narrow_MXi3000_MT${mp}_run_card.dat"


done
