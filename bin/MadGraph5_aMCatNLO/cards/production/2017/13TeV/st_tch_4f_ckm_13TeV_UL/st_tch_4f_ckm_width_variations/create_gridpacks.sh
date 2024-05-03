#!/usr/bin/env zsh

# Run from bin/MadGraph5_aMCatNLO/ as ./cards/production/2017/13TeV/st_tch_4f_ckm_13TeV_UL/st_tch_4f_ckm_width_variations/create_gridpacks.sh

git apply cards/production/2017/13TeV/st_tch_4f_ckm_13TeV_UL/st_tch_4f_ckm_width_variations/gridpack_generation_topwidth.patch

NOMINAL_WIDTH=1.31 # NLO width, as in TT_hvq cards
WIDTH_VARIATIONS=(0.55 0.7 0.85 1.0 1.15 1.3 1.45)

for variation in ${WIDTH_VARIATIONS[@]}; do
    for charge in top antitop; do
        export TOPWIDTH=$((${NOMINAL_WIDTH} * ${variation}))
        nominal_process_name=st_tch_4f_ckm_${charge}_13TeV_UL_powheg_madspin
        process_name="st_tch_4f_ckm_${charge}_13TeV_wtop$(sed 's/\./p/' <<< ${variation})_UL_powheg_madspin"

        echo "Generating gridpack for ${charge}, variation ${variation} -> width=${TOPWIDTH}, name=${process_name}"

        card_dir=${process_name}_cards
        mkdir ${card_dir}
        cp cards/production/2017/13TeV/st_tch_4f_ckm_13TeV_UL/${nominal_process_name}/* ${card_dir}
        pushd ${card_dir}
        for f in *; do
            mv $f $(sed "s/${nominal_process_name}/${process_name}/" <<< $f)
        done
        sed -i "s/${nominal_process_name}/${process_name}/" ${process_name}_proc_card.dat
        popd

        ./gridpack_generation.sh ${process_name} ${card_dir}
    done
done
