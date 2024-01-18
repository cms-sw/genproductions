for i in 15 20 25 30 35 40 45 50 55 60
do
    mkdir "${i}_GeV"

    cp hToaaTo4gamma_ma_AMASS_GeV_MLM_4f_max1j_customizecards.dat "${i}_GeV/hToaaTo4gamma_ma_${i}_GeV_MLM_4f_max1j_customizecards.dat"
    cp hToaaTo4gamma_ma_AMASS_GeV_MLM_4f_max1j_extramodels.dat "${i}_GeV/hToaaTo4gamma_ma_${i}_GeV_MLM_4f_max1j_extramodels.dat"
    cp hToaaTo4gamma_ma_AMASS_GeV_MLM_4f_max1j_proc_card.dat "${i}_GeV/hToaaTo4gamma_ma_${i}_GeV_MLM_4f_max1j_proc_card.dat"
    cp hToaaTo4gamma_ma_AMASS_GeV_MLM_4f_max1j_run_card.dat "${i}_GeV/hToaaTo4gamma_ma_${i}_GeV_MLM_4f_max1j_run_card.dat"

    printf -v zd '%.6e' $((2 * $i))
    printf -v a '%.6e' $(($i))
    sed -i "s/_ZDMASS_/${zd}/" "${i}_GeV/hToaaTo4gamma_ma_${i}_GeV_MLM_4f_max1j_customizecards.dat"
    sed -i "s/_AMASS_/${a}/" "${i}_GeV/hToaaTo4gamma_ma_${i}_GeV_MLM_4f_max1j_customizecards.dat"
    sed -i "s/AMASS/${i}/" "${i}_GeV/hToaaTo4gamma_ma_${i}_GeV_MLM_4f_max1j_proc_card.dat"
done