#!/bin/bash

echo `pwd`
echo 'set up ht-binned cards'

#3000 will be replaced by infinite
Ht_array=(0 50 100 200 400 600 800 3000)
# Ht_array=(0 80 130 230 430 630 830 3000)
# Ht_array=(0 40 70 100 200 400 600 3000)

for((i=0;i<${#Ht_array[*]}-1;i++))
do
        bin_low=$[${Ht_array[i]}-30]
        bin_high=$[${Ht_array[i+1]}+30]
        if [[ "$bin_low" -eq 3030 ]];then
                mkdir HT-${bin_low}_inf
                cp ../dy012jfxfx_inc/*custo* HT-${bin_low}_inf/DYJets_HT-${bin_low}toInf_customizecards.dat
                cp ../dy012jfxfx_inc/*proc* HT-${bin_low}_inf/DYJets_HT-${bin_low}toInf_proc_card.dat
                sed -i "9s/dyellell012j_5f_NLO_FXFX/DYJets_HT-${bin_low}toInf/g" HT-${bin_low}_inf/*proc_card.dat
                cp ../dy012jfxfx_inc/*run* HT-${bin_low}_inf/DYJets_HT-${bin_low}toInf_run_card.dat
                cp ../dy012jfxfx_inc/*cuts* HT-${bin_low}_inf/DYJets_HT-${bin_low}toInf_cuts.f
                sed -i "401s/0/$bin_low/g" HT-${bin_low}_inf/DYJets_HT-${bin_low}toInf_cuts.f
                sed -i "405s/gt/lt/g" HT-${bin_low}_inf/DYJets_HT-${bin_low}toInf_cuts.f
        else
                mkdir HT-${bin_low}_${bin_high}
                cp ../dy012jfxfx_inc/*custo* HT-${bin_low}_${bin_high}/DYJets_HT-${bin_low}to${bin_high}_customizecards.dat
                cp ../dy012jfxfx_inc/*proc* HT-${bin_low}_${bin_high}/DYJets_HT-${bin_low}to${bin_high}_proc_card.dat
                sed -i "9s/dyellell012j_5f_NLO_FXFX/DYJets_HT-${bin_low}to${bin_high}/g" HT-${bin_low}_${bin_high}/*proc_card.dat
                cp ../dy012jfxfx_inc/*run* HT-${bin_low}_${bin_high}/DYJets_HT-${bin_low}to${bin_high}_run_card.dat
                cp ../dy012jfxfx_inc/*cuts* HT-${bin_low}_${bin_high}/DYJets_HT-${bin_low}to${bin_high}_cuts.f
                sed -i "401s/0/$bin_low/g" HT-${bin_low}_${bin_high}/DYJets_HT-${bin_low}to${bin_high}_cuts.f
                sed -i "405s/40/$bin_high/g" HT-${bin_low}_${bin_high}/DYJets_HT-${bin_low}to${bin_high}_cuts.f
        fi
done
