#!/bin/bash

echo `pwd`
echo 'set up ht-binned cards'

#1000 will be replaced by infinite
Ht_array=(0 400 600 800 1000)
mkdir HT-binned
for((i=0;i<${#Ht_array[*]}-1;i++))
do
        bin_low=${Ht_array[i]}
        bin_high=${Ht_array[i+1]}
        if [[ "$bin_low" -eq "${Ht_array[3]}" ]];then
                mkdir -p HT-binned/HT-${bin_low}_inf
                cp ./inc/*proc* HT-binned/HT-${bin_low}_inf/WToQQ01234j_5f_LO_MLM_HT-${bin_low}toInf_proc_card.dat
                sed -i "13s/WToQQ01234j_5f_LO_MLM/WToQQ01234j_5f_LO_MLM_HT-${bin_low}toInf/g" HT-binned/HT-${bin_low}_inf/*proc_card.dat
                cp ./inc/*run* HT-binned/HT-${bin_low}_inf/WToQQ01234j_5f_LO_MLM_HT-${bin_low}toInf_run_card.dat
                sed -i "225s/0.0/$bin_low.0/g" HT-binned/HT-${bin_low}_inf/*run_card.dat
        else
                mkdir -p HT-binned/HT-${bin_low}_${bin_high}
                cp ./inc/*proc* HT-binned/HT-${bin_low}_${bin_high}/WToQQ01234j_5f_LO_MLM_HT-${bin_low}to${bin_high}_proc_card.dat
                sed -i "13s/WToQQ01234j_5f_LO_MLM/WToQQ01234j_5f_LO_MLM_HT-${bin_low}to${bin_high}/g" HT-binned/HT-${bin_low}_${bin_high}/*proc_card.dat
                cp ./inc/*run* HT-binned/HT-${bin_low}_${bin_high}/WToQQ01234j_5f_LO_MLM_HT-${bin_low}to${bin_high}_run_card.dat
                sed -i "225s/0.0/$bin_low.0/g" HT-binned/HT-${bin_low}_${bin_high}/*run_card.dat
                sed -i "226s/-1.0/$bin_high.0/g" HT-binned/HT-${bin_low}_${bin_high}/*run_card.dat
        fi
done
