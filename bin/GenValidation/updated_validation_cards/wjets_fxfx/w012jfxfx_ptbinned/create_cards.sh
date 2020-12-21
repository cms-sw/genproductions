#!/bin/bash

echo `pwd`
echo 'set up pt-binned cards'

#1000 will be replaced by infinite
Pt_array=(0 50 100 250 400 600 1000)

for((i=0;i<${#Pt_array[*]}-1;i++))
do
	bin_low=${Pt_array[i]}
	bin_high=${Pt_array[i+1]}
	if [[ "$bin_low" -eq "${Pt_array[5]}" ]];then
     	mkdir pt${bin_low}_inf
        cp ../w012jfxfx_inc/*custo* pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_customizecards.dat
		cp ../w012jfxfx_inc/*madspin* pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_madspin_card.dat
		cp ../w012jfxfx_inc/*run* pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_run_card.dat
		cp ../w012jfxfx_inc/*proc* pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_proc_card.dat
		sed -i "16s/wellnu012j_5f_NLO_FXFX/wellnu012j_wpt-${bin_low}toinf/g" pt${bin_low}_inf/*proc_card.dat
		cp ./wpt-0to50_cuts_template.f pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_cuts.f
		sed -i '1s/0\~50/600\~inf/g' pt${bin_low}_inf/*cuts.f
		sed -i "422s/50/$bin_low/g" pt${bin_low}_inf/*cuts.f
		sed -i "422s/gt/le/g" pt${bin_low}_inf/*cuts.f
	else
		mkdir pt${bin_low}_${bin_high}
        cp ../w012jfxfx_inc/*custo* pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_customizecards.dat
        cp ../w012jfxfx_inc/*madspin* pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_madspin_card.dat
        cp ../w012jfxfx_inc/*run* pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_run_card.dat
        cp ../w012jfxfx_inc/*proc* pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_proc_card.dat
        sed -i "16s/wellnu012j_5f_NLO_FXFX/wellnu012j_wpt-${bin_low}to${bin_high}/g" pt${bin_low}_${bin_high}/*proc_card.dat
        cp ./wpt-0to50_cuts_template.f pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_cuts.f
		if [[ "$bin_low" -ne "${Pt_array[0]}" ]];then
            sed -i "1s/0\~50/$bin_low\~$bin_high/g" pt${bin_low}_${bin_high}/*cuts.f
            sed -i "422s/ptw.gt.50/(ptw .le. $bin_low) .or. (ptw .gt. $bin_high)/g" pt${bin_low}_$bin_high/*cuts.f
            
		fi			
	fi
done

