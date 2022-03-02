#!/bin/bash

echo `pwd`
echo 'set up pt-binned cards'

#1000 will be replaced by infinite
#Pt_array=(0 50 100 250 400 600 1000)
Pt_array_low=(0 20 70 220 370 550)
Pt_array_high=(80 130 280 430 650 1000)

for((i=0;i<${#Pt_array_low[*]};i++))
do
	bin_low=${Pt_array_low[i]}
	bin_high=${Pt_array_high[i]}
	if [[ "$bin_low" -eq "${Pt_array_low[5]}" ]];then
     	mkdir pt${bin_low}_inf
        cp ../w012jfxfx_inc/*custo* pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_customizecards.dat
		cp ../w012jfxfx_inc/*madspin* pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_madspin_card.dat
		cp ../w012jfxfx_inc/*run* pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_run_card.dat
		cp ../w012jfxfx_inc/*proc* pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_proc_card.dat
		sed -i "16s/wellnu012j_5f_NLO_FXFX/wellnu012j_wpt-${bin_low}toinf/g" pt${bin_low}_inf/*proc_card.dat
		cp ./wpt-0to80_cuts_template.f pt${bin_low}_inf/wellnu012j_wpt-${bin_low}toinf_cuts.f
		sed -i '1s/0\~80/600\~inf/g' pt${bin_low}_inf/*cuts.f
		sed -i "421s/80/$bin_low/g" pt${bin_low}_inf/*cuts.f
		sed -i "421s/gt/le/g" pt${bin_low}_inf/*cuts.f
	else
		mkdir pt${bin_low}_${bin_high}
        cp ../w012jfxfx_inc/*custo* pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_customizecards.dat
        cp ../w012jfxfx_inc/*madspin* pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_madspin_card.dat
        cp ../w012jfxfx_inc/*run* pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_run_card.dat
        cp ../w012jfxfx_inc/*proc* pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_proc_card.dat
        sed -i "16s/wellnu012j_5f_NLO_FXFX/wellnu012j_wpt-${bin_low}to${bin_high}/g" pt${bin_low}_${bin_high}/*proc_card.dat
        cp ./wpt-0to80_cuts_template.f pt${bin_low}_${bin_high}/wellnu012j_wpt-${bin_low}to${bin_high}_cuts.f
		if [[ "$bin_low" -ne "${Pt_array[0]}" ]];then
            	  sed -i "1s/0\~80/$bin_low\~$bin_high/g" pt${bin_low}_${bin_high}/*cuts.f
            	  sed -i "421s/80/$bin_high/g" pt${bin_low}_${bin_high}/*cuts.f
            	  sed -i "421s/) then//g" pt${bin_low}_${bin_high}/*cuts.f
	    	  sed -i "421a&              .or. (p(1,i)+p(1,j))**2+(p(2,i)+p(2,j))**2 .le.${bin_low}d0**2 ) then" pt${bin_low}_${bin_high}/*cuts.f
	    	  sed -i '422s/^/     /g' pt${bin_low}_${bin_high}/*cuts.f
		fi			
	fi
done

