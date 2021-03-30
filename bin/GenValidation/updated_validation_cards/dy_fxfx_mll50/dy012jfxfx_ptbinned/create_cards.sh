#!/bin/bash

echo `pwd`
echo 'set up pt-binned cards'

#1000 will be replaced by infinite
#Pt_array=(0 50 100 250 400 650 1000)
Pt_array_low=(0 20 70 220 370 600)
Pt_array_high=(80 130 280 430 700 1000)

for((i=0;i<${#Pt_array_low[*]};i++))
do
	bin_low=${Pt_array_low[i]}
	bin_high=${Pt_array_high[i]}
	if [[ "$bin_low" -eq "${Pt_array_low[5]}" ]];then
     		mkdir pt${bin_low}_inf
		cp ../dy012jfxfx_inc/*custom* pt${bin_low}_inf/DYJetsToLL_012j_Zpt-${bin_low}Toinf_5f_NLO_FXFX_customizecards.dat
		cp ../dy012jfxfx_inc/*proc* pt${bin_low}_inf/DYJetsToLL_012j_Zpt-${bin_low}Toinf_5f_NLO_FXFX_proc_card.dat
		sed -i "9s/dyellell012j_5f_NLO_FXFX/DYJetsToLL_012j_Zpt-${bin_low}Toinf_5f_NLO_FXFX/g" pt${bin_low}_inf/*proc_card.dat
		cp ../dy012jfxfx_inc/*run* pt${bin_low}_inf/DYJetsToLL_012j_Zpt-${bin_low}Toinf_5f_NLO_FXFX_run_card.dat
		cp ./Zpt-0To80_cuts_template.f pt${bin_low}_inf/DYJetsToLL_012j_Zpt-${bin_low}Toinf_5f_NLO_FXFX_cuts.f
		sed -i '1s/0\~80/650\~inf/g' pt${bin_low}_inf/*cuts.f
		sed -i "420s/80/$bin_low/g" pt${bin_low}_inf/*cuts.f
		sed -i "420s/gt/le/g" pt${bin_low}_inf/*cuts.f
	else
		mkdir pt${bin_low}_${bin_high}
		cp ../dy012jfxfx_inc/*custom* pt${bin_low}_${bin_high}/DYJetsToLL_012j_Zpt-${bin_low}To${bin_high}_5f_NLO_FXFX_customizecards.dat
		cp ../dy012jfxfx_inc/*proc* pt${bin_low}_${bin_high}/DYJetsToLL_012j_Zpt-${bin_low}To${bin_high}_5f_NLO_FXFX_proc_card.dat
		sed -i "9s/dyellell012j_5f_NLO_FXFX/DYJetsToLL_012j_Zpt-${bin_low}To${bin_high}_5f_NLO_FXFX/g" pt${bin_low}_${bin_high}/*proc_card.dat
		cp ../dy012jfxfx_inc/*run* pt${bin_low}_${bin_high}/DYJetsToLL_012j_Zpt-${bin_low}To${bin_high}_5f_NLO_FXFX_run_card.dat
		cp ./Zpt-0To80_cuts_template.f pt${bin_low}_${bin_high}/DYJetsToLL_012j_Zpt-${bin_low}To${bin_high}_5f_NLO_FXFX_cuts.f
		if [[ "$bin_low" -ne "${Pt_array[0]}" ]];then
			sed -i "1s/0\~80/$bin_low\~$bin_high/g" pt${bin_low}_${bin_high}/*cuts.f
			sed -i "420s/80/$bin_high/g" pt${bin_low}_${bin_high}/*cuts.f
			sed -i "420s/) then//g" pt${bin_low}_${bin_high}/*cuts.f
			sed -i "420a&              .or. (p(1,i)+p(1,j))**2+(p(2,i)+p(2,j))**2 .le.${bin_low}d0**2 ) then" pt${bin_low}_${bin_high}/*cuts.f
			sed -i '421s/^/     /g' pt${bin_low}_${bin_high}/*cuts.f
		fi			
	fi
done

