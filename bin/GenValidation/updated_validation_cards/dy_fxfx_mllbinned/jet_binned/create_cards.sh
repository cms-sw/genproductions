#!/bin/bash

echo `pwd`
echo 'set up jet-binned cards'

jet_array=(0 1 2)

for((i=0;i<${#jet_array[*]};i++))
do
	bin_num=${jet_array[i]}
	mkdir dy${bin_num}jfxfx
	cp ../mll10to50/*custo* dy${bin_num}jfxfx/dyellell${bin_num}j_mll10to50_5f_NLO_FXFX_customizecards.dat
	cp ../mll10to50/*proc* dy${bin_num}jfxfx/dyellell${bin_num}j_mll10to50_5f_NLO_FXFX_proc_card.dat
	sed -i "9s/dyellell012j_mll10to50_5f_NLO_FXFX/dyellell${bin_num}j_mll10to50_5f_NLO_FXFX/g" dy${bin_num}jfxfx/*proc_card.dat
	cp ../mll10to50/*run* dy${bin_num}jfxfx/dyellell${bin_num}j_mll10to50_5f_NLO_FXFX_run_card.dat
	cp ../mll10to50/*cuts* dy${bin_num}jfxfx/dyellell${bin_num}j_mll10to50_5f_NLO_FXFX_cuts.f
	
	if [[ "$bin_num" -eq 0 ]];then
		sed -i '7d' dy${bin_num}jfxfx/*proc_card.dat
		sed -i '7d' dy${bin_num}jfxfx/*proc_card.dat
		sed -i '6s/@0//g' dy${bin_num}jfxfx/*proc_card.dat
	elif [[ "$bin_num" -eq 1 ]];then
		sed -i '6d' dy${bin_num}jfxfx/*proc_card.dat
		sed -i '7d' dy${bin_num}jfxfx/*proc_card.dat
		sed -i '6s/@1//g' dy${bin_num}jfxfx/*proc_card.dat
		sed -i '6s/add process/generate/g' dy${bin_num}jfxfx/*proc_card.dat
	else 
		sed -i '6d' dy${bin_num}jfxfx/*proc_card.dat
		sed -i '6d' dy${bin_num}jfxfx/*proc_card.dat
		sed -i '6s/@2//g' dy${bin_num}jfxfx/*proc_card.dat
		sed -i '6s/add process/generate/g' dy${bin_num}jfxfx/*proc_card.dat
     	fi
done

