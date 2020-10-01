#!/bin/bash

echo `pwd`
echo 'set up jet-binned cards'

jet_array=(0 1 2)

for((i=0;i<${#jet_array[*]};i++))
do
	bin_num=${jet_array[i]}
	mkdir w${bin_num}jfxfx
	cp ../w012jfxfx_inc/*custo* w${bin_num}jfxfx/wellnu${bin_num}j_5f_NLO_FXFX_customizecards.dat 
	cp ../w012jfxfx_inc/*proc* w${bin_num}jfxfx/wellnu${bin_num}j_5f_NLO_FXFX_proc_card.dat
	sed -i "16s/wellnu012j_5f_NLO_FXFX/wellnu${bin_num}j_5f_NLO_FXFX/g" w${bin_num}jfxfx/*proc_card.dat
	cp ../w012jfxfx_inc/*run* w${bin_num}jfxfx/wellnu${bin_num}j_5f_NLO_FXFX_run_card.dat
	cp ../w012jfxfx_inc/*madspin* w${bin_num}jfxfx/wellnu${bin_num}j_5f_NLO_FXFX_madspin_card.dat
	if [[ "$bin_num" -eq 0 ]];then
		sed -i '9d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '9d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '11d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '11d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '10s/@3/@1/g' w${bin_num}jfxfx/*proc_card.dat
	elif [[ "$bin_num" -eq 1 ]];then
		sed -i '8d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '9d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '10d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '11d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '8s/@1/@0/g' w${bin_num}jfxfx/*proc_card.dat
		sed -i '10s/@4/@1/g' w${bin_num}jfxfx/*proc_card.dat
		sed -i '8s/add process/generate/g' w${bin_num}jfxfx/*proc_card.dat
	else 
		sed -i '8d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '8d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '10d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '10d' w${bin_num}jfxfx/*proc_card.dat
		sed -i '8s/@2/@0/g' w${bin_num}jfxfx/*proc_card.dat
		sed -i '10s/@5/@1/g' w${bin_num}jfxfx/*proc_card.dat
		sed -i '8s/add process/generate/g' w${bin_num}jfxfx/*proc_card.dat
     	fi
done

