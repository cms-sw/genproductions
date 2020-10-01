#!/bin/bash

echo `pwd`
echo 'set up jet-binned cards'

jet_array=(0 1 2 3 4)

for((i=0;i<${#jet_array[*]};i++))
do
	bin_num=${jet_array[i]}
	mkdir w${bin_num}jmlm
	cp ../w01234mlm_inc/*custo* w${bin_num}jmlm/W${bin_num}JetsToLNu_mlm_customizecards.dat 
	cp ../w01234mlm_inc/*proc* w${bin_num}jmlm/W${bin_num}JetsToLNu_mlm_proc_card.dat
	sed -i "16s/WJetsToLNu_mlm_inc/W${bin_num}JetsToLNu_mlm/g" w${bin_num}jmlm/*proc_card.dat
	cp ../w01234mlm_inc/*run* w${bin_num}jmlm/W${bin_num}JetsToLNu_mlm_run_card.dat
	
	if [[ "$bin_num" -eq 0 ]];then
		sed -i '7d' w${bin_num}jmlm/*proc_card.dat
		sed -i '7d' w${bin_num}jmlm/*proc_card.dat
		sed -i '7d' w${bin_num}jmlm/*proc_card.dat
		sed -i '7d' w${bin_num}jmlm/*proc_card.dat
		sed -i '8d' w${bin_num}jmlm/*proc_card.dat
		sed -i '8d' w${bin_num}jmlm/*proc_card.dat
		sed -i '8d' w${bin_num}jmlm/*proc_card.dat
		sed -i '8d' w${bin_num}jmlm/*proc_card.dat
		sed -i '7s/@5/@1/g' w${bin_num}jmlm/*proc_card.dat
	elif [[ "$bin_num" -eq 1 ]];then
		sed -i '6d' w${bin_num}jmlm/*proc_card.dat
		sed -i '7d' w${bin_num}jmlm/*proc_card.dat
		sed -i '7d' w${bin_num}jmlm/*proc_card.dat
		sed -i '7d' w${bin_num}jmlm/*proc_card.dat
		sed -i '7d' w${bin_num}jmlm/*proc_card.dat
		sed -i '8d' w${bin_num}jmlm/*proc_card.dat
		sed -i '8d' w${bin_num}jmlm/*proc_card.dat
		sed -i '8d' w${bin_num}jmlm/*proc_card.dat
		sed -i '6s/@1/@0/g' w${bin_num}jmlm/*proc_card.dat
		sed -i '7s/@6/@1/g' w${bin_num}jmlm/*proc_card.dat
		sed -i '6s/add process/generate/g' w${bin_num}jmlm/*proc_card.dat
	elif [[ "$bin_num" -eq 2 ]];then
		sed -i '6d' w${bin_num}jmlm/*proc_card.dat
                sed -i '6d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '8d' w${bin_num}jmlm/*proc_card.dat
                sed -i '8d' w${bin_num}jmlm/*proc_card.dat
                sed -i '6s/@2/@0/g' w${bin_num}jmlm/*proc_card.dat
                sed -i '7s/@7/@1/g' w${bin_num}jmlm/*proc_card.dat
                sed -i '6s/add process/generate/g' w${bin_num}jmlm/*proc_card.dat
	elif [[ "$bin_num" -eq 3 ]];then
		sed -i '6d' w${bin_num}jmlm/*proc_card.dat
                sed -i '6d' w${bin_num}jmlm/*proc_card.dat
                sed -i '6d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '8d' w${bin_num}jmlm/*proc_card.dat
                sed -i '6s/@3/@0/g' w${bin_num}jmlm/*proc_card.dat
                sed -i '7s/@8/@1/g' w${bin_num}jmlm/*proc_card.dat
                sed -i '6s/add process/generate/g' w${bin_num}jmlm/*proc_card.dat
	else 
		sed -i '6d' w${bin_num}jmlm/*proc_card.dat
                sed -i '6d' w${bin_num}jmlm/*proc_card.dat
                sed -i '6d' w${bin_num}jmlm/*proc_card.dat
                sed -i '6d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '7d' w${bin_num}jmlm/*proc_card.dat
                sed -i '6s/@4/@0/g' w${bin_num}jmlm/*proc_card.dat
                sed -i '7s/@9/@1/g' w${bin_num}jmlm/*proc_card.dat
                sed -i '6s/add process/generate/g' w${bin_num}jmlm/*proc_card.dat
     	fi
done

