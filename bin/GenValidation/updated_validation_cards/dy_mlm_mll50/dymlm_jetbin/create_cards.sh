#!/bin/bash

echo `pwd`
echo 'set up jet-binned cards'

jet_array=(0 1 2 3 4)

for((i=0;i<${#jet_array[*]};i++))
do
	bin_num=${jet_array[i]}
	mkdir dy${bin_num}jmlm
	cp ../dy01234mlm_inc/*custo* dy${bin_num}jmlm/dyellell${bin_num}j_5f_LO_MLM_customizecards.dat
	cp ../dy01234mlm_inc/*proc* dy${bin_num}jmlm/dyellell${bin_num}j_5f_LO_MLM_proc_card.dat
	sed -i "11s/dyellell01234j_5f_LO_MLM/dyellell${bin_num}j_5f_LO_MLM/g" dy${bin_num}jmlm/*proc_card.dat
	cp ../dy01234mlm_inc/*run* dy${bin_num}jmlm/dyellell${bin_num}j_5f_LO_MLM_run_card.dat
	
	if [[ "$bin_num" -eq 0 ]];then
		sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '6s/@0//g' dy${bin_num}jmlm/*proc_card.dat
	elif [[ "$bin_num" -eq 1 ]];then
		sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '6s/@1//g' dy${bin_num}jmlm/*proc_card.dat
		sed -i '6s/add process/generate/g' dy${bin_num}jmlm/*proc_card.dat
	elif [[ "$bin_num" -eq 2 ]];then
                sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
                sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
                sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
                sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
                sed -i '6s/@2//g' dy${bin_num}jmlm/*proc_card.dat
                sed -i '6s/add process/generate/g' dy${bin_num}jmlm/*proc_card.dat
	elif [[ "$bin_num" -eq 3 ]];then
                sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
                sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
                sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
                sed -i '7d' dy${bin_num}jmlm/*proc_card.dat
                sed -i '6s/@3//g' dy${bin_num}jmlm/*proc_card.dat
                sed -i '6s/add process/generate/g' dy${bin_num}jmlm/*proc_card.dat
	else 
		sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '6d' dy${bin_num}jmlm/*proc_card.dat
		sed -i '6s/@4//g' dy${bin_num}jmlm/*proc_card.dat
		sed -i '6s/add process/generate/g' dy${bin_num}jmlm/*proc_card.dat
     	fi
done

