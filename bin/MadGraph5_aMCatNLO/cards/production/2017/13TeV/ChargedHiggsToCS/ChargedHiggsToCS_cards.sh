#!/bin/bash

masses=(80 90 100 110 120 130 140 150 155 160)
#masses=(80) 
charges=("plus" "minus")

postfix=(_run_card.dat _param_card.dat _proc_card.dat _extramodels.dat)

echo ${masses[*]}

# get length of an array
tLen=${#postfix[@]}
for charge in ${charges[*]};do
    sample=H${charge}ToCS
    for mass in ${masses[*]}; do
        echo generating cards $charge, for M = $mass GeV
	madspin=H${charge}ToCS_M${mass}_madspin_card.dat
	echo "set ms_dir ./madspingrid">$madspin
	echo "set Nevents_for_max_weigth 500">>$madspin 
 	echo "set max_weight_ps_point 400">>$madspin
	echo "set max_running_process 1">>$madspin
	echo $madspin
	if [[ "$charge" == "plus" ]]; then
	    echo "decay t~ > w- b~, w- > l- vl~">>$madspin
	    echo "decay t > h+ b, h+ > c s~">>$madspin
        else
	    echo "decay t > w+ b, w+ > l+ vl">>$madspin
	    echo "decay t~ > h- b~, h- > c~ s">>$madspin
	fi
	echo "launch" >> $madspin


#	mv $madspin ${sample}_M${mass}/	

        for (( i=0; i<${tLen}; i++ )) do
            if [ ! -d ${sample}_M${mass} ]; then
                mkdir ${sample}_M${mass}
            fi
            sed "s/<MASS>/${mass}/g" ${sample}/${sample}${postfix[$i]} > ${sample}_M$mass/${sample}_M$mass${postfix[$i]}
	    
        done
	mv $madspin ${sample}_M${mass}/
	done  
	done
