#!/bin/bash


for i in `seq $1 $2`; do

name=${3}

	if [ -f alicegrid09.ba.infn.it/${name}/${name}_${i}_events.lhe.gz  ] ;then
	        mv alicegrid09.ba.infn.it/${name}/${name}_${i}_events.lhe.gz 7TeV_${name}_run${i}_unweighted_events.lhe.gz
	       #gzip -d 7TeV_${name}_run${i}_unweighted_events.lhe.gz
       fi	       
done


