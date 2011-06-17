#!/bin/bash

###$1 start seed number , $2 end seed number , $3 model name, $4 qcut value $5 true or false for replace.pl 

for i in `seq $1 $2`; do

file="$3"_unweighted_events.lhe_${i}_*.gz
file=7TeV_"$3"_run${i}_unweighted_events.lhe

file2=7TeV_"$3"_run"$i"_unweighted_events_test.lhe


if [ -f ${file}.gz ] ; then
	gzip -d ${file}.gz
fi
if [ -f ${file} ] ; then


	mv ${file} ${file2}

	awk 'BEGIN{FS=" = gseed"}/gseed/{print $1}' ${file2} > banner

	while read seed
	do

  if  [ "$seed" -ne "$i" ] ; then

file3=7TeV_"$3"_run"$seed"_unweighted_events.lhe
	  echo  The seed is $seed for $file which will be renamed to ${file3} while $i


	mv  ${file2}  ${file3}

fi



    done < banner
	rm banner
fi

done

