#!/bin/bash

#Script needed for obtaing the x-sec  of MG5 files for CMS MC production
#Written by Alexis Kalogeropoulos on 3/3/2011
#Last revision 16/6/2011

if [ ${1} == help ] ;then
	        echo You asked for help...
		echo
	       	echo this script will check the relative x-secs and number of written events corresponding to each given processprocess by process  -- Use should use this script AFTER post-rpocessing , as will fail if the event history is not correct for example - You can use it like
		echo 
	        echo '$1' = start of seed number 
	        echo
               	echo '$2' = end of seed number 
        	echo 
		echo '$3' = name of the process : This script , will assume that your LHE files have the following suffixes '7TeV_${3}_run${seed}_unweighted_events_qcut'${5}'_mgPostv2.lhe' - The file'(s)' can be gziped as well
		echo
		echo '$4' =  the reference file to be compared with all the rest
		echo
		echo '$5' =  the qcut value
		return
fi


file_ref=${4}
if [ ! -f ${file_ref} ]; then
	echo File ${file_ref} does not exist... Please check
	return
fi

if [  -f ${file_ref}.gz ]; then
	gzip -d ${file_ref}.gz
	fi
	echo ... checking ${file_ref}
. check_xsec.sh ${file_ref} > ${file_ref}_log
        echo done...

for i in `seq $1 $2`; do

file=7TeV_${3}_run${i}_unweighted_events_qcut${5}_mgPostv2.lhe

if [  -f ${file}.gz ]; then
	gzip -d ${file}.gz
	fi


if [ -f ${file} ]; then
        echo ....checking ${file}
. check_xsec.sh ${file} > ${file}_log
        echo done...

. compare.sh  ${file_ref}_log  ${file}_log > outlog

fi

statuss=`grep -c "WARNING" outlog`

if [ $statuss -gt 0 ] ;then
	echo There might be a problem, as for ${file} the difference is above 10% - The report is 
	cat outlog
fi

done



