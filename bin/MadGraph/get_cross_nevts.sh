#!/bin/bash

#Script needed for obtaing the x-sec  of MG5 files for CMS MC production
#Written by Alexis Kalogeropoulos on 3/3/2011
#Last revision 16/6/2011

if [ ${1} == help ] ;then
	        echo You asked for help...
		echo
	       	echo this script will just take out the x-sec from LHE files -- You can use it like
		echo 
	        echo '$1' = start of seed number 
	        echo
               	echo '$2' = end of seed number 
        	echo 
		echo '$3' = name of the process : This script , will assume that your LHE files have the following suffixes '7TeV_${3}_run${seed}_unweighted_events.lhe' - The file'(s)' can be gziped but they they should NOT have been post-processed, as the post-processing removes the xsec information from the banner - At the end, you should inspect the following files
		echo total_number_of_events_'${3}' --- Gives the total number of events -- Is the average of all the input LHE files
		echo total_xsec_'${3}' -- Gives the average x-sec of all the input LHE files -- Nevertheless, along with this, always check as well the report_xsec_'${3}' to quickly inspect for something strange as this is the file listing the file-by-file xsecs
		echo
		return
fi


for i in `seq $1 $2`; do

file=7TeV_${3}_run${i}_unweighted_events.lhe
file_ban="$TTPATH"/${3}_run${i}_banner.txt


if [  -f ${file}.gz ]; then
	gzip -d ${file}.gz
	fi

if [  -f ${file} ]; then
		
sed "1i\# this ${file} has been processed for rnd ${i} at $DT" process_report > log_file_${3}_${i}
awk 'BEGIN{FS="#Integrated weight (pb) "}/Integrated/{print $1}' "${file}" > out

awk 'BEGIN{FS=":  "}/ /{print $2}' out > xsec_${i}
sed "1i\# this is the MG x-sec ${file} for rnd ${i} " xsec_${i} > xsec2_${i};mv xsec2_${i} xsec_${i}

awk 'BEGIN{FS="#  Number of Events        :      "}/Number of /{print $2}' "${file}"  > events_${i}
sed "1i\# this is the number of events ${file} for rnd ${i} " events_${i} > events2_${i};mv events2_${i} events_${i}

sed '/^$/d' events_${i}  > nevents_${i}

rm out;
fi

done

less xsec_* > report_xsec_${3}
rm xsec_*

less log_file_* > log_report_${3}
sed '/^$/d' log_report_${3} > ${3} ; mv ${3} log_report_${3} ;
rm log_file_*

less nevents_* > report_number_of_events_${3}
rm nevents_*
rm events_*

awk '/^(#|$)/{next;}{++c;s+=$1} END {printf ("# %d total jobs: ",c);c=(c>0?c:1);print "sum: " s ", naverage: " s/c ;print  s/c}'  report_xsec_${3}  > total_xsec_${3}
awk '/^(#|$)/{next;}{++c;s+=$1} END {printf ("# %d total jobs: ",c);c=(c>0?c:1);print "sum: " s ", naverage: " s/c ;print  s/c}'  report_number_of_events_${3}  > total_number_of_events_${3}


