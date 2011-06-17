#!/bin/bash

#Sscript needed for the uploading  of MG5 post-processed files for CMS MC production
#Written by Alexis Kalogeropoulos on 3/3/2011
#Last revision 16/6/2011


mkdir Uploaded_${4}
mkdir Fewer_Events_${4}

if [ ${1} == help ] ;then
	echo You asked for help...
	echo 
	echo This script uploads files on mcdb -- Needs the upload2mcdb.pl script to be also present in the same dir  -- Put those 2 files in the mgPostv2 dir and you can use it like
	echo 
	echo '$1' = start of seed number 
	echo
	echo '$2' = end of seed number 
	echo 
	echo '$3' = name of the process : This script , will assume that you LHE files have been post-processed and  follow the suffix '7TeV_${3}_run${seed}_unweighted_events_qcut${4}_mgPostv2.lhe' - So, before uploading, this script will remove the seed information lines and will check if less than 100k events are present in the file -- If this is case and less than 100k events are detectd,, then the file wont be uploaded, but it will be moved to a dir named 'Fewer_Events_${mcdb}'
	echo
	echo '$4' = the qcut
	echo
	echo '$5' = the mcdb article number were you want to upload the file
	echo
	echo -- So, first upload a file using  '>' perl upload2mcbd.pl '7TeV_${3}_run${seed}_unweighted_events_qcut${4}_mgPostv2.lhe' -- This will autocreate a new article in mcdb and now you should visit http://mcdb.cern.ch/ and see the unique number of the article -- BE CAREFULL -- the upload2mcdb DOES NOT remove seed info and does not check for the 100k events if they are present or not -- So, even if you "uploaded" the file, ALWAYS upload  it again with the upload.sh script -- mcdb will keep only the most-updated version
	return
fi

for i in `seq ${1} ${2}`
do

name=${3}

file=7TeV_${name}_run${i}_unweighted_events_qcut${4}_mgPostv2.lhe   

if [ -f ${file} ];then

	grep "ERROR" ${file}

	sed -i '/ERROR/ d' ${file}
	sed  -i     '/! Random seed number/ d' ${file}
	grep -c "<event>" ${file} > count_events

status=`head -n 1 count_events`
st=100000
rm count_events

if [ "$status" -ne "$st" ] ; then

	echo ${file} has $status events !!! will not upload it to mcdb artid ${5}
	mv ${file} Fewer_Events_${5}/.

fi
	
if [[  ! -f  Uploaded_${5}/${file}  && ! -f Fewer_Events_${5}/${file}  ]]; then

perl upload2mcdb.pl ${file} -artid ${5}

	mv ${file} Uploaded_${5}/.

fi
fi

done


