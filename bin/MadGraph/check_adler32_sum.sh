#!/bin/bash
# Created by Nasuf Sonmez on 11/08/2011.
# 2011 Ege University.

if [ $# -ne 1 ] ;then
	echo "You need to give an article number"
	echo "./check_adler32_sum.sh [ article ]"
	exit
fi
if [ "$1" == "--help" -o "$1" == "help" ] ;then
	echo "This script compares the Adler32 Sums for the LHE files in the current directory with the files in EOS. "
	echo "Keep in mind that it looks for a specific pattern in the name of the files: 7TeV_**_runprocesses_unweighted_events.lhe"
	exit
fi
# _______________________________________
article=$1
# _______________________________________

EOSLHEFOLDER=/eos/cms/store/lhe/${article}

source /afs/cern.ch/cms/caf/eos.sh
# check how many files reside in the article folder
eosresponse=$(/afs/cern.ch/user/a/apeters/public/eoscms ls ${EOSLHEFOLDER} 2>&1)
eosresponse=`echo $eosresponse | sed 's/ /\n/g'`

response_unable=`echo $eosresponse | grep -c "Unable to stat"`
n_eos_files=`echo $eosresponse | sed 's/ /\n/g' | grep -c 7TeV`
n_loc_files=`ls 7TeV_*processes_unweighted_events.lhe | grep -c 7TeV`

# create a list for files in EOS
echo $eosresponse | sed "s/ /\n/g" > files_in_eos 

# EXIT if there is no file in current dir or EOS folder.
if [ "$response_unable" -ne 0 ] ;then
	echo "!!!  The article number $article doesnt exist."
	exit
elif [ "$n_eos_files" -eq 0  ] ;then
	echo "!!!  There is no file in article $article"
	exit
elif [ "$n_loc_files" -eq 0 ] ;then
	echo "!!!  There is no LHE file in the current folder"
	exit
fi

echo "                                                  "
echo "    There are '$n_loc_files' files in current dir."
echo "    There are '$n_eos_files' files in EOS folder.	"
echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 	"

# _________________________________________________________________
# do the Adler32 comparison

for i in `seq 1 $n_eos_files`; do		# loop over eos files

	# pick one file from the list
	eosfile=`cat files_in_eos | head -${i} | tail -1`
	control_flag=0
	for j in `seq 1 $n_local_files`; do     # loop over local files

		# pick one file from the current dir.
		localfile=`ls  7TeV_*processes_unweighted_events.lhe | head -${j} | tail -1`
		
		# if the LOCAL file name and EOS name are the same
		if [ "$eosfile" == "$localfile" ] ;then
			control_flag=1
			# GET the adler32 strings for EOS and LOCAL
			eos_adler_sum=$(/afs/cern.ch/user/a/apeters/public/eoscms fileinfo ${EOSLHEFOLDER}/${eosfile} --checksum)
			eos_adler32=`echo  $eos_adler_sum | awk -F "xs: " '{print $2}'`
			loc_adler32=`/afs/cern.ch/cms/caf/bin/cms_adler32 $localfile | awk -F " " '{print $1}'`

			echo "$i - $eosfile                  "
			# COMPARE Adler32 
			if [ "$eos_adler32" == "$loc_adler32"  ] ;then
				echo "Local file :  $loc_adler32"  
				echo "EOS file   :  $eos_adler32"
				echo "___________________________"
			else
				echo "Adler32 sum are not the same for the local and eos file"
				echo "Local : $loc_adler32"
				echo "EOS   : $eos_adler32"
				echo "___________________________"
			fi
		fi 
	done

	if [ $control_flag -eq 0 ] ;then
		echo "$eosfile doesnt exist in EOS directory, skips cheking for this file."
		echo "___________________________"	
	fi 
done

rm -rf files_in_eos
