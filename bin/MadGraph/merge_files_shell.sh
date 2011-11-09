#!/bin/bash

#Wrapper/script needed for the post-processing of MG5 files for CMS MC production
#Written by Alexis Kalogeropoulos on 29/7/2011
#Last revision 1/8/2011

if [ ${1} == help ] ;then
	echo You asked for help... PLEASE READ ALSO THE README_MERGE_FILES FIRST!!!!!!!
	echo 
	echo In short, the  script does the following... 1- Seperate the LHE files according to the number of the processes included in the '<init>' block , as the merging requires that all of the files to be merged carry the same number of processes - For this a reference file is being used, and then the script scans all of the files and moves the files in directories according to processes- Then, the actual merging takes place and merges all of the files MATCHING the number of processes of the reference file - While merging, the script makes a few sanity checks, like the xsecs should match in within a  given acceptance difference, the number of processes in the init blocks should be always the same etc etc.... So,you should just use it like
	echo 
	echo '$1' = start of seed number 
	echo
	echo '$2' = end of seed number 
	echo 
	echo '$3' = name of the process : This script , will assume that you LHE files have the following suffixes '7TeV_${3}_run${seed}_unweighted_events.lhe'
	echo 
	echo '$4' = the qcut you want to pass in 
	echo
	echo '$5' = the reference file...Please, make sure that the start_seed is greater than the run_number of the reference file
	echo
	echo Do not hesitate to contact CMS MC team in case of questions/comments/bugs found
	echo
	return
fi

name=${3};
step=${5};

for i in `seq $1  $2`; do

file2=7TeV_${name}_run${i}_unweighted_events_qcut${4}_mgPostv2.lhe

	if [  -f ${file2}  ]; then

start_seed=$(( i + 1 ))		
end_seed=$(( i + ${step} -1 ))		
.  merge_files.sh ${start_seed} ${end_seed} ${name} ${4} ${file2}


fi
done
