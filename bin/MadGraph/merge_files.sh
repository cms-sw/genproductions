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

if [ ! -f merging_scripts.tar ]; then
	echo Cannot work without the merging_scripts.tar.....sorry
	return ;
fi
tar -xvf merging_scripts.tar
name=${3};
ref_file=${5};
ref_first_block=`awk '/<init>/{print NR }' "${ref_file}"`
ref_second_block=`awk '/<\/init>/{print NR }' "${ref_file}"`
ref_num=${ref_second_block}-${ref_first_block}
ref_num=$(( ref_num - 2 ));

#mkdir ${name}_${ref_num}_processes;
mkdir temp_dir;

	echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo  The reference file ${ref_file} has ${ref_num} processes... Will check the rest of the files before merging...
	echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	echo
for i in `seq $1 $2`; do

#file2=7TeV_${name}_run${i}_unweighted_events

file2=7TeV_${name}_run${i}_unweighted_events_qcut${4}_mgPostv2.lhe

	if [[  -f ${file2}   && -f ${5} ]]; then


first_block=`awk '/<init>/{print NR }' "${file2}"`
second_block=`awk '/<\/init>/{print NR }' "${file2}"`
let num=${second_block}-${first_block}
num=$(( num - 2 ));
	
if [ ${ref_num} -eq ${num} ]; then

	echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	echo ${file2}.lhe and ${ref_file} have the same number of processes...will move the ${file2} from the way...
	echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	echo
	#mv ${file2} ${name}_${ref_num}_processes/.
	mv ${file2} temp_dir/.

fi	
	
if [ ${ref_num} -ne ${num} ]; then

#	if [ ! -d ${name}_${num}_processes ] ;then
#	mkdir ${name}_${num}_processes
#	fi

	echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	echo ${file2} has ${num} and ${ref_file} has ${ref_num} DO NOT have the same number of processes...
	echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	echo
	#echo will move the ${file2} in folder ${name}_${num}_processes
	#mv ${file2} ${name}_${num}_processes/.
fi	


fi
done
	echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	echo Now, will extract the banner info  and do the merging...WARNING!!! If banner already exists in current directory will NOT overwrite it -
	echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	echo 
	if [ ! -f banner.txt ] ;then
		perl extract_banner-pl	 ${ref_file} banner.txt
	fi
	#mv ${ref_file} ${name}_${ref_num}_processes/.
	mv ${ref_file} temp_dir/.
ls temp_dir/* -ltrh
perl merge-pl temp_dir/*  7TeV_${name}_run${1}to${2}_${ref_num}processes_unweighted_events.lhe.gz  banner.txt
if [ ! -d Merged_Files ] ;then
mkdir Merged_Files
fi
gzip -d 7TeV_${name}_run${1}to${2}_${ref_num}processes_unweighted_events.lhe.gz
mv 7TeV_${name}_run${1}to${2}_${ref_num}processes_unweighted_events.lhe.gz Merged_Files/.

if [ ! -d Merged_Files/run${1}to${2}_${ref_num}processes ]; then
mkdir Merged_Files/run${1}to${2}_${ref_num}processes
fi

#mv ${name}_${ref_num}_processes/*  Merged_Files/run${1}to${2}_${ref_num}processes/.
mv temp_dir/*  Merged_Files/run${1}to${2}_${ref_num}processes/.
rm -fr temp_dir
echo DONE!!!! CU around!
