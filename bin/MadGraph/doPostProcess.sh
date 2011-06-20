#!/bin/bash

#Wrapper/script needed for the post-processing of MG5 files for CMS MC production
#Written by Alexis Kalogeropoulos on 3/3/2011
#Last revision 16/6/2011

if [ ${1} == help ] ;then
	echo You asked for help...
	echo 
	echo This script does the post processing... You should just use it like
	echo 
	echo '$1' = start of seed number 
	echo
	echo '$2' = end of seed number 
	echo 
	echo '$3' = name of the process : This script , will assume that you LHE files have the following suffixes '7TeV_${3}_run${seed}_unweighted_events.lhe' - The file'(s)' can be gziped -- Alternatively, if you have your files stored in your local SE, plz uncomment and modify accordingly around line 50 - Another script, rename.sh can be used to rename files of the format '$name'_events.lhe_*.gz which is most usual output if you have generated files using the official CMS CRAB-MG5 tools
	echo 
	echo '$4' = the qcut you want to pass in 
	echo
	echo '$5' = true or false, if you want to replace el and vel with el/mu/tau and vel/vmu/vtau -usually processes of V+jets where we generate only 1 flavor- Please inspect the proc_card to decide if you need this or not...
	echo
	echo '$6' = the max jet flavor parameter - If you do want to apply cuts on the b-quarks, like for example V+HF processes like Zbb+jets, you should use 4 otherwise use 5-example, tt~+jets
	echo
	echo '$7' = should be wjets, zjets, qcd, ttbar -- This is a really important switch, as it initializes the proper arguments for the actual post-processing python script -ie adding masses, restoring w/z in history of the event etc etc-- Be very carefull how to use this and always consult the proc_card you used.
	echo
	echo '$8' = if is true, then DECAY will be run for decaying the tops -- Take care, as if you do that, you should have DECAY dir in 'pwd'/DECAY/DECAY and have already put the correct DECAY parameters in there -- Use it with care
	echo
	echo '$9' = the mcdb article -- Once a file is postprocessed will be copied to a dir named mgPostv2 and then you should upload them to mcdb -- So, if you use the upload.sh script, once a file is uploaded, it will be moved to a folder name after mgPostv2/Uploaded_'$9' -- So, this '$9' checks if a file is there, which normally means that is already uploaded to mcdb --If this is case the postprocessing will be skipped for this file - However, use '$9'=-1 if you want to bypass this and have your file processed anyway -- 
	echo
	echo Do not hesitate to contact CMS MC team in case of questions/comments/bugs found
	echo
	return
fi

mkdir mgPostv2
mkdir ProcessedFiles_${9}
name=${3}

for i in `seq $1 $2`; do

file2=7TeV_${name}_run${i}_unweighted_events

	if [  -f mgPostv2/Uploaded_${9}/${file2}_qcut${4}_mgPostv2.lhe  ]; then
echo File ${file2}_qcut${4}_mgPostv2.lhe already in Uploaded_${9} folder in mcdb artid ${9}...
	fi


file2=7TeV_${name}_run${i}_unweighted_events

	if [[ ! -f mgPostv2/Uploaded_${9}/${file2}_qcut${4}_mgPostv2.lhe  && ! -f mgPostv2/${file2}_qcut${4}_mgPostv2.lhe  ]]; then

############### UNCOMMENT THIS IF YOU HAVE FILES STORED IN LOCAL SE
#if [ -f /pnfs/iihe/cms/store/user/alkaloge/7TeV_Summer11/${name}/7TeV_${name}_run${i}_unweighted_events.lhe.gz ]; then
#echo Copying file 7TeV_${name}_run${i}_unweighted_events....
#dccp /pnfs/iihe/cms/store/user/alkaloge/7TeV_Summer11/${name}/7TeV_${name}_run${i}_unweighted_events.lhe.gz .


if [ -f 7TeV_${name}_run${i}_unweighted_events.lhe.gz ] ; then

gzip -d 7TeV_${name}_run${i}_unweighted_events.lhe.gz

fi

file=${3}_${i}_results.tar.gz
file2=7TeV_${name}_run${i}_unweighted_events

	if [ ! -f mgPostv2/${file2}_qcut${4}_mgPostv2.lhe ]; then

	#if [[ ! -f mgPostv2/Uploaded_${9}/${file2}_qcut${4}_mgPostv2.lhe  && ! -f mgPostv2/${file2}_qcut${4}_mgPostv2.lhe ]]; then
		         
	if  [ -f  ${file2}.lhe ] ; then

if [ "$8" == true ] ; then
	if [ -d DECAY ]; then
		gzip -zxvf DECAY.tar.gz
	fi

	echo will do the DECAY...Please make sure that you have used the desired parameters/switches in the MGxx/DECAY dir - Also, this script will do decay top with a mass of b_quark=4.7GeV
	sed 's/  5 0.000000 # b : 0/  5  4.700000 #b/g' ${file2}.lhe > test.lhe; mv test.lhe  ${file2}.lhe;

	mv ${file2}.lhe DECAY/events.lhe
	cd DECAY/DECAY
	for (( i = 1; i <=2; i++)) ; do

		echo "Decaying events..."
		mv ../events.lhe ../events_in.lhe
		./decay < decay_$i\.in
	done
	#rm ../events_in.lhe
	mv ../events.lhe ../../${file2}_in.lhe
	cd ../../									
fi



if [ ${5} == true ] ; then
	echo will also do the replacing...
	

echo ./replace.pl ${file2}_in.lhe ${file2}.lhe < replace_card1.dat
if [ -f ${file2}.lhe ] ; then
echo moving ${file2}.lhe to ${file2}_in.lhe for the replacing...
mv ${file2}.lhe ${file2}_in.lhe
fi
perl replace.pl ${file2}_in.lhe ${file2}.lhe < replace_card1.dat
fi

mv ${file2}.lhe ${file2}_in.lhe


if [[ ${7} == wjets || ${7} == zjets ]] ; then
	echo V+jets -- WILL fix w/z particle in events history and fix lepton masses mgPostProcv2.py -o $file2.lhe -m -w -j ${6} -q ${4} -e 5 -s ${file2}_in.lhe
python ./mgPostProcv2.py -o ${file2}_qcut${4}_mgPostv2.lhe -m -w -j ${6} -q ${4} -e 5 -s ${file2}_in.lhe
fi

if [ ${7} == qcd ] ; then
	echo QCD process -- mgPostProcv2.py -o $file2.lhe -q ${4} -j ${6} -e 5 -s ${file2}_in.lhe
python ./mgPostProcv2.py -o ${file2}_qcut${4}_mgPostv2.lhe -q ${4} -j ${6} -e 5 -s ${file2}_in.lhe
fi


if [ ${7} == ttbar ] ; then
	echo TTBAR PROCESS  -- WILL add W resonance in top decays and fix masses -- mgPostProcv2.py -o $file2.lhe -q ${4} -w -m -t -j ${6} -q ${4} -e 5 -s ${file2}_in.lhe
python ./mgPostProcv2.py -o ${file2}_qcut${4}_mgPostv2.lhe  -m -w -t -j ${6} -q ${4} -e 5 -s ${file2}_in.lhe
fi

mv ${file2}_qcut${4}_mgPostv2.lhe mgPostv2/
mv ${file2}_in.lhe ProcessedFiles_${9}/${file2}.lhe

#rm ${file2}_in.lhe
fi
fi
fi
############### UNCOMMENT THIS IF YOU HAVE FILES STORED IN LOCAL SE
#fi

done

