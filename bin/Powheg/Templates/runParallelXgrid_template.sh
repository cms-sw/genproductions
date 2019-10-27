#cd ${rootfolder}/${folderName}
echo ${iJob} | ./pwhg_main &> run_${jobID}.log
cp -p *.top ${rootfolder}/${folderName}/.
cp -p *.dat ${rootfolder}/${folderName}/.
cp -p *.log ${rootfolder}/${folderName}/. 

