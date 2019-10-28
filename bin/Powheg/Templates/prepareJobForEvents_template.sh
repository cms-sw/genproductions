cp -p ${rootfolder}/${folderName}/*.dat  ./

if [ -e ${rootfolder}/${folderName}/obj-gfortran/proclib ]; then
  mkdir ./obj-gfortran/
  cp -pr ${rootfolder}/${folderName}/obj-gfortran/proclib  ./obj-gfortran/
  cp -pr ${rootfolder}/${folderName}/obj-gfortran/*.so  ./obj-gfortran/
fi   

cd -

pwd
ls
echo $iJob | ${rootfolder}/pwhg_main &> log_${tag}.log
cp -p log_${tag}.log ${rootfolder}/${folderName}/.

