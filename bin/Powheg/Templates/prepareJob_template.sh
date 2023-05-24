#!/bin/bash 
fail_exit() { echo "$$@" 1>&2; exit 1; } 

echo "Start of job on " `date`

cd $rootfolder

source /cvmfs/cms.cern.ch/cmsset_default.sh

eval `scramv1 runtime -sh`

### Prepare environments for FastJet ### 

export FASTJET_BASE=`scram tool info fastjet | grep FASTJET_BASE | sed -e s%FASTJET_BASE=%%`
export PATH=$$FASTJET_BASE/bin/:$$PATH 

### Prepare environments for LHAPDF ### 
if [ "$$process" == "ttJ_MiNNLO" ]; then
  LHAPDF_BASE=/cvmfs/cms.cern.ch/slc7_amd64_gcc900/external/lhapdf/6.3.0
else
  LHAPDF6TOOLFILE=$$CMSSW_BASE/config/toolbox/$$SCRAM_ARCH/tools/available/lhapdf6.xml    
  if [ -e $$LHAPDF6TOOLFILE ]; then    
    export LHAPDF_BASE=`cat $$LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`    
  else    
    export LHAPDF_BASE=`scram tool info lhapdf | grep LHAPDF_BASE | sed -e s%LHAPDF_BASE=%%`    
  fi    
fi

echo "LHAPDF_BASE is set to:" $$LHAPDF_BASE 
export PATH=$$LHAPDF_BASE/bin/:$$PATH 
export LHAPDF_DATA_PATH=`$$LHAPDF_BASE/bin/lhapdf-config --datadir`
export PYTHONPATH=.:$$PYTHONPATH

cd -
echo "I am here:"
pwd

cp -p ${rootfolder}/${folderName}/powheg.input ./
cp -p ${rootfolder}/${folderName}/JHUGen.input ./
cp -p ${rootfolder}/${folderName}/*.dat  ./
cp -p ${rootfolder}/${folderName}/pwhg_main  ./ 
if [ -e ${rootfolder}/${folderName}/obj-gfortran/proclib ]; then
  mkdir ./obj-gfortran/
  cp -pr ${rootfolder}/${folderName}/obj-gfortran/proclib  ./obj-gfortran/
  cp -pr ${rootfolder}/${folderName}/obj-gfortran/*.so  ./obj-gfortran/ 
  export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:`pwd`/obj-gfortran/proclib/:$${LD_LIBRARY_PATH}
fi
if [ -d ${rootfolder}/${folderName} ]; then 
  cp -p ${rootfolder}/${folderName}/*.grid .
  cp -p ${rootfolder}/${folderName}/*.cdf .
  cp -p ${rootfolder}/${folderName}/*.py* .
fi 

