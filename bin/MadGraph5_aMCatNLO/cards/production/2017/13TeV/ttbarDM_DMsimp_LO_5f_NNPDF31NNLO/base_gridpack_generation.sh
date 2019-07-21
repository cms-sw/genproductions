#!/bin/bash
echo -e "\nJob started at "`date`" on "`hostname --fqdn`"\n"
# set up cms shortcuts 
source /cvmfs/cms.cern.ch/cmsset_default.sh
cd SUBPATH
./gridpack_generation.sh SUBJOB addons/cards/SUBJOB 
#done 
echo -e "\nJob stoped at "`date`" on "`hostname --fqdn`"\n"
exit 0

