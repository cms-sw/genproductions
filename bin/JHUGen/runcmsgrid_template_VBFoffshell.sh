#!/bin/bash

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`
use_gridpack_env=true
if [ -n "$4" ]
  then
  use_gridpack_env=$4
fi

if [ "$use_gridpack_env" = true ]
  then
    if [ -n "$5" ]
      then
        scram_arch_version=${5}
      else
        scram_arch_version=SCRAM_ARCH_VERSION_REPLACE
    fi
    echo "%MSG-MG5 SCRAM_ARCH version = $scram_arch_version"

    if [ -n "$6" ]
      then
        cmssw_version=${6}
      else
        cmssw_version=CMSSW_VERSION_REPLACE
    fi
    echo "%MSG-MG5 CMSSW version = $cmssw_version"
    export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
    source $VO_CMS_SW_DIR/cmsset_default.sh
    export SCRAM_ARCH=${scram_arch_version}
    scramv1 project CMSSW ${cmssw_version}
    cd ${cmssw_version}/src
    eval `scramv1 runtime -sh`
fi

set -euo pipefail

cd $LHEWORKDIR

eval $(JHUGenMELA/MELA/setup.sh env)
cd JHUGenerator/

#seq 1 5 | parallel -j${ncpu} --eta "GENCOMMAND VBFoffsh_run={} ReadCSmax"
#no parallel command on condor
#https://www.gnu.org/software/parallel/parallel_alternatives.html#DIFFERENCES-BETWEEN-xargs-AND-GNU-Parallel
seq 1 164 | xargs -d "\n" -P${ncpu} -I {} bash -c "GENCOMMAND VBFoffsh_run={} ReadCSmax |& tee log_{}"




#randomize order of catted lhe files
awk -v loop=164 -v range=164 'BEGIN{                                                                             
                      
  srand()                                                                                                        
                    
  do {                                                                                                           
                    
    numb = 1 + int(rand() * range)                                                                               
                    
    if (!(numb in prev)) {                                                                                       
                    
      if(numb < 10 ){
         print "00"numb 
      }                                                                                                          
         
      if(numb < 100 && numb > 9 ){
         print "0"numb 
      }
      if(numb > 99){
         print numb
      }                                                                                                          
         
       prev[numb] = 1                                                                                            
                    
       count++                                                                                                   
                    
    }                                                                                                            
                    
  } while (count<loop)                                                                                           
                    
}' > order.txt


if [  -e Out.lhe  ]
 then 
    rm Out.lhe

 fi


count=0
for line in $(cat order.txt)
do
    if [ $count -eq 0 ]
    then 
	cat Out_$line.lhe      | grep -Ev "</LesHouchesEvents" >> Out.lhe
    elif [ $count != 163 ] 
    then 
	cat Out_$line.lhe | grep -Ev "</?LesHouchesEvents" | sed -e '/<init>/,+3d'>> Out.lhe
    else    
	cat Out_$line.lhe | grep -Ev "</LesHouchesEvents" | sed -e '/<init>/,+3d'>> Out.lhe
    fi
    count=$((count+1))
done
rm order.txt

mv Out.lhe $LHEWORKDIR/cmsgrid_final.lhe
cd $LHEWORKDIR
