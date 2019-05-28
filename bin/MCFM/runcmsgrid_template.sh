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
cd $LHEWORKDIR

basename=BASENAME

LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`
#if lhapdf6 external is available then above points to lhapdf5 and needs to be overridden
LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
if [ -e $LHAPDF6TOOLFILE ]; then
  LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
fi
#make sure env variable for pdfsets points to the right place
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:`$LHAPDFCONFIG --libdir`
export LHAPDF_DATA_PATH=`$LHAPDFCONFIG --datadir`
export LHAPATH=$LHADPDF_DATA_PATH

#idea taken from https://github.com/cms-sw/genproductions/blob/733f0729283054fc381f1035616d6793db0ea33c/bin/MadGraph5_aMCatNLO/runcmsgrid_LO.sh#L68
#but the implementation is a little different, because the problem isn't requesting too many events, it's the job crashing when it hits nan

echo 0 > produced_lhe
run_counter=0
inputseed=$rnum

# if rnum allows, multiply by 10 to avoid multiple runs 
# with the same seed across the workflow
run_random_start=$(($rnum*10))
# otherwise, still multiply by 10 but chop the first digit
if [  $run_random_start -gt "89999990" ]; then
    run_random_start=$(echo "print str($run_random_start)[1:]" | python)
fi

while [ $(cat produced_lhe | paste -sd+ | bc) -lt ${nevt} ]; do
  # set the incremental iteration seed
  run_random_seed=$(echo $run_random_start + $run_counter | bc)
  # increase the iteration counter
  run_counter=$(echo $run_counter+1 | bc)

  # don't allow more than 90 iterations
  if [  $run_counter -gt "90" ]; then
      echo "asking for more than 90 iterations, this should never happen"
      break
  fi

  (
    echo "Running MCFM for the $run_counter time"
    mkdir running_$run_counter
    cd running_$run_counter
    ln -s ../*.DAT ../*.dat ../br.sm* ../*_grid ../*.bin .
    rm readInput.DAT
    cp ../readInput.DAT readInput.DAT
    inputnevt=$( echo "(${nevt} - $(cat ../produced_lhe | paste -sd+ | bc)) * 1.1 / $ncpu" | bc)
    inputnevt=${inputnevt%.*}
    sed -i "s/NEVENT/"$inputnevt"/" readInput.DAT
    sed -i "s/SEED/"$run_random_seed"/" readInput.DAT
    ../Bin/mcfm readInput.DAT |& tee log
    lhefiles=($(ls *.lhe | grep -v cmsgrid_))
    if [ ${#lhefiles[@]} -ne 1 ];then exit 1; else FILENAME=${lhefiles[0]}; fi
    grep "</event>" $FILENAME | wc -l >> ../produced_lhe
    mv $FILENAME $LHEWORKDIR/cmsgrid_${run_counter}.lhe
  ) &

  while [ $( jobs | grep '^\[' | wc -l ) -ge $ncpu ]; do
    jobs > /dev/null  #otherwise it does an infinite loop.  not sure why this happens.
    echo "$( jobs | grep '^\[' | wc -l ) jobs running"
    sleep 10s
  done

  submitmorejobs=false
  if ! cat produced_lhe | grep [1-9]; then
    submitmorejobs=true
  fi
  while ! $submitmorejobs; do
    jobs > /dev/null
    njobsrunning=$(jobs | grep '^\[' | wc -l)
    njobsfinished=$(cat produced_lhe | grep [1-9] | wc -l)
    eventsproduced=$(cat produced_lhe | paste -sd+ | bc)
    nprojectedevents=$(echo "$eventsproduced * ($njobsrunning + $njobsfinished) / $njobsfinished" | bc)
    if [ $nprojectedevents -lt ${nevt} ] || [ $eventsproduced -ge ${nevt} ]; then
      submitmorejobs=true
    else
      echo "When the current jobs are done, we will have about $nprojectedevents events"
      echo "No need to start more jobs"
      sleep 10s
    fi
  done
done

wait

cd $LHEWORKDIR
python adjlheevent.py ${nevt} $(cat produced_lhe | paste -sd+ | bc) ${rnum} cmsgrid_*.lhe

ls -l
echo

exit 0
