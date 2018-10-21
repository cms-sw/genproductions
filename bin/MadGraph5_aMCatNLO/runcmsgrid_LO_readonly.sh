#!/bin/bash

#set -x

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`

use_gridpack_env=true
if [ -n "$4" ]; then
  use_gridpack_env=$4
fi

if [ "$use_gridpack_env" = true ]
  then
    if [ -n "$5" ]
      then
        scram_arch_version=${5}
      else
        scram_arch_version=slc6_amd64_gcc481
    fi
    echo "%MSG-MG5 SCRAM_ARCH version = $scram_arch_version"

    if [ -n "$6" ]
      then
        cmssw_version=${6}
      else
        cmssw_version=CMSSW_7_1_30
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

path_to_gridpack=$LHEWORKDIR
if [ -n "$7" ]; then
  path_to_gridpack=$7
fi
echo $path_to_gridpack

cp $path_to_gridpack/process/madevent/Cards/me5_configuration.txt .

#make sure lhapdf points to local cmssw installation area
LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`
echo "lhapdf = $LHAPDFCONFIG" >> me5_configuration.txt
# echo "cluster_local_path = `${LHAPDFCONFIG} --datadir`" >> me5_configuration.txt

if [ "$ncpu" -gt "1" ]; then
  echo "run_mode = 2" >> me5_configuration.txt
  echo "nb_core = $ncpu" >> me5_configuration.txt
fi

#########################################
# FORCE IT TO PRODUCE EXACTLY THE REQUIRED NUMBER OF EVENTS
#########################################

# define max event per iteration as 5000 if n_evt<45000 or n_evt/9 otherwise
max_events_per_iteration=$(( $nevt > 5000*9 ? ($nevt / 9) + ($nevt % 9 > 0) : 5000 ))
# set starting variables
produced_lhe=0
run_counter=0
# if rnum allows, multiply by 10 to avoid multiple runs 
# with the same seed across the workflow
run_random_start=$(($rnum*10))
# otherwise don't change the seed and increase number of events as 10000 if n_evt<50000 or n_evt/9 otherwise
if [  $run_random_start -gt "89999990" ]; then
    run_random_start=$rnum
    max_events_per_iteration=$(( $nevt > 10000*9 ? ($nevt / 9) + ($nevt % 9 > 0) : 10000 ))
fi

while [ $produced_lhe -lt $nevt ]; do
  
  # set the incremental iteration seed
  run_random_seed=$(($run_random_start + $run_counter))
  # increase the iteration counter
  let run_counter=run_counter+1 
  
  # don't allow more than 90 iterations
  if [  $run_counter -gt "90" ]; then
      echo "asking for more than 90 iterations, this should never happen"
      break
  fi
  # compute remaining events
  remaining_event=$(($nevt - $produced_lhe))
  
  echo "Running MG5_aMC for the "$run_counter" time"
  # set number of events to max_events_per_iteration or residual ones if less than that
  submitting_event=$(( $remaining_event < $max_events_per_iteration ? $remaining_event : $max_events_per_iteration ))
  # run mg5_amc
  echo "produced_lhe " $produced_lhe "nevt " $nevt "submitting_event " $submitting_event " remaining_event " $remaining_event
  echo run.sh $submitting_event $run_random_seed
  $path_to_gridpack/process/run.sh $submitting_event $run_random_seed
  
  # compute number of events produced in the iteration
  produced_lhe=$(($produced_lhe+`zgrep \<event events.lhe.gz | wc -l`))
  
  # rename output file to avoid overwriting
  mv events.lhe.gz events_${run_counter}.lhe.gz
  echo "run "$run_counter" finished, total number of produced events: "$produced_lhe"/"$nevt
  
  echo ""
done

# merge multiple lhe files if needed
ls -lrt events*.lhe.gz
if [  $run_counter -gt "1" ]; then
    echo "Merging files and deleting unmerged ones"
    cp /cvmfs/cms.cern.ch/phys_generator/gridpacks/lhe_merger/merge.pl ./
    chmod 755 merge.pl
    # ./madevent/bin/internal/merge.pl events*.lhe.gz events.lhe.gz banner.txt
    ./merge.pl events*.lhe.gz events.lhe.gz banner.txt
    rm events_*.lhe.gz banner.txt;
else
    mv events_${run_counter}.lhe.gz events.lhe.gz
fi

#########################################
echo "run finished, produced number of events:"
zgrep \<event events.lhe.gz |wc -l

domadspin=0 
if [ -f $path_to_gridpack/process/madspin_card.dat ]; then #TODO - test in read-only mode
    domadspin=1
    cp $path_to_gridpack/process/madspin_card.dat .
    echo "import events.lhe.gz" > madspinrun.dat
    rnum2=$(($rnum+1000000))
    echo `echo "set seed $rnum2"` >> madspinrun.dat
    cat ./madspin_card.dat >> madspinrun.dat
    cat madspinrun.dat | $path_to_gridpack/mgbasedir/MadSpin/madspin
fi
cd $LHEWORKDIR

runlabel=GridRun_PostProc_${rnum}
mkdir ${runlabel}

event_file=events.lhe.gz
if [ "$domadspin" -gt "0" ] ; then 
    event_file=events_decayed.lhe.gz
fi
mv $event_file ${runlabel}/events.lhe.gz

# Add scale and PDF weights using systematics module
pdfsets="320900,11082,13091@0,13191@0,13202@0,23100,23300,23490,23600,23790,25410,25510,25570,25605,25620,25710,25770,25805,25840,92000,306000,320500,260400,262400@0,263400@0,292000,292400@0"
scalevars="--mur=1,2,0.5 --muf=1,2,0.5 --together=muf,mur,dyn --dyn=-1,1,2,3,4"
python $path_to_gridpack/process/madevent/bin/internal/systematics.py ${runlabel}/events.lhe.gz ${runlabel}/events_sys.lhe.gz --lhapdf_config=$LHAPDFCONFIG --remove_wgts=all --start_id=1001 --pdf=$pdfsets $scalevars

mv ${runlabel}/events.lhe.gz cmsgrid_final.lhe.gz
gzip -d cmsgrid_final.lhe.gz

#reweight if necessary
if [ -e $path_to_gridpack/process/madevent/Cards/reweight_card.dat ]; then #TODO - test in read-only mode
    echo "reweighting events"
    mv cmsgrid_final.lhe $runlabel/unweighted_events.lhe

    echo "WARNING: bypass the creation of the RunWeb for the read-only gridpack ..."
    python $path_to_gridpack/process/madevent/bin/internal/madevent_interface.py reweight -f GridRun_${run_random_start}
    mv $runlabel/unweighted_events.lhe.gz cmsgrid_final.lhe.gz
fi

gzip -d  cmsgrid_final.lhe.gz

ls -l
echo

exit 0







