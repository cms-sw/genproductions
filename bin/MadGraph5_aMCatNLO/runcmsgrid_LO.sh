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

if [ ! -z "${PYTHON27PATH}" ] ; then export PYTHONPATH=${PYTHON27PATH} ; fi 

cd $LHEWORKDIR/process

#make sure lhapdf points to local cmssw installation area
LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`

# workaround for el8
LHAPDFLIBS=`$LHAPDFCONFIG --libdir`
LHAPDFPYTHONVER=`find $LHAPDFLIBS -name "python*" -type d -exec basename {} \;`
LHAPDFPYTHONLIB=`find $LHAPDFLIBS/$LHAPDFPYTHONVER/site-packages -name "*.egg" -type d -exec basename {} \;`
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$LHAPDFLIBS

if [ ! -z "${LHAPDFPYTHONLIB}" ] ; then
  export PYTHONPATH=$PYTHONPATH:$LHAPDFLIBS/$LHAPDFPYTHONVER/site-packages/$LHAPDFPYTHONLIB
else
  export PYTHONPATH=$PYTHONPATH:$LHAPDFLIBS/$LHAPDFPYTHONVER/site-packages
fi

echo "lhapdf_py3 = $LHAPDFCONFIG" >> ./madevent/Cards/me5_configuration.txt
# echo "cluster_local_path = `${LHAPDFCONFIG} --datadir`" >> ./madevent/Cards/me5_configuration.txt
#To overcome problem of taking toomanythreads
#if [ "$ncpu" -gt "1" ]; then
echo "run_mode = 2" >> ./madevent/Cards/me5_configuration.txt
echo "nb_core = $ncpu" >> ./madevent/Cards/me5_configuration.txt
#fi

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
  ./run.sh $submitting_event $run_random_seed
  
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
    # use version in genproduction, not cvmfs, nor mg5_amc@nlo default 
    perl $LHEWORKDIR/merge.pl events*.lhe.gz events.lhe.gz banner.txt
    rm events_*.lhe.gz banner.txt;
else
    mv events_${run_counter}.lhe.gz events.lhe.gz
fi

#########################################
#########################################
#########################################

echo "run finished, produced number of events:"
zgrep \<event events.lhe.gz |wc -l

#reweight if necessary
doreweighting=0
if [ -e ./madevent/Cards/reweight_card.dat ]; then
    echo "reweighting events"
    doreweighting=1
    mkdir -p ./madevent/Events/GridRun_${rnum}/
    mv events.lhe.gz ./madevent/Events/GridRun_${rnum}/unweighted_events.lhe.gz
    cd madevent
    echo "0" |./bin/madevent --debug reweight GridRun_${rnum}
    cd ..
    mv $LHEWORKDIR/process/madevent/Events/GridRun_${rnum}/unweighted_events.lhe.gz $LHEWORKDIR/process/events.lhe.gz
fi

gzip -d $LHEWORKDIR/process/events.lhe.gz

domadspin=0
#checking for a madspin_card in the InputCards directory,
#rather than checking for it in the process directory, since run.sh will remove *.dat from that directory,
#if it fails to generate any event in the first iteration 
if [ -f $LHEWORKDIR/InputCards/*madspin_card.dat ] ;then
    domadspin=1
    # extract header as overwritten by madspin 
    if grep -R "<initrwgt>" events.lhe ; then
	sed -n '/<initrwgt>/,/<\/initrwgt>/p' events.lhe >  initrwgt.txt
    fi
    echo "import events.lhe" > madspinrun.dat
    rnum2=$(($rnum+1000000))
    echo `echo "set seed $rnum2"` >> madspinrun.dat
    cat $LHEWORKDIR/InputCards/*madspin_card.dat >> madspinrun.dat
    $LHEWORKDIR/mgbasedir/MadSpin/madspin madspinrun.dat 
    # add header back 
    gzip -d events_decayed.lhe.gz  
    if [ -e initrwgt.txt ]; then
	sed -i "/<\/header>/ {
             h
             r initrwgt.txt
             g
             N
        }" events_decayed.lhe
	rm initrwgt.txt
    fi
fi

# Test if time_of_flight is set to a positive floating point value
has_time_of_flight=$(egrep "^\s*\+?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?\s*=\s*time_of_flight" ./madevent/Cards/run_card.dat)
if [ ! -z "$has_time_of_flight" ] ; then
    ./madevent/bin/madevent add_time_of_flight events.lhe
fi
    

cd $LHEWORKDIR

runlabel=GridRun_PostProc_${rnum}
mkdir process/madevent/Events/${runlabel}

event_file=events.lhe
if [ "$domadspin" -gt "0" ] ; then 
    event_file=events_decayed.lhe
fi
mv process/$event_file process/madevent/Events/${runlabel}/events.lhe

# add scale and PDF weights using systematics module
pushd process/madevent
pdfsets="PDF_SETS_REPLACE"
scalevars="--mur=1,2,0.5 --muf=1,2,0.5 --together=muf,mur,dyn --dyn=-1,1,2,3,4 --alps=0.5,1,2"
echo "systematics $runlabel --start_id=1001 --pdf=$pdfsets $scalevars" | ./bin/madevent
popd

# check lhe output  
echo -e "\nRun xml check" 
xmllint --stream --noout ${LHEWORKDIR}/process/madevent/Events/${runlabel}/events.lhe ; test $? -eq 0 || exit 1 
echo "Number of weights that are NaN:" 
grep  NaN  ${LHEWORKDIR}/process/madevent/Events/${runlabel}/events.lhe | grep "</wgt>" | wc -l ; test $? -eq 0 || exit 1 
echo -e "All checks passed \n" 

# copy output and print directory 
mv ${LHEWORKDIR}/process/madevent/Events/${runlabel}/events.lhe ${LHEWORKDIR}/cmsgrid_final.lhe
ls -l

# exit 
exit 0
