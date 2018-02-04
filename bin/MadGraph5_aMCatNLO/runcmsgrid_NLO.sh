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

cd process

#make sure lhapdf points to local cmssw installation area
LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`

echo "lhapdf = $LHAPDFCONFIG" >> ./Cards/amcatnlo_configuration.txt
# echo "cluster_local_path = `${LHAPDFCONFIG} --datadir`" >> ./Cards/amcatnlo_configuration.txt

echo "run_mode = 2" >> ./Cards/amcatnlo_configuration.txt
echo "nb_core = $ncpu" >> ./Cards/amcatnlo_configuration.txt

echo "done" > runscript.dat
echo "reweight=OFF" > runscript.dat
echo "set nevents $nevt" >> runscript.dat
echo "set iseed $rnum" >> runscript.dat

#set job splitting for worker processes
nevtjob=$[$nevt/$ncpu]
if [ "$nevtjob" -lt "10" ]; then
  nevtjob=10
fi

echo "set nevt_job ${nevtjob}" >> runscript.dat

echo "done" >> runscript.dat

domadspin=0
if [ -f ./Cards/madspin_card.dat ] ;then
  #set random seed for madspin
  rnum2=$(($rnum+1000000))
  echo "$(echo `echo "set seed $rnum2"` | cat - ./Cards/madspin_card.dat)" > ./Cards/madspin_card.dat
  domadspin=1
fi

runname=cmsgrid


#generate events

#First check if normal operation with MG5_aMCatNLO events is planned
if [ ! -e $LHEWORKDIR/header_for_madspin.txt ]; then
  
    cat runscript.dat | ./bin/generate_events -ox -n $runname

    runlabel=$runname
    if [ "$domadspin" -gt "0" ] ; then 
      runlabel=${runname}_decayed_1
    fi

    if [ -f ./Cards/reweight_card.dat ] ;then
        rwgt_dir="$LHEWORKDIR/process/rwgt"
        export PYTHONPATH=$rwgt_dir:$PYTHONPATH
        echo "0" | ./bin/aMCatNLO --debug reweight pilotrun
    fi

    pdfsets="PDF_SETS_REPLACE"
    scalevars="--mur=1,2,0.5 --muf=1,2,0.5 --together=muf,mur --dyn=-1"

    echo "systematics $runlabel --remove_wgts=all --start_id=1001 --pdf=$pdfsets $scalevars" | ./bin/aMCatNLO
	cp ./Events/${runlabel}/events.lhe.gz $LHEWORKDIR/${runname}_final.lhe.gz

#else handle external tarball
else
    cd $LHEWORKDIR/external_tarball

    ./runcmsgrid.sh $nevtjob $rnum $ncpu

#splice blocks needed for MadSpin into LHE file
    sed -i "/<init>/ {
         h
         r ../header_for_madspin.txt
         g
         N
     }" cmsgrid_final.lhe

#check if lhe file has reweighting blocks - temporarily save those, because MadSpin later overrides the entire header
    if grep -R "<initrwgt>" cmsgrid_final.lhe; then
        sed -n '/<initrwgt>/,/<\/initrwgt>/p' cmsgrid_final.lhe > ../initrwgt.txt
    fi

    mv cmsgrid_final.lhe ../cmsgrid_predecay.lhe
    cd $LHEWORKDIR
    rm -r external_tarball
    echo "import $LHEWORKDIR/cmsgrid_predecay.lhe" > madspinrun.dat
    echo "set ms_dir $LHEWORKDIR/process/madspingrid" >> madspinrun.dat
    echo "launch" >> madspinrun.dat
    cat madspinrun.dat | $LHEWORKDIR/mgbasedir/MadSpin/madspin
    rm madspinrun.dat
    rm cmsgrid_predecay.lhe.gz
    mv cmsgrid_predecay_decayed.lhe.gz cmsgrid_final.lhe.gz
    
    if [ -e initrwgt.txt ];then
    	gzip -d cmsgrid_final.lhe.gz
    	sed -i "/<\/header>/ {
             h
             r initrwgt.txt
             g
             N
        }" cmsgrid_final.lhe
        rm initrwgt.txt
        gzip cmsgrid_final.lhe
    fi

    
fi

cd $LHEWORKDIR
gzip -d ${runname}_final.lhe.gz
sed -i -e '/<mgrwgt/,/mgrwgt>/d' ${runname}_final.lhe 
ls -l
echo

exit 0
