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

#if lhapdf6 external is available then above points to lhapdf5 and needs to be overridden
LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
if [ -e $LHAPDF6TOOLFILE ]; then
  LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
fi

#make sure env variable for pdfsets points to the right place
export LHAPDF_DATA_PATH=`$LHAPDFCONFIG --datadir`

echo "lhapdf = $LHAPDFCONFIG" >> ./madevent/Cards/me5_configuration.txt
# echo "cluster_local_path = `${LHAPDFCONFIG} --datadir`" >> ./madevent/Cards/me5_configuration.txt

if [ "$ncpu" -gt "1" ]; then
  echo "run_mode = 2" >> ./madevent/Cards/me5_configuration.txt
  echo "nb_core = $ncpu" >> ./madevent/Cards/me5_configuration.txt
fi

#########################################
# FORCE IT TO PRODUCE EXACTLY THE REQUIRED NUMBER OF EVENTS
#########################################

echo "produce backup tarball"
cd $LHEWORKDIR
tar cfJ gridpack.tar.xz ./
cd process

produced_lhe=0
run_counter=0
run_random_seed=$rnum
remaining_event=$nevt

echo "start the loop"
while [ $produced_lhe -lt $nevt ]; do

  let run_counter=run_counter+1 
  echo Running MG5_aMC for the $run_counter time
  remaining_event=$(($remaining_event - $produced_lhe))

  if [  $run_counter -gt "1" ]; then
      echo "refresing the working directory (i.e. remove mgbasedir, process and and untar gridpack again"
      cd $LHEWORKDIR
      rm -rf mgbasedir process
      tar xf gridpack.tar.xz
      cd process
  fi
  
  #generate events
  echo run.sh $remaining_event $run_random_seed
  ./run.sh $remaining_event $run_random_seed # TO REALLY PRODUCE EVENTS
  
  produced_lhe=$(($produced_lhe+`zgrep \<event events.lhe.gz | wc -l`))
  mv events.lhe.gz $LHEWORKDIR/events_${run_counter}.lhe.gz
  # cp $LHEWORKDIR/events_${run_counter}.lhe.gz $LHEWORKDIR/events.lhe.gz # TO DELETE
  
  echo "run "$run_counter" finished, total number of produced events: "$produced_lhe"/"$nevt
  run_random_seed=$(($run_random_seed + 1))
  
  # if [  $run_counter -eq "10" ]; then
      # break
  # fi
  
done


cd $LHEWORKDIR
# rm events.lhe.gz # TO DELETE

ls -lrt events*.lhe.gz
if [  $run_counter -gt "1" ]; then
    echo "Merging files and deleting unmerged ones"
    ./madevent/bin/internal/merge.pl events*.lhe.gz events.lhe.gz banner.txt
    rm events_*.lhe.gz banner.txt;
else
    mv events_${run_counter}.lhe.gz events.lhe.gz
fi

mv events.lhe.gz process/

#########################################
#########################################
#########################################

echo "run finished, produced number of events:"
zgrep \<event events.lhe.gz |wc -l

cd $LHEWORKDIR

mv process/events.lhe.gz events_presys.lhe.gz
gzip -d events_presys.lhe.gz

echo "
# Central scale factors
scalefact:
1 2 0.5
# choice of correlation scheme between muF and muR
# set here to reproduce aMC@NLO order
scalecorrelation:
0 3 6 1 4 7 2 5 8
# PDF sets and number of members (0 or none for all members)
PDF:
" > syscalc_card.dat

is5FlavorScheme=PDF_FLAVOR_SCHEME_REPLACE

#run syscalc to populate pdf and scale variation weights
if [ $is5FlavorScheme -eq 1 ]; then
  # 5F PDF
  echo "
  NNPDF31_nnlo_hessian_pdfas.LHgrid
  NNPDF31_nnlo_as_0108.LHgrid 1
  NNPDF31_nnlo_as_0110.LHgrid 1
  NNPDF31_nnlo_as_0112.LHgrid 1
  NNPDF31_nnlo_as_0114.LHgrid 1
  NNPDF31_nnlo_as_0117.LHgrid 1
  NNPDF31_nnlo_as_0119.LHgrid 1
  NNPDF31_nnlo_as_0122.LHgrid 1
  NNPDF31_nnlo_as_0124.LHgrid 1
  NNPDF31_nlo_hessian_pdfas.LHgrid
  CT14nnlo.LHgrid
  CT14nnlo_as_0116.LHgrid 1
  CT14nnlo_as_0120.LHgrid 1
  CT14nlo.LHgrid
  CT14nlo_as_0116.LHgrid 1
  CT14nlo_as_0120.LHgrid 1
  CT14lo.LHgrid 1
  MMHT2014nlo68clas118.LHgrid
  MMHT2014nnlo68cl.LHgrid
  MMHT2014lo68cl.LHgrid 1
  ABMP16als118_5_nnlo.LHgrid
  PDF4LHC15_nlo_100_pdfas.LHgrid
  PDF4LHC15_nnlo_100_pdfas.LHgrid
  PDF4LHC15_nlo_30_pdfas.LHgrid
  PDF4LHC15_nnlo_30_pdfas.LHgrid
  HERAPDF20_NLO_EIG.LHgrid
  HERAPDF20_NLO_VAR.LHgrid
  HERAPDF20_NNLO_EIG.LHgrid
  HERAPDF20_NNLO_VAR.LHgrid
  CT14qed_inc_proton.LHgrid
  LUXqed17_plus_PDF4LHC15_nnlo_100.LHgrid
  NNPDF30_nlo_nf_5_pdfas.LHgrid
  NNPDF30_nnlo_nf_5_pdfas.LHgrid 1
  NNPDF31_lo_as_0118.LHgrid 1
  NNPDF31_lo_as_0130.LHgrid 1
  NNPDF30_lo_as_0118.LHgrid 1
  NNPDF30_lo_as_0130.LHgrid 1
 " >> syscalc_card.dat

else
 # 4F PDF
  echo "
  NNPDF31_nnlo_hessian_pdfas.LHgrid
  CT10nlo_nf4.LHgrid
  CT14nnlo_NF4.LHgrid 1
  CT14nlo_NF4.LHgrid 1
  CT14lo_NF4.LHgrid 1
  MSTW2008lo68cl_nf4.LHgrid
  MSTW2008nlo68cl_nf4.LHgrid
  MSTW2008nlo_mbrange_nf4.LHgrid
  MSTW2008nnlo68cl_nf4.LHgrid
  MSTW2008nnlo_mbrange_nf4.LHgrid
  MMHT2014nlo68cl_nf4.LHgrid
  MMHT2014nlo68clas118_nf4.LHgrid
  MMHT2014nlo_asmzsmallrange_nf4.LHgrid
  MMHT2014nlo_mcrange_nf4.LHgrid
  MMHT2014nlo_mbrange_nf4.LHgrid
  MMHT2014nnlo68cl_nf4.LHgrid
  MMHT2014nnlo_asmzsmallrange_nf4.LHgrid
  MMHT2014nnlo_mcrange_nf4.LHgrid
  MMHT2014nnlo_mbrange_nf4.LHgrid
  PDF4LHC15_nlo_nf4_30.LHgrid
  NNPDF31_nnlo_as_0118_nf_4.LHgrid
  NNPDF31_nlo_as_0118_nf_4.LHgrid
  NNPDF30_nlo_as_0118_nf_4.LHgrid
  NNPDF30_lo_as_0118_nf_4.LHgrid 1
  NNPDF30_lo_as_0130_nf_4.LHgrid 1
  NNPDF30_nlo_nf_4_pdfas.LHgrid
  NNPDF30_nnlo_nf_4_pdfas.LHgrid 1
  " >> syscalc_card.dat
fi

LD_LIBRARY_PATH=`${LHAPDFCONFIG} --libdir`:${LD_LIBRARY_PATH} ./mgbasedir/SysCalc/sys_calc events_presys.lhe syscalc_card.dat cmsgrid_final.lhe

#reweight if necessary
if [ -e process/madevent/Cards/reweight_card.dat ]; then
    echo "reweighting events"
    mv cmsgrid_final.lhe process/madevent/Events/GridRun_${rnum}/unweighted_events.lhe
    cd process/madevent
    ./bin/madevent reweight -f GridRun_${rnum}
    cd ../..
    mv process/madevent/Events/GridRun_${rnum}/unweighted_events.lhe.gz cmsgrid_final.lhe.gz
    gzip -d  cmsgrid_final.lhe.gz
fi

if [ -f process/madspin_card.dat ] ;then
    mv cmsgrid_final.lhe process
    cd process
    gzip  cmsgrid_final.lhe
    echo "import cmsgrid_final.lhe.gz" > madspinrun.dat
    rnum2=$(($rnum+1000000))
    echo `echo "set seed $rnum2"` >> madspinrun.dat
    cat ./madspin_card.dat >> madspinrun.dat
    cat madspinrun.dat | $LHEWORKDIR/mgbasedir/MadSpin/madspin
    cd $LHEWORKDIR
    mv process/cmsgrid_final_decayed.lhe.gz cmsgrid_final.lhe.gz
    gzip -d  cmsgrid_final.lhe.gz
fi

ls -l
echo

exit 0
