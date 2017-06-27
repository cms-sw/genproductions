#!/bin/bash

# scram_arch_version=${1}
scram_arch_version=slc6_amd64_gcc481
echo "%MSG-MG5 SCRAM_ARCH version = $scram_arch_version"

# cmssw_version=${2}
cmssw_version=CMSSW_7_1_28
echo "%MSG-MG5 CMSSW version = $cmssw_version"

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`
export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
source $VO_CMS_SW_DIR/cmsset_default.sh
export SCRAM_ARCH=${scram_arch_version}
scramv1 project CMSSW ${cmssw_version}
cd ${cmssw_version}/src
eval `scramv1 runtime -sh`
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

#generate events
./run.sh $nevt $rnum

domadspin=0
if [ -f ./madspin_card.dat ] ;then
    domadspin=1
    echo "import events.lhe.gz" > madspinrun.dat
    rnum2=$(($rnum+1000000))
    echo `echo "set seed $rnum2"` >> madspinrun.dat
    cat ./madspin_card.dat >> madspinrun.dat
    cat madspinrun.dat | $LHEWORKDIR/mgbasedir/MadSpin/madspin
fi

cd $LHEWORKDIR

if [ "$domadspin" -gt "0" ] ; then 
    mv process/events_decayed.lhe.gz events_presys.lhe.gz
else
    mv process/events.lhe.gz events_presys.lhe.gz
fi

gzip -d events_presys.lhe.gz

# Remove the bias rwgt block that causes SysCalc to fail (patch 9 fixes this)
#sed -i ':a;N;$!ba;s/<rwgt>\n<wgt id='\''bias'\''>   0.1000000E+01<\/wgt>\n<\/rwgt>\n//g' events_presys.lhe


use_syscalc=0
if [ "$use_syscalc" -gt 1 ]; then
    #run syscalc to populate pdf and scale variation weights
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
    NNPDF30_lo_as_0130.LHgrid
    NNPDF30_lo_as_0130_nf_4.LHgrid
    NNPDF30_lo_as_0118.LHgrid 1
    NNPDF23_lo_as_0130_qed.LHgrid
    NNPDF23_lo_as_0119_qed.LHgrid 1
    cteq6l1.LHgrid
    MMHT2014lo68cl.LHgrid
    MMHT2014lo_asmzsmallrange.LHgrid
    HERAPDF15LO_EIG.LHgrid
    NNPDF30_nlo_as_0118.LHgrid 1
    NNPDF23_nlo_as_0119.LHgrid 1
    CT10nlo.LHgrid
    MMHT2014nlo68cl.LHgrid 1
    " > syscalc_card.dat
    
    LD_LIBRARY_PATH=`${LHAPDFCONFIG} --libdir`:${LD_LIBRARY_PATH} ./mgbasedir/SysCalc/sys_calc events_presys.lhe syscalc_card.dat cmsgrid_final.lhe
else 
   echo "systematics events_presys.lhe cmsgrid_final.lhe --dyn=-1 --mur=1,2,0.5 --muf=1,2,0.5 --pdf=NNPDF30_lo_as_0130,NNPDF30_lo_as_0130_nf_4,NNPDF30_lo_as_0118@0,NNPDF23_lo_as_0130_qed,NNPDF23_lo_as_0119_qed@0,cteq6l1,MMHT2014lo68cl,MMHT2014lo_asmzsmallrange,HERAPDF15LO_EIG,NNPDF30_nlo_as_0118@0,NNPDF23_nlo_as_0119@0,CT10nlo,MMHT2014nlo68cl@0" > systematics_card.dat
   cat systematics_card.dat | $LHEWORKDIR/process/madevent/bin/madevent 
fi

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

ls -l
echo

exit 0
