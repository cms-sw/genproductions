#/bin/bash

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`

cd process

#make sure lhapdf points to local cmssw installation area
echo "lhapdf = `echo "$LHAPATH/../../../full/bin/lhapdf-config"`" >> ./madevent/Cards/me5_configuration.txt

if [ "$ncpu" -gt "1" ]; then
  echo "run_mode = 2" >> ./madevent/Cards/me5_configuration.txt
  echo "nb_core = $ncpu" >> ./madevent/Cards/me5_configuration.txt
fi

#generate events
./run.sh $nevt $rnum

domadspin=0
if [ -f ./madevent/Cards/madspin_card.dat ] ;then
    domadspin=1
    echo "import events.lhe.gz" > madspinrun.dat
    rnum2=$(($rnum+1000000))
    echo `echo "set seed $rnum2"` >> madspinrun.dat
    cat ./madevent/Cards/madspin_card.dat >> madspinrun.dat
    cat madspinrun.dat | $LHEWORKDIR/mgbasedir/MadSpin/madspin
fi

cd $LHEWORKDIR

if [ "$domadspin" -gt "0" ] ; then 
    mv process/events_decayed.lhe.gz events_presys.lhe.gz
else
    mv process/events.lhe.gz events_presys.lhe.gz
fi

gzip -d events_presys.lhe.gz

#run syscalc to populate pdf and scale variation weights
echo "
# Central scale factors
scalefact:
0.5 1 2
# choice of correlation scheme between muF and muR
scalecorrelation:
-1
# PDF sets and number of members (0 or none for all members)
PDF:
CT10nlo.LHgrid
#MSTW2008nlo68cl.LHgrid 1
NNPDF23_nlo_as_0119.LHgrid 1
" > syscalc_card.dat

./mgbasedir/SysCalc/sys_calc events_presys.lhe syscalc_card.dat cmsgrid_final.lhe




ls -l
echo

exit 0