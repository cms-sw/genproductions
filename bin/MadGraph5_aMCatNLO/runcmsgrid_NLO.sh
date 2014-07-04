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
echo "lhapdf = `echo "$LHAPATH/../../../full/bin/lhapdf-config"`" >> ./Cards/amcatnlo_configuration.txt

# if [ "$ncpu" -gt "1" ]; then
#   echo "run_mode = 2" >> ./Cards/amcatnlo_configuration.txt
#   echo "nb_core = $ncpu" >> ./Cards/amcatnlo_configuration.txt
# fi

echo "run_mode = 2" >> ./Cards/amcatnlo_configuration.txt
echo "nb_core = $ncpu" >> ./Cards/amcatnlo_configuration.txt

echo "done" > runscript.dat
echo "set nevents $nevt" >> runscript.dat
echo "set iseed $rnum" >> runscript.dat
#set job splitting for worker processes
if [ "$ncpu" -gt "1" ]; then
  echo "set nevt_job $[$nevt/$ncpu]" >> runscript.dat
else
  echo "set nevt_job -1" >> runscript.dat
fi
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
cat runscript.dat | ./bin/generate_events -ox -n $runname

if [ "$domadspin" -gt "0" ] ; then 
    mv ./Events/${runname}_decayed_1/events.lhe.gz $LHEWORKDIR/${runname}_final.lhe.gz
else
    mv ./Events/${runname}/events.lhe.gz $LHEWORKDIR/${runname}_final.lhe.gz
fi

cd $LHEWORKDIR
gzip -d ${runname}_final.lhe.gz

ls -l
echo

exit 0