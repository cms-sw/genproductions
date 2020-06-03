#! /bin/bash

PROCS="Pair Single" #NonRes
MASSES=800 #`seq 800 300 2300`
LAMBDAS="1.5" # 2.0 2.5"

for proc in $PROCS; do
  proc="${proc}ScalarLQToBTau"
  echo
  echo ">>> Generating process ${proc} ... "
  for lambda in $LAMBDAS; do
    for mass in $MASSES; do
      [ $lambda = 1.0 -a $mass = 500 ] && continue
      echo ">>> Mass = ${mass}, lambda = $lambda"
      
      SAMPLE0="${proc}_M500_L1p0"
      SAMPLE="${proc}_M${mass}_L${lambda/./p}"
      CARDDIR="$SAMPLE"
      rm -rfv $CARDDIR
      cp -rv $SAMPLE0 $CARDDIR
      
      CARDS="extramodels customizecards proc_card run_card"
      for card in $CARDS; do
        mv -v $CARDDIR/${SAMPLE0}_${card}.dat $CARDDIR/${SAMPLE}_${card}.dat
      done
      
      SCALES="scale dsqrt_q2fact1 dsqrt_q2fact2"
      for scale in $SCALES; do
        sed -i -e "s/$scale ${mass}/$scale ${mass}/" $CARDDIR/${SAMPLE}_customizecards.dat
      done
      sed -i -e "s/FRBLOCK 3 1.0/FRBLOCK 3 ${lambda}/" $CARDDIR/${SAMPLE}_customizecards.dat
      sed -i -e "s/MASS 9000006 500/MASS 9000006 ${mass}/" $CARDDIR/${SAMPLE}_customizecards.dat
      sed -i -e "s/output ${proc}_M500/output $CARDDIR/" $CARDDIR/${SAMPLE}_proc_card.dat
      
    done
  done
done
echo