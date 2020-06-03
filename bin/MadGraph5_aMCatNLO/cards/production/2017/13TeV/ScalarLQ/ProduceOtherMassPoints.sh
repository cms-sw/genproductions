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
      
      # SETTINGS
      SAMPLE0="${proc}_M500_L1p0"
      SAMPLE="${proc}_M${mass}_L${lambda/./p}"
      CARDDIR="$SAMPLE"
      
      # CREATE CARDS
      rm -rfv $CARDDIR
      cp -rv $SAMPLE0 $CARDDIR
      CARDS="extramodels customizecards proc_card run_card"
      for card in $CARDS; do
        mv -v $CARDDIR/${SAMPLE0}_${card}.dat $CARDDIR/${SAMPLE}_${card}.dat
      done
      
      # SET PARAMETERS
      SCALES="scale dsqrt_q2fact1 dsqrt_q2fact2"
      for scale in $SCALES; do
        sed -i -e "s/$scale 500/$scale ${mass}/" $CARDDIR/${SAMPLE}_customizecards.dat
        sed -i -e "s/500\(\s*= ${scale}\)/${mass}\1/" $CARDDIR/${SAMPLE}_run_card.dat # should be covered by customizecards.dat
      done
      sed -i -e "s/FRBLOCK 3 1.0/FRBLOCK 3 ${lambda}/" $CARDDIR/${SAMPLE}_customizecards.dat
      sed -i -e "s/MASS 9000006 500/MASS 9000006 ${mass}/" $CARDDIR/${SAMPLE}_customizecards.dat
      sed -i -e "s/output $SAMPLE0/output $SAMPLE/" $CARDDIR/${SAMPLE}_proc_card.dat
      
    done
  done
done
echo