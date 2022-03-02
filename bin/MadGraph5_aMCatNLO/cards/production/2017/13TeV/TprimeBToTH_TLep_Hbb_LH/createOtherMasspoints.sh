#! /bin/bash

PROCNAME=TprimeBToTH_TLep_Hbb_LH
MASSES_T="600 700 800 900 1000 1100 1200"
MASSES_H="75 100 125 175 200 250 350 450 500"
PDF=325500 # 4-f version of 303600 for CP5, 2017 and 2018
#PDF=263400 # 4-f version of 263000 for TuneCUETP8M1, 2016



for mt in $MASSES_T; do
  for mh in $MASSES_H; do
    [ $mt = 600 -a $mh = 75 ] && continue
    echo
    echo ">>> Mass T = ${mt}, mass H = $mh"

    # SETTINGS
    SAMPLE_TEMPLATE="${PROCNAME}_MT600_MH75"
    SAMPLE="${PROCNAME}_MT${mt}_MH${mh}"
    CARDDIR="$SAMPLE"

    # CREATE CARDS
    rm -rfv $CARDDIR
    cp -rv $SAMPLE_TEMPLATE $CARDDIR
    CARDS="extramodels customizecards proc_card run_card"
    for card in $CARDS; do
      mv -v $CARDDIR/${SAMPLE_TEMPLATE}_${card}.dat $CARDDIR/${SAMPLE}_${card}.dat
    done

    # SET PARAMETERS
    sed -i -e "s/mass 6000006 600/mass 6000006 ${mt}/" $CARDDIR/${SAMPLE}_customizecards.dat
    sed -i -e "s/mass 25 75/mass 25 ${mh}/" $CARDDIR/${SAMPLE}_customizecards.dat
    sed -i -e "s/^[^#]\+\(\s*=\s*lhaid\)/ ${PDF} \1/" $CARDDIR/${SAMPLE}_run_card.dat
    sed -i -e "s/output $SAMPLE_TEMPLATE/output $SAMPLE/" $CARDDIR/${SAMPLE}_proc_card.dat

    diff $SAMPLE_TEMPLATE/${SAMPLE_TEMPLATE}_proc_card.dat $CARDDIR/${SAMPLE}_proc_card.dat
    diff $SAMPLE_TEMPLATE/${SAMPLE_TEMPLATE}_customizecards.dat $CARDDIR/${SAMPLE}_customizecards.dat
    diff $SAMPLE_TEMPLATE/${SAMPLE_TEMPLATE}_run_card.dat $CARDDIR/${SAMPLE}_run_card.dat
  done
done
echo
