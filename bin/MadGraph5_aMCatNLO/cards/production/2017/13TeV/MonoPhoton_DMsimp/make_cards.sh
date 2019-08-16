#!/bin/bash

#################################################
### Script to generate cards directories for  ###
### monophoton DMsimp samples (pta > 130 GeV) ###
#################################################

CARDDIR=$(cd $(dirname $0); pwd)

for MMED in 50 100 500 1000 1500 2000
do
  for MDM in 1 10 50 150 500
  do
    [ $MDM -eq $(($MMED/2)) ] && MDM=$(($MDM-10))
    [ $MDM -gt $(($MMED/2)) ] && break

    CARDLABEL=Mphi-${MMED}_Mchi-${MDM}_gSM-0p25_gDM-1p0_pta130

    ## Vector
    CARDNAME=Vector_MonoPhoton_NLO_${CARDLABEL}    
    DIR=$CARDDIR/Vector_${CARDLABEL}

    mkdir -p $DIR

    cp $CARDDIR/extramodels.dat $DIR/${CARDNAME}_extramodels.dat
    cp $CARDDIR/run_card.dat $DIR/${CARDNAME}_run_card.dat
    sed -e "s/_MMED_/$MMED/" -e "s/_MDM_/$MDM/" -e "s/_gDMV_/1/" -e "s/_gVq_/0.25/" -e "s/_gDMA_/0/" -e "s/_gAq_/0/" $CARDDIR/customizecards.dat > $DIR/${CARDNAME}_customizecards.dat
    sed "s/_NAME_/$CARDNAME/" $CARDDIR/proc_card.dat > $DIR/${CARDNAME}_proc_card.dat

    ## Axial
    CARDNAME=Axial_MonoPhoton_NLO_${CARDLABEL}    
    DIR=$CARDDIR/Axial_${CARDLABEL}

    mkdir -p $DIR

    cp $CARDDIR/extramodels.dat $DIR/${CARDNAME}_extramodels.dat
    cp $CARDDIR/run_card.dat $DIR/${CARDNAME}_run_card.dat
    sed -e "s/_MMED_/$MMED/" -e "s/_MDM_/$MDM/" -e "s/_gDMV_/0/" -e "s/_gVq_/0/" -e "s/_gDMA_/1/" -e "s/_gAq_/0.25/" $CARDDIR/customizecards.dat > $DIR/${CARDNAME}_customizecards.dat
    sed "s/_NAME_/$CARDNAME/" $CARDDIR/proc_card.dat > $DIR/${CARDNAME}_proc_card.dat

  done
done
