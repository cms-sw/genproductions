#!/bin/sh
CARDDIR=$PWD
echo $CARDDIR
i=1
while IFS= read -r line
do
    MassPhi=$(echo $line | awk -F " " '{print$1}')
    MassChi=$(echo $line | awk -F " " '{print$2}')
    
    echo  $MassPhi $MassChi
    
    MassPhi_=$(echo ${MassPhi} | sed -e 's/\./p/' -e 's/-/m/'  -e 's/-/m/')
    MassChi_=$(echo ${MassChi} | sed -e 's/\./p/' -e 's/-/m/'  -e 's/-/m/')
    CARDLABEL2=Vector_MonoTop_NLO_Mphi-${MassPhi_}_Mchi-${MassChi_}_gSM-0p25_gDM-1p0_13TeV-madgraph
    CARDNAME2=${CARDLABEL2}
    DIR2=${CARDLABEL2}
    mkdir -p $DIR2
    cp $CARDDIR/Vector_MonoTop_NLO_madgraph_run_card.dat      $DIR2/${CARDNAME2}_run_card.dat
    cp $CARDDIR/Vector_MonoTop_NLO_madgraph_reweight_card.dat $DIR2/${CARDNAME2}_reweight_card.dat
    cp $CARDDIR/Vector_MonoTop_NLO_madgraph_madspin_card.dat  $DIR2/${CARDNAME2}_madspin_card.dat
    cp $CARDDIR/Vector_MonoTop_NLO_madgraph_extramodels.dat   $DIR2/${CARDNAME2}_extramodels.dat
    sed -e "s/MassPhi/$MassPhi/" -e "s/MassChi/$MassChi/"  $CARDDIR/Vector_MonoTop_NLO_customizecards.dat > $DIR2/${CARDNAME2}_customizecards.dat
    sed "s/_NAME_/$CARDNAME2/" $CARDDIR/Vector_MonoTop_NLO_madgraph_proc_card.dat  > $DIR2/${CARDNAME2}_proc_card.dat
    
done < massgrid.txt
