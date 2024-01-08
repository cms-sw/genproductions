#!/bin/bash

default=$(basename $(pwd))
is=(360  400  400  500  500  500  500  600  600  600  600  600  700  700  700  700  700  700  800  800  800  800  800  800  1000  1000  1000  1000  1000  1000  1000  1500  1500  1500  1500  1500  1500  1500  1500  2000  2000  2000  2000  2000  2000  2000  2000  2000  2500  2500  2500  2500  2500  2500  2500  2500  2500  2500  3000  3000  3000  3000  3000  3000  3000  3000  3000  3000 )
js=(260 260 300 260 300 350 400 260 300 350 400 500 260 300 350 400 500 600 260 300 350 400 500 600 260 300 350 400 500 600 800 260 300 350 400 500 600 800 1000 260 300 350 400 500 600 800 1000 1500 260 300 350 400 500 600 800 1000 1500 2000 260 300 350 400 500 600 800 1000 1500 2000)
for idx in "${!is[@]}"; do
   mA=${is[$idx]}
   mH=${js[$idx]}
   ks=0.2
   echo $wA
    echo "Copying mass (mA,mH)" # ($mA,$mH)
    newdir="$default"_mA"$mA"_mH"$mH"
    mkdir $newdir
    cp "$default"_customizecards.dat $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
    cp "$default"_extramodels.dat $newdir/"$default"_mA"$mA"_mH"$mH"_extramodels.dat
    cp "$default"_madspin_card.dat $newdir/"$default"_mA"$mA"_mH"$mH"_madspin_card.dat
    cp "$default"_proc_card.dat $newdir/"$default"_mA"$mA"_mH"$mH"_proc_card.dat
    cp "$default"_run_card.dat $newdir/"$default"_mA"$mA"_mH"$mH"_run_card.dat
    # modify output name
    sed -i 's/'$default'/'$default'_mA'$mA'_mH'$mH'/g' $newdir/"$default"_mA"$mA"_mH"$mH"_proc_card.dat
    # Modify mass parameter
    sed -i 's/AMASS/'$mA'.0/g' $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
    sed -i 's/HMASS/'$mH'.0/g' $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
    # sed -i 's/HWIDTH/'$(bc <<< "$ks * $mH")'/g' $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
    # sed -i 's/AWIDTH/'$(bc <<< "$ks * $mA")'/g' $newdir/"$default"_mA"$mA"_mH"$mH"_customizecards.dat
done

 sed -i 's/lambda2/-1.00442/g' AZHTo4b_NW_mA360_mH260/AZHTo4b_NW_mA360_mH260_customizecards.dat
 sed -i 's/lambda3/1.31454/g' AZHTo4b_NW_mA360_mH260/AZHTo4b_NW_mA360_mH260_customizecards.dat

 sed -i 's/lambda2/-1.50586/g' AZHTo4b_NW_mA400_mH260/AZHTo4b_NW_mA400_mH260_customizecards.dat
 sed -i 's/lambda3/1.81599/g' AZHTo4b_NW_mA400_mH260/AZHTo4b_NW_mA400_mH260_customizecards.dat

 sed -i 's/lambda2/-1.23958/g' AZHTo4b_NW_mA400_mH300/AZHTo4b_NW_mA400_mH300_customizecards.dat
 sed -i 's/lambda3/1.46121/g' AZHTo4b_NW_mA400_mH300/AZHTo4b_NW_mA400_mH300_customizecards.dat

 sed -i 's/lambda2/-2.99041/g' AZHTo4b_NW_mA500_mH260/AZHTo4b_NW_mA500_mH260_customizecards.dat
 sed -i 's/lambda3/3.30054/g' AZHTo4b_NW_mA500_mH260/AZHTo4b_NW_mA500_mH260_customizecards.dat

 sed -i 's/lambda2/-2.72413/g' AZHTo4b_NW_mA500_mH300/AZHTo4b_NW_mA500_mH300_customizecards.dat
 sed -i 's/lambda3/2.94575/g' AZHTo4b_NW_mA500_mH300/AZHTo4b_NW_mA500_mH300_customizecards.dat

 sed -i 's/lambda2/-2.33778/g' AZHTo4b_NW_mA500_mH350/AZHTo4b_NW_mA500_mH350_customizecards.dat
 sed -i 's/lambda3/2.43100/g' AZHTo4b_NW_mA500_mH350/AZHTo4b_NW_mA500_mH350_customizecards.dat

 sed -i 's/lambda2/-1.89199/g' AZHTo4b_NW_mA500_mH400/AZHTo4b_NW_mA500_mH400_customizecards.dat
 sed -i 's/lambda3/1.83706/g' AZHTo4b_NW_mA500_mH400/AZHTo4b_NW_mA500_mH400_customizecards.dat

 sed -i 's/lambda2/-4.80486/g' AZHTo4b_NW_mA600_mH260/AZHTo4b_NW_mA600_mH260_customizecards.dat
 sed -i 's/lambda3/5.11498/g' AZHTo4b_NW_mA600_mH260/AZHTo4b_NW_mA600_mH260_customizecards.dat

 sed -i 's/lambda2/-4.53857/g' AZHTo4b_NW_mA600_mH300/AZHTo4b_NW_mA600_mH300_customizecards.dat
 sed -i 's/lambda3/4.76020/g' AZHTo4b_NW_mA600_mH300/AZHTo4b_NW_mA600_mH300_customizecards.dat

 sed -i 's/lambda2/-4.15222/g' AZHTo4b_NW_mA600_mH350/AZHTo4b_NW_mA600_mH350_customizecards.dat
 sed -i 's/lambda3/4.24545/g' AZHTo4b_NW_mA600_mH350/AZHTo4b_NW_mA600_mH350_customizecards.dat

 sed -i 's/lambda2/-3.70643/g' AZHTo4b_NW_mA600_mH400/AZHTo4b_NW_mA600_mH400_customizecards.dat
 sed -i 's/lambda3/3.65151/g' AZHTo4b_NW_mA600_mH400/AZHTo4b_NW_mA600_mH400_customizecards.dat

 sed -i 's/lambda2/-2.63654/g' AZHTo4b_NW_mA600_mH500/AZHTo4b_NW_mA600_mH500_customizecards.dat
 sed -i 's/lambda3/2.22605/g' AZHTo4b_NW_mA600_mH500/AZHTo4b_NW_mA600_mH500_customizecards.dat

 sed -i 's/lambda2/-6.94920/g' AZHTo4b_NW_mA700_mH260/AZHTo4b_NW_mA700_mH260_customizecards.dat
 sed -i 's/lambda3/7.25933/g' AZHTo4b_NW_mA700_mH260/AZHTo4b_NW_mA700_mH260_customizecards.dat

 sed -i 's/lambda2/-6.68292/g' AZHTo4b_NW_mA700_mH300/AZHTo4b_NW_mA700_mH300_customizecards.dat
 sed -i 's/lambda3/6.90455/g' AZHTo4b_NW_mA700_mH300/AZHTo4b_NW_mA700_mH300_customizecards.dat

 sed -i 's/lambda2/-6.29657/g' AZHTo4b_NW_mA700_mH350/AZHTo4b_NW_mA700_mH350_customizecards.dat
 sed -i 's/lambda3/6.38980/g' AZHTo4b_NW_mA700_mH350/AZHTo4b_NW_mA700_mH350_customizecards.dat

 sed -i 's/lambda2/-5.85078/g' AZHTo4b_NW_mA700_mH400/AZHTo4b_NW_mA700_mH400_customizecards.dat
 sed -i 's/lambda3/5.79585/g' AZHTo4b_NW_mA700_mH400/AZHTo4b_NW_mA700_mH400_customizecards.dat

 sed -i 's/lambda2/-4.78088/g' AZHTo4b_NW_mA700_mH500/AZHTo4b_NW_mA700_mH500_customizecards.dat
 sed -i 's/lambda3/4.37039/g' AZHTo4b_NW_mA700_mH500/AZHTo4b_NW_mA700_mH500_customizecards.dat

 sed -i 's/lambda2/-3.47324/g' AZHTo4b_NW_mA700_mH600/AZHTo4b_NW_mA700_mH600_customizecards.dat
 sed -i 's/lambda3/2.62816/g' AZHTo4b_NW_mA700_mH600/AZHTo4b_NW_mA700_mH600_customizecards.dat

 sed -i 's/lambda2/-9.42345/g' AZHTo4b_NW_mA800_mH260/AZHTo4b_NW_mA800_mH260_customizecards.dat
 sed -i 's/lambda3/9.73357/g' AZHTo4b_NW_mA800_mH260/AZHTo4b_NW_mA800_mH260_customizecards.dat

 sed -i 's/lambda2/-9.15716/g' AZHTo4b_NW_mA800_mH300/AZHTo4b_NW_mA800_mH300_customizecards.dat
 sed -i 's/lambda3/9.37879/g' AZHTo4b_NW_mA800_mH300/AZHTo4b_NW_mA800_mH300_customizecards.dat

 sed -i 's/lambda2/-8.77081/g' AZHTo4b_NW_mA800_mH350/AZHTo4b_NW_mA800_mH350_customizecards.dat
 sed -i 's/lambda3/8.86404/g' AZHTo4b_NW_mA800_mH350/AZHTo4b_NW_mA800_mH350_customizecards.dat

 sed -i 's/lambda2/-8.32502/g' AZHTo4b_NW_mA800_mH400/AZHTo4b_NW_mA800_mH400_customizecards.dat
 sed -i 's/lambda3/8.27010/g' AZHTo4b_NW_mA800_mH400/AZHTo4b_NW_mA800_mH400_customizecards.dat

 sed -i 's/lambda2/-7.25513/g' AZHTo4b_NW_mA800_mH500/AZHTo4b_NW_mA800_mH500_customizecards.dat
 sed -i 's/lambda3/6.84464/g' AZHTo4b_NW_mA800_mH500/AZHTo4b_NW_mA800_mH500_customizecards.dat

 sed -i 's/lambda2/-5.94748/g' AZHTo4b_NW_mA800_mH600/AZHTo4b_NW_mA800_mH600_customizecards.dat
 sed -i 's/lambda3/5.10241/g' AZHTo4b_NW_mA800_mH600/AZHTo4b_NW_mA800_mH600_customizecards.dat

 sed -i 's/lambda2/-15.36163/g' AZHTo4b_NW_mA1000_mH260/AZHTo4b_NW_mA1000_mH260_customizecards.dat
 sed -i 's/lambda3/15.67176/g' AZHTo4b_NW_mA1000_mH260/AZHTo4b_NW_mA1000_mH260_customizecards.dat

 sed -i 's/lambda2/-15.09535/g' AZHTo4b_NW_mA1000_mH300/AZHTo4b_NW_mA1000_mH300_customizecards.dat
 sed -i 's/lambda3/15.31698/g' AZHTo4b_NW_mA1000_mH300/AZHTo4b_NW_mA1000_mH300_customizecards.dat

 sed -i 's/lambda2/-14.70900/g' AZHTo4b_NW_mA1000_mH350/AZHTo4b_NW_mA1000_mH350_customizecards.dat
 sed -i 's/lambda3/14.80223/g' AZHTo4b_NW_mA1000_mH350/AZHTo4b_NW_mA1000_mH350_customizecards.dat

 sed -i 's/lambda2/-14.26321/g' AZHTo4b_NW_mA1000_mH400/AZHTo4b_NW_mA1000_mH400_customizecards.dat
 sed -i 's/lambda3/14.20828/g' AZHTo4b_NW_mA1000_mH400/AZHTo4b_NW_mA1000_mH400_customizecards.dat

 sed -i 's/lambda2/-13.19332/g' AZHTo4b_NW_mA1000_mH500/AZHTo4b_NW_mA1000_mH500_customizecards.dat
 sed -i 's/lambda3/12.78282/g' AZHTo4b_NW_mA1000_mH500/AZHTo4b_NW_mA1000_mH500_customizecards.dat

 sed -i 's/lambda2/-11.88567/g' AZHTo4b_NW_mA1000_mH600/AZHTo4b_NW_mA1000_mH600_customizecards.dat
 sed -i 's/lambda3/11.04059/g' AZHTo4b_NW_mA1000_mH600/AZHTo4b_NW_mA1000_mH600_customizecards.dat

 sed -i 's/lambda2/-8.55711/g' AZHTo4b_NW_mA1000_mH800/AZHTo4b_NW_mA1000_mH800_customizecards.dat
 sed -i 's/lambda3/6.60582/g' AZHTo4b_NW_mA1000_mH800/AZHTo4b_NW_mA1000_mH800_customizecards.dat

 sed -i 's/lambda2/-35.98034/g' AZHTo4b_NW_mA1500_mH260/AZHTo4b_NW_mA1500_mH260_customizecards.dat
 sed -i 's/lambda3/36.29046/g' AZHTo4b_NW_mA1500_mH260/AZHTo4b_NW_mA1500_mH260_customizecards.dat

 sed -i 's/lambda2/-35.71405/g' AZHTo4b_NW_mA1500_mH300/AZHTo4b_NW_mA1500_mH300_customizecards.dat
 sed -i 's/lambda3/35.93568/g' AZHTo4b_NW_mA1500_mH300/AZHTo4b_NW_mA1500_mH300_customizecards.dat

 sed -i 's/lambda2/-35.32770/g' AZHTo4b_NW_mA1500_mH350/AZHTo4b_NW_mA1500_mH350_customizecards.dat
 sed -i 's/lambda3/35.42093/g' AZHTo4b_NW_mA1500_mH350/AZHTo4b_NW_mA1500_mH350_customizecards.dat

 sed -i 's/lambda2/-34.88191/g' AZHTo4b_NW_mA1500_mH400/AZHTo4b_NW_mA1500_mH400_customizecards.dat
 sed -i 's/lambda3/34.82699/g' AZHTo4b_NW_mA1500_mH400/AZHTo4b_NW_mA1500_mH400_customizecards.dat

 sed -i 's/lambda2/-33.81202/g' AZHTo4b_NW_mA1500_mH500/AZHTo4b_NW_mA1500_mH500_customizecards.dat
 sed -i 's/lambda3/33.40153/g' AZHTo4b_NW_mA1500_mH500/AZHTo4b_NW_mA1500_mH500_customizecards.dat

 sed -i 's/lambda2/-32.50437/g' AZHTo4b_NW_mA1500_mH600/AZHTo4b_NW_mA1500_mH600_customizecards.dat
 sed -i 's/lambda3/31.65930/g' AZHTo4b_NW_mA1500_mH600/AZHTo4b_NW_mA1500_mH600_customizecards.dat

 sed -i 's/lambda2/-29.17582/g' AZHTo4b_NW_mA1500_mH800/AZHTo4b_NW_mA1500_mH800_customizecards.dat
 sed -i 's/lambda3/27.22453/g' AZHTo4b_NW_mA1500_mH800/AZHTo4b_NW_mA1500_mH800_customizecards.dat

 sed -i 's/lambda2/-24.89624/g' AZHTo4b_NW_mA1500_mH1000/AZHTo4b_NW_mA1500_mH1000_customizecards.dat
 sed -i 's/lambda3/21.52268/g' AZHTo4b_NW_mA1500_mH1000/AZHTo4b_NW_mA1500_mH1000_customizecards.dat

 sed -i 's/lambda2/-64.84652/g' AZHTo4b_NW_mA2000_mH260/AZHTo4b_NW_mA2000_mH260_customizecards.dat
 sed -i 's/lambda3/65.15665/g' AZHTo4b_NW_mA2000_mH260/AZHTo4b_NW_mA2000_mH260_customizecards.dat

 sed -i 's/lambda2/-64.58024/g' AZHTo4b_NW_mA2000_mH300/AZHTo4b_NW_mA2000_mH300_customizecards.dat
 sed -i 's/lambda3/64.80187/g' AZHTo4b_NW_mA2000_mH300/AZHTo4b_NW_mA2000_mH300_customizecards.dat

 sed -i 's/lambda2/-64.19389/g' AZHTo4b_NW_mA2000_mH350/AZHTo4b_NW_mA2000_mH350_customizecards.dat
 sed -i 's/lambda3/64.28712/g' AZHTo4b_NW_mA2000_mH350/AZHTo4b_NW_mA2000_mH350_customizecards.dat

 sed -i 's/lambda2/-63.74810/g' AZHTo4b_NW_mA2000_mH400/AZHTo4b_NW_mA2000_mH400_customizecards.dat
 sed -i 's/lambda3/63.69317/g' AZHTo4b_NW_mA2000_mH400/AZHTo4b_NW_mA2000_mH400_customizecards.dat

 sed -i 's/lambda2/-62.67820/g' AZHTo4b_NW_mA2000_mH500/AZHTo4b_NW_mA2000_mH500_customizecards.dat
 sed -i 's/lambda3/62.26771/g' AZHTo4b_NW_mA2000_mH500/AZHTo4b_NW_mA2000_mH500_customizecards.dat

 sed -i 's/lambda2/-61.37056/g' AZHTo4b_NW_mA2000_mH600/AZHTo4b_NW_mA2000_mH600_customizecards.dat
 sed -i 's/lambda3/60.52548/g' AZHTo4b_NW_mA2000_mH600/AZHTo4b_NW_mA2000_mH600_customizecards.dat

 sed -i 's/lambda2/-58.04200/g' AZHTo4b_NW_mA2000_mH800/AZHTo4b_NW_mA2000_mH800_customizecards.dat
 sed -i 's/lambda3/56.09071/g' AZHTo4b_NW_mA2000_mH800/AZHTo4b_NW_mA2000_mH800_customizecards.dat

 sed -i 's/lambda2/-53.76243/g' AZHTo4b_NW_mA2000_mH1000/AZHTo4b_NW_mA2000_mH1000_customizecards.dat
 sed -i 's/lambda3/50.38886/g' AZHTo4b_NW_mA2000_mH1000/AZHTo4b_NW_mA2000_mH1000_customizecards.dat

 sed -i 's/lambda2/-38.90281/g' AZHTo4b_NW_mA2000_mH1500/AZHTo4b_NW_mA2000_mH1500_customizecards.dat
 sed -i 's/lambda3/30.59078/g' AZHTo4b_NW_mA2000_mH1500/AZHTo4b_NW_mA2000_mH1500_customizecards.dat

 sed -i 's/lambda2/-101.96019/g' AZHTo4b_NW_mA2500_mH260/AZHTo4b_NW_mA2500_mH260_customizecards.dat
 sed -i 's/lambda3/102.27031/g' AZHTo4b_NW_mA2500_mH260/AZHTo4b_NW_mA2500_mH260_customizecards.dat

 sed -i 's/lambda2/-101.69390/g' AZHTo4b_NW_mA2500_mH300/AZHTo4b_NW_mA2500_mH300_customizecards.dat
 sed -i 's/lambda3/101.91553/g' AZHTo4b_NW_mA2500_mH300/AZHTo4b_NW_mA2500_mH300_customizecards.dat

 sed -i 's/lambda2/-101.30755/g' AZHTo4b_NW_mA2500_mH350/AZHTo4b_NW_mA2500_mH350_customizecards.dat
 sed -i 's/lambda3/101.40078/g' AZHTo4b_NW_mA2500_mH350/AZHTo4b_NW_mA2500_mH350_customizecards.dat

 sed -i 's/lambda2/-100.86176/g' AZHTo4b_NW_mA2500_mH400/AZHTo4b_NW_mA2500_mH400_customizecards.dat
 sed -i 's/lambda3/100.80684/g' AZHTo4b_NW_mA2500_mH400/AZHTo4b_NW_mA2500_mH400_customizecards.dat

 sed -i 's/lambda2/-99.79187/g' AZHTo4b_NW_mA2500_mH500/AZHTo4b_NW_mA2500_mH500_customizecards.dat
 sed -i 's/lambda3/99.38138/g' AZHTo4b_NW_mA2500_mH500/AZHTo4b_NW_mA2500_mH500_customizecards.dat

 sed -i 's/lambda2/-98.48422/g' AZHTo4b_NW_mA2500_mH600/AZHTo4b_NW_mA2500_mH600_customizecards.dat
 sed -i 's/lambda3/97.63915/g' AZHTo4b_NW_mA2500_mH600/AZHTo4b_NW_mA2500_mH600_customizecards.dat

 sed -i 's/lambda2/-95.15567/g' AZHTo4b_NW_mA2500_mH800/AZHTo4b_NW_mA2500_mH800_customizecards.dat
 sed -i 's/lambda3/93.20438/g' AZHTo4b_NW_mA2500_mH800/AZHTo4b_NW_mA2500_mH800_customizecards.dat

 sed -i 's/lambda2/-90.87610/g' AZHTo4b_NW_mA2500_mH1000/AZHTo4b_NW_mA2500_mH1000_customizecards.dat
 sed -i 's/lambda3/87.50253/g' AZHTo4b_NW_mA2500_mH1000/AZHTo4b_NW_mA2500_mH1000_customizecards.dat

 sed -i 's/lambda2/-76.01647/g' AZHTo4b_NW_mA2500_mH1500/AZHTo4b_NW_mA2500_mH1500_customizecards.dat
 sed -i 's/lambda3/67.70445/g' AZHTo4b_NW_mA2500_mH1500/AZHTo4b_NW_mA2500_mH1500_customizecards.dat

 sed -i 's/lambda2/-55.21300/g' AZHTo4b_NW_mA2500_mH2000/AZHTo4b_NW_mA2500_mH2000_customizecards.dat
 sed -i 's/lambda3/39.98714/g' AZHTo4b_NW_mA2500_mH2000/AZHTo4b_NW_mA2500_mH2000_customizecards.dat

 sed -i 's/lambda2/-147.32133/g' AZHTo4b_NW_mA3000_mH260/AZHTo4b_NW_mA3000_mH260_customizecards.dat
 sed -i 's/lambda3/147.63146/g' AZHTo4b_NW_mA3000_mH260/AZHTo4b_NW_mA3000_mH260_customizecards.dat

 sed -i 's/lambda2/-147.05505/g' AZHTo4b_NW_mA3000_mH300/AZHTo4b_NW_mA3000_mH300_customizecards.dat
 sed -i 's/lambda3/147.27668/g' AZHTo4b_NW_mA3000_mH300/AZHTo4b_NW_mA3000_mH300_customizecards.dat

 sed -i 's/lambda2/-146.66870/g' AZHTo4b_NW_mA3000_mH350/AZHTo4b_NW_mA3000_mH350_customizecards.dat
 sed -i 's/lambda3/146.76193/g' AZHTo4b_NW_mA3000_mH350/AZHTo4b_NW_mA3000_mH350_customizecards.dat

 sed -i 's/lambda2/-146.22291/g' AZHTo4b_NW_mA3000_mH400/AZHTo4b_NW_mA3000_mH400_customizecards.dat
 sed -i 's/lambda3/146.16799/g' AZHTo4b_NW_mA3000_mH400/AZHTo4b_NW_mA3000_mH400_customizecards.dat

 sed -i 's/lambda2/-145.15302/g' AZHTo4b_NW_mA3000_mH500/AZHTo4b_NW_mA3000_mH500_customizecards.dat
 sed -i 's/lambda3/144.74252/g' AZHTo4b_NW_mA3000_mH500/AZHTo4b_NW_mA3000_mH500_customizecards.dat

 sed -i 's/lambda2/-143.84537/g' AZHTo4b_NW_mA3000_mH600/AZHTo4b_NW_mA3000_mH600_customizecards.dat
 sed -i 's/lambda3/143.00029/g' AZHTo4b_NW_mA3000_mH600/AZHTo4b_NW_mA3000_mH600_customizecards.dat

 sed -i 's/lambda2/-140.51681/g' AZHTo4b_NW_mA3000_mH800/AZHTo4b_NW_mA3000_mH800_customizecards.dat
 sed -i 's/lambda3/138.56552/g' AZHTo4b_NW_mA3000_mH800/AZHTo4b_NW_mA3000_mH800_customizecards.dat

 sed -i 's/lambda2/-136.23724/g' AZHTo4b_NW_mA3000_mH1000/AZHTo4b_NW_mA3000_mH1000_customizecards.dat
 sed -i 's/lambda3/132.86368/g' AZHTo4b_NW_mA3000_mH1000/AZHTo4b_NW_mA3000_mH1000_customizecards.dat

 sed -i 's/lambda2/-121.37762/g' AZHTo4b_NW_mA3000_mH1500/AZHTo4b_NW_mA3000_mH1500_customizecards.dat
 sed -i 's/lambda3/113.06560/g' AZHTo4b_NW_mA3000_mH1500/AZHTo4b_NW_mA3000_mH1500_customizecards.dat

 sed -i 's/lambda2/-100.57415/g' AZHTo4b_NW_mA3000_mH2000/AZHTo4b_NW_mA3000_mH2000_customizecards.dat
 sed -i 's/lambda3/85.34829/g' AZHTo4b_NW_mA3000_mH2000/AZHTo4b_NW_mA3000_mH2000_customizecards.dat

