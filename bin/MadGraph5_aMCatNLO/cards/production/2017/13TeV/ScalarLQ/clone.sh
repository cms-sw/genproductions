firstMass=600
lastMass=2000

for proc in Pair Single NonRes
#for proc in NonRes
do

    echo "generating proc ... " ${proc}

    for mass in $(seq $firstMass 100 $lastMass) 2500
#    for mass in 3000 5000 7000 10000 15000 20000
    do
	
	echo "     mass = " ${mass}
	
	rm -rf ${proc}_M${mass}
	cp -r ${proc}_M500 ${proc}_M${mass}
	
	for card in extramodels customizecards proc_card run_card
	do
	    mv ${proc}_M${mass}/${proc}_M500_${card}.dat ${proc}_M${mass}/${proc}_M${mass}_${card}.dat
	done
	
	
	sed -i -e "s/MASS 9000006 500/MASS 9000006 "${mass}"/" ${proc}_M${mass}/${proc}_M${mass}_customizecards.dat

	sed -i -e "s/output "${proc}"_M500/output "${proc}"_M"${mass}"/" ${proc}_M${mass}/${proc}_M${mass}_proc_card.dat

    done
done

