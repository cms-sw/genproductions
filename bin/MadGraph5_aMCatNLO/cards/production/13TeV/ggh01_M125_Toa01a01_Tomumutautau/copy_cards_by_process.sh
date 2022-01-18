
## Higgs production mode
prodmodes=("ggh01_M125_Toa01a01_M")


## "a" boson mass points
masspoints=(20 25 30 35 40 45 50 55 60)

## Loop over all Higgs production modes
for prod in "${prodmodes[@]}"
do
    ## Loop over all "a" boson mass points
    for mp in "${masspoints[@]}"
    do

	prodmp="${prod}${mp}_Tomumutautau"
	echo ""
	echo "Creating cards for ${prodmp}"

	rm -r "${prodmp}"
	mkdir -p "${prodmp}"
	cp "${prod}15_Tomumutautau/${prod}15_Tomumutautau_run_card.dat"       "${prodmp}/${prodmp}_run_card.dat" 
	cp "${prod}15_Tomumutautau/${prod}15_Tomumutautau_proc_card.dat"      "${prodmp}/${prodmp}_proc_card.dat" 
	cp "${prod}15_Tomumutautau/${prod}15_Tomumutautau_proc_card_mg5.dat"      "${prodmp}/${prodmp}_proc_card_mg5.dat" 
	cp "${prod}15_Tomumutautau/${prod}15_Tomumutautau_extramodels.dat"    "${prodmp}/${prodmp}_extramodels.dat"
	cp "${prod}15_Tomumutautau/${prod}15_Tomumutautau_param_card.dat"      "${prodmp}/${prodmp}_param_card.dat" 
    
	cd ${prodmp}

	## Set "a" boson mass
    sed -i "s/36 1.500000e+01/36 ${mp}/g" "${prodmp}_param_card.dat"
	sed -i "s/ggh01_M125_Toa01a01_M15_Tomumutautau/ggh01_M125_Toa01a01_M${mp}_Tomumutautau/g"   "${prodmp}_proc_card.dat"
	sed -i "s/ggh01_M125_Toa01a01_M15_Tomumutautau/ggh01_M125_Toa01a01_M${mp}_Tomumutautau/g"   "${prodmp}_proc_card_mg5.dat"

	cd ../
	
	echo "The cards are created for ${prodmp}"
    done
done
