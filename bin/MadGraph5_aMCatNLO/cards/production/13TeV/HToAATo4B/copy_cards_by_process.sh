
## Higgs production mode
prodmodes=("SUSY_GluGluH_01J_HToAATo4B_M-"
           "SUSY_GluGluH_1J_HToAATo4B_HT100_M-"
           "SUSY_VBFH_HToAATo4B_M-"
           "SUSY_WH_WToAll_HToAATo4B_M-"
           "SUSY_ZH_ZToAll_HToAATo4B_M-"
           "SUSY_TTH_TTToAll_HToAATo4B_M-")

## "a" boson mass points
masspoints=(12 15 20 25 30 35 40 45 50 55 60)

## Loop over all Higgs production modes
for prod in "${prodmodes[@]}"
do
    ## Loop over all "a" boson mass points
    for mp in "${masspoints[@]}"
    do

	prodmp="${prod}${mp}"
	echo ""
	echo "Creating cards for ${prodmp}"

	rm -r "${prodmp}"
	mkdir -p "${prodmp}"
	cp "${prod}MASS/${prod}MASS_run_card.dat"       "${prodmp}/${prodmp}_run_card.dat" 
	cp "${prod}MASS/${prod}MASS_proc_card.dat"      "${prodmp}/${prodmp}_proc_card.dat" 
	cp "${prod}MASS/${prod}MASS_extramodels.dat"    "${prodmp}/${prodmp}_extramodels.dat"
	cp "${prod}MASS/${prod}MASS_customizecards.dat" "${prodmp}/${prodmp}_customizecards.dat" 
    
	cd ${prodmp}

	## Set "a" boson mass
	sed -i "s/MASS/${mp}.0/g" "${prodmp}_customizecards.dat"
	sed -i "s/MASS/${mp}/g"   "${prodmp}_proc_card.dat"

	cd ../
	
	echo "The cards are created for ${prodmp}"
    done
done
