
## Higgs production mode
prodmodes=(#"SUSY_GluGluH_01J_HToAATo4B_M-"
           "SUSY_GluGluH_1J_HToAATo4B_HT100_M-")
           #"SUSY_VBFH_HToAATo4B_M-"
           #"SUSY_WH_WToAll_HToAATo4B_M-"
           #"SUSY_ZH_ZToAll_HToAATo4B_M-"
           #"SUSY_TTH_TTToAll_HToAATo4B_M-")

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
	cp "TEMPLATE_run_card.dat"       "${prodmp}/${prodmp}_run_card.dat" 
	cp "TEMPLATE_proc_card.dat"      "${prodmp}/${prodmp}_proc_card.dat" 
	cp "TEMPLATE_extramodels.dat"    "${prodmp}/${prodmp}_extramodels.dat"
	cp "TEMPLATE_customizecards.dat" "${prodmp}/${prodmp}_customizecards.dat" 
    
	cd ${prodmp}

	## Add HT > 100 GeV cut for ggH+1j
	if [[ "${prodmp}" == *"HT100"* ]]; then
	    sed -i "s/MINHT/ 100 /g" "${prodmp}_run_card.dat"
	else
	    sed -i "s/MINHT/ 0   /g" "${prodmp}_run_card.dat"
	fi
	
	## Set process (or processes) for different production modes
	sed -i "s/TEMPLATE/${prodmp}/g"   "${prodmp}_proc_card.dat"

	if [[ "${prodmp}" == *"GluGluH_01J"* ]]; then
	    sed -i "s/PROCESSA/generate p p  > h01 \$\$ w+ w- z , (h01 > a01 a01, a01 > b b~, a01 > b b~)/g"   "${prodmp}_proc_card.dat"
	    sed -i "s/PROCESSB/generate p p  > h01 j \$\$ w+ w- z , (h01 > a01 a01, a01 > b b~, a01 > b b~)/g" "${prodmp}_proc_card.dat"
	fi
	if [[ "${prodmp}" == *"GluGluH_1J"* ]]; then
	    sed -i "s/PROCESSA/generate p p  > h01 j \$\$ w+ w- z , (h01 > a01 a01, a01 > b b~, a01 > b b~)/g" "${prodmp}_proc_card.dat"
	    sed -i "s/PROCESSB//g" "${prodmp}_proc_card.dat"
	fi
	if [[ "${prodmp}" == *"VBFH"* ]]; then
	    sed -i "s/PROCESSA/#the \$\$ syntax is needed to exclude VH associated production with hadronic W\/Z decays\n/g" "${prodmp}_proc_card.dat"
	    sed -i "s/PROCESSB/generate p p > h01 j j \$\$ w+ w- z, (h01 > a01 a01, a01 > b b~, a01 > b b~)/g" "${prodmp}_proc_card.dat"
	fi
	if [[ "${prodmp}" == *"WH"* ]]; then
	    sed -i "s/PROCESSA/generate p p  > w+ h01 , (h01 > a01 a01, a01 > b b~, a01 > b b~)/g" "${prodmp}_proc_card.dat"
	    sed -i "s/PROCESSB/generate p p  > w- h01 , (h01 > a01 a01, a01 > b b~, a01 > b b~)/g" "${prodmp}_proc_card.dat"
	fi
	if [[ "${prodmp}" == *"ZH"* ]]; then
	    sed -i "s/PROCESSA/generate p p  > Z h01 , (h01 > a01 a01, a01 > b b~, a01 > b b~)/g" "${prodmp}_proc_card.dat"
	    sed -i "s/PROCESSB//g" "${prodmp}_proc_card.dat"
	fi
	if [[ "${prodmp}" == *"TTH"* ]]; then
	    sed -i "s/PROCESSA/generate p p  > t t~ h01 , (h01 > a01 a01, a01 > b b~, a01 > b b~)/g" "${prodmp}_proc_card.dat"
	    sed -i "s/PROCESSB//g" "${prodmp}_proc_card.dat"
	fi
	
	## Set H --> aa model
	if [[ "${prodmp}" == *"GluGluH"* ]]; then
	    sed -i "s/BSMMODEL/NMSSMHET/g" "${prodmp}_proc_card.dat"
	    sed -i "s/MODELCOMMENT/#NMSSM higgs to aa from Yiming Zhong \"Exotic Decays of the 125 GeV Higgs Boson\" http:\/\/exotichiggs.physics.sunysb.edu\/web\/wopr\//g" "${prodmp}_extramodels.dat"
	    sed -i "s/MODELSOURCE/NMSSMHET.zip/g" "${prodmp}_extramodels.dat"
	else
	    sed -i "s/BSMMODEL/NMSSM/g" "${prodmp}_proc_card.dat"
	    sed -i "s/MODELCOMMENT/#NMSSM model from http:\/\/feynrules.irmp.ucl.ac.be\/attachment\/wiki\/NMSSM\/p1_ufo.tgz/g" "${prodmp}_extramodels.dat"
	    sed -i "s/MODELSOURCE/NMSSM.tar.gz/g" "${prodmp}_extramodels.dat"
	fi

	## Set "a" boson mass
	sed -i "s/AMASS/${mp}.0/g" "${prodmp}_customizecards.dat"

	cd ../
	
	echo "The cards are created for ${prodmp}"
    done
done
