#!/bin/bash                                                                                                                                                                                                        
# Name of base model
MODEL=DH_4f_LO

# Dark Higgs Mass
DHMASS=70

# based on path from script to cards dir: addons/cards/

for ZPRIMEMASS in 500 1000 1500 2000 2500 3000 ; do
    for DMMASS in 100 200 300 400 500 600 ; do
	SETUP=MZP-${MZPRIME}_MCHI-${MCHI}
	rm -rf addons/cards/${SETUP} 

	# prepare card files 
	mkdir -p addons/cards/${SETUP}
        # copy and rename base cards
	for CARD in run_card proc_card customizecards extramodels ; do 
	    cp -rp addons/cards/${MODEL}/${MODEL}_${CARD}.dat  addons/cards/${SETUP}/${SETUP}_${CARD}.dat
	done 
        # modify cards to match specifications
	sed -i -e "s|ZPRIMEMASS|${ZPRIMEMASS}|g" addons/cards/${SETUP}/${SETUP}_*.dat 
        sed -i -e "s|DHMASS|${DHMASS}|g" addons/cards/${SETUP}/${SETUP}_*.dat
        sed -i -e "s|DMMASS|${DMMASS}|g" addons/cards/${SETUP}/${SETUP}_*.dat
        sed -i -e "s|${MODEL}|${SETUP}|g" addons/cards/${SETUP}/${SETUP}_*.dat

