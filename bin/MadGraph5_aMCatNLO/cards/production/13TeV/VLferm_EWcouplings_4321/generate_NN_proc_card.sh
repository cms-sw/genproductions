#!/bin/bash

card_base=${1}

echo "import model VLferm_UFO
generate p p > Ln Ln~ QED=2 NP=0, Ln > vt t t~, Ln~ > vt~ t~ t
add process p p > Ln Ln~ QED=2 NP=0, Ln >  t b~ ta-, Ln~ > vt~ t~ t
add process p p > Ln Ln~ QED=2 NP=0, Ln >  t t~ vt, Ln~ >  t~ b ta+ 
add process p p > Ln Ln~ QED=2 NP=0, Ln >  t b~ ta-, Ln~ >  t~ b ta+ 
output ${card_base}" > ${card_base}_proc_card.dat
