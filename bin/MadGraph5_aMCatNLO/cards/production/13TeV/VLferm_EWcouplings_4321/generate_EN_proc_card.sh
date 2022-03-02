#!/bin/bash

card_base=${1}

echo "import model VLferm_UFO
generate p p > Le+ Ln QED=2 NP=0, Le+ > b b~ ta+, Ln > t b~ ta- 
add process p p > Le- Ln~ QED=2 NP=0, Le- > b b~ ta-, Ln~ > t~ b ta+
add process p p > Le- Ln~ QED=2 NP=0, Le- > b t~ vt, Ln~ > t~ b ta+ 
add process p p > Le- Ln~ QED=2 NP=0, Le- > b b~ ta-, Ln~ > t~ t vt~ 
add process p p > Le+ Ln QED=2 NP=0, Le+ > b~ t vt~, Ln > t b~ ta- 
add process p p > Le+ Ln QED=2 NP=0, Le+ > b~ b ta+, Ln > t t~ vt
add process p p > Le+ Ln QED=2 NP=0, Le+ > b~ t vt~, Ln > t t~ vt
add process p p > Le- Ln~ QED=2 NP=0, Le- > b t~ vt, Ln~ > t t~ vt~
output ${card_base}" > ${card_base}_proc_card.dat
