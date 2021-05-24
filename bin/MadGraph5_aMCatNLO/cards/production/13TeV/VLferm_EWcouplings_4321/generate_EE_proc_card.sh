#!/bin/bash

card_base=${1}

echo "import model VLferm_UFO
generate p p > Le+ Le- QED=2 NP=0, Le- > vt b t~, Le+ > vt~ b~ t
add process p p > Le+ Le- QED=2 NP=0, Le- > ta- b b~, Le+ > vt~ b~ t
add process p p > Le+ Le- QED=2 NP=0, Le- > vt b t~, Le+ > ta+ b~ b
add process p p > Le+ Le- QED=2 NP=0, Le- > ta- b b~, Le+ > ta+ b~ b
output ${card_base}" > ${card_base}_proc_card.dat
