#!/bin/bash

vll_mass=${1}
card_base=${2}

echo "../../../../cards/production/13TeV/VLferm_EWcouplings_4321/param_card.dat
set param_card mln ${vll_mass}
set param_card mle ${vll_mass}
update dependent
set decay wLn Auto
set decay wLe Auto
update dependent" > ${card_base}_customizecards.dat
