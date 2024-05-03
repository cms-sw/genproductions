#!/bin/bash


vll_mass=${1}
card_base=${2}

echo "
set param_card mvlq 3500
set param_card mQu 2500
set param_card mQd 2500

set param_card kappaRu33 0
set param_card kappaRd33 0

set param_card betaL33 0.9
set param_card betaRd33  0

set param_card betaLQL 0.9
set param_card betaRQL 0.9
set param_card betaL3L -0.5
set param_card betaLQ3 0.5

set param_card zetaRu33 0
set param_card zetaRd33 0
set param_card zetaRe33 0


set param_card mln ${vll_mass}
set param_card mle ${vll_mass}

set decay wZp Auto
set decay wVlq Auto

set decay wQu 0
set decay wQd 0

set decay wLn Auto
set decay wLe Auto
update dependent" > ${card_base}_customizecards.dat
