#!/bin/env bash
basedir=$PWD
cardsdir=$PWD
cards=($(ls ${cardsdir}| grep ".DAT"))
numberofcards=${#cards[@]}
echo "Temporary work directory for MCFM+JHUGen $PWD for inputcard ${cards[@]}" | mail -s "MCFM+JHUGen workdir" "carolhungwt@gmail.com"
cd ${basedir}
for card in ${cards[@]}; do

	./run_mcfm.py -i ${card} -n 1 -s 123456 
done
#bsub -q 1nw monitor_script.sh
