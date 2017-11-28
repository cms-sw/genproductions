#!/bin/bash

#############################################################################
#                                                                          ##
#                    MadGraph/MadEvent                                     ##
#                                                                          ##
# FILE : run.sh                                                            ##
# VERSION : 1.0                                                            ##
# DATE : 29 January 2008                                                   ##
# AUTHOR : Michel Herquet (UCL-CP3)                                        ##
#                                                                          ##
# DESCRIPTION : script to save command line param in a grid card and       ##
#   call gridrun                                                           ##
# USAGE : run [num_events] [iseed]                                         ##
#############################################################################

if [[ ! -d ./madevent ]]; then
        echo "Error: no madevent directory found !"
        exit
fi

# For Linux
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${PWD}/madevent/lib:${PWD}/HELAS/lib
# For Mac OS X
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${PWD}/madevent/lib:${PWD}/HELAS/lib

card=./madevent/Cards/grid_card.dat


if [[  ($1 != "") && ("$2" != "") && ("$3" == "") ]]; then
   num_events=$1
   seed=$2
   echo "Updating grid_card.dat..."
   sed -i.bak "s/\s*\d*.*gevents/  $num_events = gevents/g" $card
   sed -i.bak "s/\s*\d*.*gseed/  $seed = gseed/g" $card
   gran=`awk '/^[^#].*=.*ngran/{print $1}' $card`
elif [[  ($1 != "") && ("$2" != "") && ("$3" != "") ]]; then
   num_events=$1
   seed=$2
   gran=$3
   echo "Updating grid_card.dat..."
   sed -i.bak "s/\s*\d*.*gevents/  $num_events = gevents/g" $card
   sed -i.bak "s/\s*\d*.*gseed/  $seed = gseed/g" $card
   sed -i.bak "s/\s*\d*.*ngran/  $gran = ngran/g" $card
else
   echo "Warning: input is not correct, using values from the grid_card.dat."
   if [[ ! -e $card ]]; then
        echo "Error: $card not found !"
        exit
   fi
   num_events=`awk '/^[^#].*=.*gevents/{print $1}' $card`
   seed=`awk '/^[^#].*=.*gseed/{print $1}' $card`
   gran=`awk '/^[^#].*=.*ngran/{print $1}' $card`
fi


echo "Now generating $num_events events with random seed $seed and granularity $gran"


if [[ ! -x ./madevent/bin/gridrun ]]; then
    echo "Error: gridrun script not found !"
    exit
else
    cd ./madevent
    ./bin/gridrun $num_events $seed
fi

if [[ -e ./Events/GridRun_${seed}/unweighted_events.lhe.gz ]]; then
	gunzip ./Events/GridRun_${seed}/unweighted_events.lhe.gz
fi

if [[ ! -e  ./Events/GridRun_${seed}/unweighted_events.lhe ]]; then
    echo "Error: event file not found !"
    exit
else
    echo "Moving events from  events.lhe"
    mv ./Events/GridRun_${seed}/unweighted_events.lhe ../events.lhe
    cd ..
fi

gzip -f events.lhe
exit
