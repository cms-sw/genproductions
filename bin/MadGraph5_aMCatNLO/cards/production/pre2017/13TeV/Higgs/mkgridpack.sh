#!/bin/bash

###################################
##### SYNTAX TO MAKE GRIDPACK #####
###################################
## for the model use: HAHM_variablesw_v3
## for the folder_path, point to the directory holding the cards
## e.g. 
## ./mkgridpack.sh HAHM_variablesw_v3 HAHMcards_MZD20_lhaid263000/ 

model=${1}
folder_path=${2}

./gridpack_generation.sh ${model} ${folder_path} 
