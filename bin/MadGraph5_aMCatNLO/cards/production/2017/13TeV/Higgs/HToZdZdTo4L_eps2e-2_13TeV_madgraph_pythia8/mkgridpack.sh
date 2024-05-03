#!/bin/bash

### SYNTAX: ./mkgridpack.sh HAHM_variablesw_v3 <dir_holding_MG_cards>/ 

model=${1}
folder_path=${2}

./gridpack_generation.sh ${model} ${folder_path} 
