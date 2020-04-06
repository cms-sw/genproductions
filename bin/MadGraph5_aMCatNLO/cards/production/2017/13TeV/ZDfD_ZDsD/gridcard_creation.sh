#!/bin/sh

#  GridCard Script.sh
#  
#
#  Created by Jacob Chesslo on 3/30/20.
#

echo "Starting Gridcard Creation... "

###############################
#Declaring FD1, FD2, ZD Arrays#
###############################

FD1=(1 1 1 1 1 1 1 1 1 10 10 10 10 50 50 50 50 50 150 150 150 150 150 500 500 500 500 1000 1000 1000)
FD2=(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
ZD=(10 20 50 100 200 300 500 1000 2000 10 15 50 100 10 50 95 200 300 10 200 295 500 1000 10 500 995 2000 10 1000 1995)
SD=(1 1 1 1 1 1 1 1 1 10 10 10 10 50 50 50 50 50 150 150 150 150 150 500 500 500 500 1000 1000 1000) #Equal to FD1
naming_convention="ZDfD_ZDsD"
models=("ZD_FD" "ZD_SD")


###########################
#Declaring Directory Names#
###########################

#base_dir="research"
#template_dir="${base_dir}/${naming_convention}"
template_dir=""


#############################
#For Loop for Two (2) Models#
#############################

for model_name in "${models[@]}"
do
    
    ###################################
    #For Loop for Thirty (30) Mass Sets#
    ####################################
    
    for i in {1..30}    #i will refer to ith component of Declared Array
    do
        
        ###########################################
        #Creating Directory for each Paramater Set#
        #Creates and Declares i-th directory names#
        #Creates Name for Files/Dir Moving Forward#
        ###########################################
        
        spec_name="${naming_convention}_${model_name}_${FD1[i]}_${FD2[i]}_${ZD[i]}"
        dir="${spec_name}/"   #Declares directory name
        mkdir -p "$dir"
        
        ##################################
        #param_card.dat Creation, Editing#
        ##################################
        
        cp "${naming_convention}_param_card.dat" "${spec_name}_param_card.dat"
        
        ##############################
        #Edit Lines in param_card.dat#
        ##############################
        
        #Declares XXX_var as the ith array element of array XXX[]
        
        ZD_var=${ZD[i]}
        SD_var=${SD[i]}
        FD1_var=${FD1[i]}
        FD2_var=${FD2[i]}
        
        #Declares XXX_sci as the 6 decimal representation of XXX_var
        
        ZD_sci=$(printf "%.6e" ${ZD_var})
        SD_sci=$(printf "%.6e" ${SD_var})
        FD1_sci=$(printf "%.6e" ${FD1_var})
        FD2_sci=$(printf "%.6e" ${FD2_var})
        
        #Replace(s) Line's (nn) text string (word1) with variable XXX_sci (word2) in specified file (filename.dat)
        #   sed -i "nns/word1/word2/g" "filename.dat"
        
        sed -i '' "72s/1.000000e+01/$ZD_sci/g" "${spec_name}_param_card.dat"
        sed -i '' "74s/1.000000e+01/$SD_sci/g" "${spec_name}_param_card.dat"
        sed -i '' "75s/1.000000e+00/$FD1_sci/g" "${spec_name}_param_card.dat"
        sed -i '' "77s/1.000000e+00/$FD2_sci/g" "${spec_name}_param_card.dat"
        
        
        #################################
        #proc_card.dat Creation, Editing#
        #################################
        
        cp "${naming_convention}_${model_name}_proc_card.dat" "${spec_name}_proc_card.dat"
        
        
        ################################
        #run_card.dat Creation, Editing#
        ################################
        
        cp "${naming_convention}_run_card.dat" "${spec_name}_run_card.dat"
        
        
        ###################################
        #extramodels.dat Creation, Editing#
        ###################################
         
        cp "${naming_convention}_extramodels.dat" "${spec_name}_extramodels.dat"
        
        
        #################################
        #Move Cards to Correct Directory#
        #################################
        
        mv "${spec_name}_param_card.dat" "$dir"
        
        mv "${spec_name}_proc_card.dat" "$dir"
        
        mv "${spec_name}_run_card.dat" "$dir"
        
        mv "${spec_name}_extramodels.dat" "$dir"
        
    done
    
done


echo "Finished with GridCard Creation!"
