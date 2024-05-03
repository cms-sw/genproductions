masspoints=(20 30 40 50 60 70)

template_dir="HeavyNeutrino_trilepton_mNltmW_V-0p01_tau_LO/" 

# loop over all masspoints
for mp in "${masspoints[@]}"
do  
    dir="HeavyNeutrino_trilepton_M${mp}_V-0p01_tau_LO/"
    dir="$template_dir$dir"
 
    mkdir -p "$dir"
    cp "${template_dir}HeavyNeutrino_trilepton_V-0p01_tau_LO_extramodels.dat" "${dir}HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_LO_extramodels.dat"
    cp "${template_dir}HeavyNeutrino_trilepton_V-0p01_tau_LO_proc_card.dat" "${dir}HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_LO_proc_card.dat" 
    cp "${template_dir}HeavyNeutrino_trilepton_V-0p01_tau_LO_run_card.dat" "${dir}HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_LO_run_card.dat" 
    cp "${template_dir}HeavyNeutrino_trilepton_V-0p01_tau_LO_customizecards.dat" "${dir}HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_LO_customizecards.dat" 
    
    cd ${dir}
    sed -i "s/HNLMASS/${mp}/g" "HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_LO_proc_card.dat"
    sed -i "s/HNLMASS/${mp}/g" "HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_LO_customizecards.dat"
    cd ../../
    echo the cards are created for $dir 
done

