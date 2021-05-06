masspoints=(100 125 150 175 200 250 300 350 400 450 500 600 700 800 900 1000)

template_dir="HeavyNeutrino_trilepton_mNgtmZ_V-0p01_tau_NLO/" 

# loop over all masspoints
for mp in "${masspoints[@]}"
do  
    dir="HeavyNeutrino_trilepton_M${mp}_V-0p01_tau_NLO/"
    dir="$template_dir$dir"
 
    mkdir -p "$dir"
    cp "${template_dir}HeavyNeutrino_trilepton_V-0p01_tau_NLO_extramodels.dat" "${dir}HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_NLO_extramodels.dat"
    cp "${template_dir}HeavyNeutrino_trilepton_V-0p01_tau_NLO_proc_card.dat" "${dir}HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_NLO_proc_card.dat" 
    cp "${template_dir}HeavyNeutrino_trilepton_V-0p01_tau_NLO_run_card.dat" "${dir}HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_NLO_run_card.dat" 
    cp "${template_dir}HeavyNeutrino_trilepton_V-0p01_tau_NLO_customizecards.dat" "${dir}HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_NLO_customizecards.dat" 
    cp "${template_dir}HeavyNeutrino_trilepton_V-0p01_tau_NLO_madspin_card.dat" "${dir}HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_NLO_madspin_card.dat"
    
    cd ${dir}
    sed -i "s/HNLMASS/${mp}/g" "HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_NLO_proc_card.dat"
    sed -i "s/HNLMASS/${mp}/g" "HeavyNeutrino_trilepton_M-${mp}_V-0p01_tau_NLO_customizecards.dat"
    cd ../../
    echo the cards are created for $dir 
done

