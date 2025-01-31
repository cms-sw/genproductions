#!/bin/bash

# Template file
template_card="ZJToMuMu-13p6TeV-suggested-nnpdf31-ncalls-doublefsr-q139-powheg.input"

if [[ ! -f $template_card ]]; then
    echo "Error: Template card '$template_card' not found!"
    exit 1
fi

# Array of mZ bins
mZ_bins=(10 50 100 200 400 600 800 1000 1500 2000 4000 6000 13600)

# Array of decay modes: 2 = muons, 1 = electrons, 3 = taus
decay_modes=("MuMu" "EE" "TauTau")
vdecay_modes=(2 1 3)

# Loop over all decay modes
for ((d=0; d<${#decay_modes[@]}; d++)); do
    # Loop over all bins, excluding the last one for pairing
    for ((i=0; i<${#mZ_bins[@]}-1; i++)); do
        lower=${mZ_bins[i]}
        upper=${mZ_bins[i+1]}

        # Define the output card name
        card_name="ZJTo${decay_modes[d]}-13p6TeV-suggested-nnpdf31-ncalls-doublefsr-q139-m${lower}to${upper}-powheg.input"

        # Replace placeholders in the template and create the new card
        sed -e "s/^vdecaymode .*/vdecaymode  ${vdecay_modes[d]}     ! Z decay products/" \
            -e "s/^min_Z_mass .*/min_Z_mass ${lower}d0  ! Lower mass cut-off for Z boson production/" \
            -e "s/^max_Z_mass .*/max_Z_mass ${upper}d0  ! Upper mass cut-off for Z boson production/" \
            $template_card > $card_name

        echo "Generated $card_name"
    done
done

echo "All cards have been generated!"     