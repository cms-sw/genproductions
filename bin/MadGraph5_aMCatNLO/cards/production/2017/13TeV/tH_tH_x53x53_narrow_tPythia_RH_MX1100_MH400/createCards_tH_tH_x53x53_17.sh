# script to create production cards for 2016 bstar from example card (bstar_tW_Left_M700)

masspoints=(500 600 700 800 900 1000 1100 1200 1500)
masspointsH=( 200 400 600 800 1000 )  
couplings=( "LH" "RH" )
base_dir="2017/13TeV"
template_dir="${base_dir}/tH_tH_x53x53_narrow_tPythia_RH_MX1100_MH400/"

for handedness in "${couplings[@]}"
do
# loop over all masspoints
    for mp in "${masspoints[@]}"
    do
        for mpH in "${masspointsH[@]}"
        do
            if [[ $(($mp - $mpH - 173))  -gt 0 ]] 
            then
    	        dir="${base_dir}/tH_tH_x53x53_narrow_tPythia_${handedness}_MX${mp}_MH${mpH}/"
    	        mkdir -p "$dir"
    	        cp "${template_dir}tH_tH_x53x53_narrow_tPythia_RH_MX1100_MH400_extramodels.dat" "${dir}tH_tH_x53x53_narrow_tPythia_${handedness}_MX${mp}_MH${mpH}_extramodels.dat"
    	        cp "${template_dir}tH_tH_x53x53_narrow_tPythia_RH_MX1100_MH400_proc_card.dat" "${dir}tH_tH_x53x53_narrow_tPythia_${handedness}_MX${mp}_MH${mpH}_proc_card.dat"
    	        sed -i "s/_RH_MX1100_MH400/_${handedness}_MX${mp}_MH${mpH}/g" "${dir}tH_tH_x53x53_narrow_tPythia_${handedness}_MX${mp}_MH${mpH}_proc_card.dat"
    	        cp "${template_dir}tH_tH_x53x53_narrow_tPythia_RH_MX1100_MH400_run_card.dat" "${dir}tH_tH_x53x53_narrow_tPythia_${handedness}_MX${mp}_MH${mpH}_run_card.dat"
    	        root -l -b -q "writeCards.C(${mp},${mpH},\"${handedness}\",\"${dir}\")"
            fi
        done
    done
done
