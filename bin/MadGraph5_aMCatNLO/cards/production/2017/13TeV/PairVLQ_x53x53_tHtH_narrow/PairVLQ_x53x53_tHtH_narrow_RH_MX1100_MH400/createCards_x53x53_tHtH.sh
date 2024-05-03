# script to create production cards for 2016 bstar from example card (bstar_tW_Left_M700)

masspoints=(500 600 700 800 900 1000 1100 1200 1500)
masspointsH=( 200 400 600 800 1000 )  
couplings=( "LH" "RH" )
base_dir="2017/13TeV/PairVLQ_x53x53_tHtH_narrow/"
template_dir="2017/13TeV/PairVLQ_x53x53_tHtH_narrow/PairVLQ_x53x53_tHtH_narrow_RH_MX1100_MH400/"

for handedness in "${couplings[@]}"
do
# loop over all masspoints
    for mp in "${masspoints[@]}"
    do
        for mpH in "${masspointsH[@]}"
        do
            if [[ $(($mp - $mpH - 173))  -gt 0 ]] 
            then
    	        dir="${base_dir}/PairVLQ_x53x53_tHtH_narrow_${handedness}_MX${mp}_MH${mpH}/"
    	        mkdir -p "$dir"
    	        cp "${template_dir}PairVLQ_x53x53_tHtH_narrow_RH_MX1100_MH400_extramodels.dat" "${dir}PairVLQ_x53x53_tHtH_narrow_${handedness}_MX${mp}_MH${mpH}_extramodels.dat"
    	        cp "${template_dir}PairVLQ_x53x53_tHtH_narrow_RH_MX1100_MH400_proc_card.dat" "${dir}PairVLQ_x53x53_tHtH_narrow_${handedness}_MX${mp}_MH${mpH}_proc_card.dat"
    	        sed -i "s/_RH_MX1100_MH400/_${handedness}_MX${mp}_MH${mpH}/g" "${dir}PairVLQ_x53x53_tHtH_narrow_${handedness}_MX${mp}_MH${mpH}_proc_card.dat"
    	        cp "${template_dir}PairVLQ_x53x53_tHtH_narrow_RH_MX1100_MH400_run_card.dat" "${dir}PairVLQ_x53x53_tHtH_narrow_${handedness}_MX${mp}_MH${mpH}_run_card.dat"
    	        root -l -b -q "writeCards.C(${mp},${mpH},\"${handedness}\",\"${dir}\")"
            fi
        done
    done
done
