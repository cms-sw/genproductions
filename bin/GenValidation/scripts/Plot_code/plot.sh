#!/bin/bash

##array length is 2 for two version comparison
#xs=(5208. 5208.)
#histo_name=("MGinc_265.root" "MGinc_273.root")
#names=("MGinc_265" "MGinc_273")

#array length is larger than 2 for inclusive vs stitched, e.g.:
xs=(65500.99 60896.39 3528.86 783.44 28.04 3.59 0.55)
histo_name=("WJet_inc_265.root" "WJet_pt0to50_265.root" "WJet_pt50to100_265.root" "WJet_pt100to250_265.root" "WJet_pt250to400_265.root" "WJet_pt400to600_265.root" "WJet_pt600toinf_265.root")
names=("inclusive" "pt0-50" "pt50-100" "pt100-250" "pt250-400" "pt400-600" "pt600-inf")


inDIR="./"
outDIR="./pic"
mkdir -p $outDIR

xs_=""
histo_name_=""
names_=""
for var in ${xs[@]};do
  xs_="$xs_ $var"
done
xs_="--xs ${xs_}"
for var1 in ${histo_name[@]};do
  histo_name_="$histo_name_ $var1"
done
histo_name_="--histo ${histo_name_}"
for var2 in ${names[@]};do
  names_="$names_ $var2"
done
names_="--name ${names_}"

while getopts "m:" opt;do
  case "$opt" in
    m) MODE=$OPTARG
	;;
  esac
done

if [ -z "MODE" ]; then
  #TWOVERSION means comparison between two versions, e.g., MG265 inclusive vs MG273 inclusive
  #INCVSSTIT means comparison between inclusive vs summed stitched
  echo "plot.sh -m <TWOVERSION/INCVSSTIT>" 
fi

case $MODE in
  TWOVERSION)
    echo "two version comparison mode--->>>"
    if [ ${#xs[*]} -ne 2 ] || [ ${#histo_name[*]} -ne 2 ]; then
	echo "ERROR! Only two cross section value and two histograms should be provided!!"
	break
    fi
#    python plot.py --xs ${xs[0]} ${xs[1]} --inputdir $inDIR --histo ${histo_name[0]} ${histo_name[1]} --name ${names[0]} ${names[1]}
    python plot.py ${xs_}  --inputdir $inDIR ${histo_name_} ${names_} --outputdir $outDIR
	;;

  INCVSSTIT)
    echo "inclusive vs stitched mode--->>>"
    if [ ${#xs[*]} -lt 3 ] || [ ${#histo_name[*]} -lt 3 ]; then
	echo "ERROR! Only two cross section value and two histograms is provided, need mode!!!"
	break
    fi
    python plot.py ${xs_} --inputdir $inDIR ${histo_name_} ${names_} --outputdir $outDIR
	;;
esac
