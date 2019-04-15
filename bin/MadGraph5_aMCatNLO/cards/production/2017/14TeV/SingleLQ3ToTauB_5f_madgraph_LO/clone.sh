# s-channel
# create Madgraph cards, based on M200 GeV one
# 

for mass in 500 1000 1500 2000 2500 
do

    echo "generating ... " ${mass}

    cp -r SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M200 SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M${mass}

    for card in extramodels param_card proc_card run_card
    do
	mv SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M200_${card}.dat SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M${mass}_${card}.dat
    done


    sed -i -e "s/SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M200/SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M"${mass}"/" SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M${mass}_proc_card.dat
    sed -i -e "s/9000006 2.000000e+02 # mr23/9000006 "${mass}" # mr23/" SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_s-channel-M${mass}_param_card.dat

done



# t-channel
# create Madgraph cards, based on M200 GeV one
# 

for mass in 500 1000 1500 2000 2500 
do

    echo "generating ... " ${mass}

    cp -r SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M200 SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M${mass}

    for card in extramodels param_card proc_card run_card
    do
	mv SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M200_${card}.dat SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M${mass}_${card}.dat
    done


    sed -i -e "s/SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M200/SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M"${mass}"/" SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M${mass}_proc_card.dat
    sed -i -e "s/9000006 2.000000e+02 # mr23/9000006 "${mass}" # mr23/" SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_t-channel-M${mass}_param_card.dat

done


# pair prod.
# create Madgraph cards, based on M200 GeV one
# 

for mass in 500 1000 1500 2000 2500 
do

    echo "generating ... " ${mass}

    cp -r SingleLQ3ToTauB_5f_madgraph_LO_pair-M200 SingleLQ3ToTauB_5f_madgraph_LO_pair-M${mass}

    for card in extramodels param_card proc_card run_card
    do
	mv SingleLQ3ToTauB_5f_madgraph_LO_pair-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_pair-M200_${card}.dat SingleLQ3ToTauB_5f_madgraph_LO_pair-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_pair-M${mass}_${card}.dat
    done


    sed -i -e "s/SingleLQ3ToTauB_5f_madgraph_LO_pair-M200/SingleLQ3ToTauB_5f_madgraph_LO_pair-M"${mass}"/" SingleLQ3ToTauB_5f_madgraph_LO_pair-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_pair-M${mass}_proc_card.dat
    sed -i -e "s/9000006 2.000000e+02 # mr23/9000006 "${mass}" # mr23/" SingleLQ3ToTauB_5f_madgraph_LO_pair-M${mass}/SingleLQ3ToTauB_5f_madgraph_LO_pair-M${mass}_param_card.dat

done

