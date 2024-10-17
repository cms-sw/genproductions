#customizecards.dat: modify the charm quark yukawa coupling
cDefault=63400
CHARGES="minus plus"
KAPPA="1 40 80 120"
DECAYS="tt ww gg"
for CHARGE in $CHARGES
do
    for cMultiple in $KAPPA
    do
        cYukawa=$(expr $cMultiple*$cDefault | bc)
        #WH(WW)
        sed -e 's/ctag/'"$cYukawa"'/g' sample_customizecards.dat >trash_ww_custom.dat
        sed -e 's/decay_higgs_tag/'"4.070000e-03"'/g' trash_ww_custom.dat >W${CHARGE}H_HToWW_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_customizecards.dat
        #WH(TT)
        sed -e 's/ctag/'"$cYukawa"'/g' sample_customizecards.dat >trash_tt_custom.dat
        sed -e 's/decay_higgs_tag/'"4.070000e-03"'/g' trash_tt_custom.dat >W${CHARGE}H_HTo2Tau_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_customizecards.dat
        #WH(GG)
        sed -e 's/ctag/'"$cYukawa"'/g' sample_customizecards.dat >trash_gg_custom.dat
        sed -e 's/decay_higgs_tag/'"AUTO"'/g' trash_gg_custom.dat >W${CHARGE}H_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_customizecards.dat
        rm trash_ww_custom.dat trash_tt_custom.dat trash_gg_custom.dat
    done
done
#param_cat.dat for the WH(GG) channel needs additional lines
for CHARGE in $CHARGES
do
for cMultiple in $KAPPA
do
cp sample_param_card.dat W${CHARGE}H_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_param_card.dat
cp sample_param_card.dat W${CHARGE}H_HTo2Tau_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_param_card.dat
cp sample_param_card.dat W${CHARGE}H_HToWW_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_param_card.dat
cat >> W${CHARGE}H_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_param_card.dat << EOL

###################################
## INFORMATION FOR NPBLOCK
###################################
Block npblock
    1 5.000000e-01 # gHaa
    2 1.000000e+03 # LambdaFF
EOL
done 
done
#proc_card.dat: modify the charge of the associated W boson and the model in the WH(GG) channel
for CHARGE in $CHARGES
do
    for cMultiple in $KAPPA
    do
        if [[ $CHARGE == "minus" ]]; then
            sed -e 's/Wtag/'"w-"'/g' sample_proc_card.dat >trash_proc.dat
            sed -e 's/Outtag/'"WminusH_HToWW_TuneCP5_KappaC${cMultiple}"'/g' trash_proc.dat >WminusH_HToWW_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_proc_card.dat
            sed -e 's/Outtag/'"WminusH_HTo2Tau_TuneCP5_KappaC${cMultiple}"'/g' trash_proc.dat >WminusH_HTo2Tau_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_proc_card.dat
            sed -e 's/SM_dusc_NLO_py3/'"SM_dusc_NLO_haa"'/g' trash_proc.dat >trashB_proc.dat
            sed -e 's/Outtag/'"WminusH_HToGG_TuneCP5_KappaC${cMultiple}"'/g' trashB_proc.dat >WminusH_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_proc_card.dat
            rm trash_proc.dat trashB_proc.dat
        fi
        if [[ $CHARGE == "plus" ]]; then
            sed -e 's/Wtag/'"w+"'/g' sample_proc_card.dat >trash_proc.dat
            sed -e 's/Outtag/'"WplusH_HToWW_TuneCP5_KappaC${cMultiple}"'/g' trash_proc.dat >WplusH_HToWW_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_proc_card.dat
            sed -e 's/Outtag/'"WplusH_HTo2Tau_TuneCP5_KappaC${cMultiple}"'/g' trash_proc.dat >WplusH_HTo2Tau_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_proc_card.dat
            sed -e 's/SM_dusc_NLO_py3/'"SM_dusc_NLO_haa"'/g' trash_proc.dat >trashB_proc.dat
            sed -e 's/Outtag/'"WplusH_HToGG_TuneCP5_KappaC${cMultiple}"'/g' trashB_proc.dat >WplusH_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_proc_card.dat
            rm trash_proc.dat trashB_proc.dat
        fi
    done
done
#madspin_card.dat: modify the decay of the associated W boson and the H boson, depending on the H boson decay channel
for DECAY in $DECAYS
do
    for CHARGE in $CHARGES
    do
        for cMultiple in $KAPPA
        do
            if [[ $CHARGE == "minus" ]]; then
                if [[ $DECAY == "tt" ]]; then
                    sed -e 's/Wtag/'"w-"'/g' sample_madspin_card.dat >trashA_madspin.dat
                    sed -e 's/Htag/'"ta+ ta-"'/g' trashA_madspin.dat >trashB_madspin.dat
                    sed -e 's/Wdecaytag/'"ltjets ltjets"'/g' trashB_madspin.dat >WminusH_HTo2Tau_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_madspin_card.dat
                    rm trashA_madspin.dat trashB_madspin.dat
                fi
                if [[ $DECAY == "ww" ]]; then
                    sed -e 's/Wtag/'"w-"'/g' sample_madspin_card.dat >trashA_madspin.dat
                    sed -e 's/Htag/'"w+ w- > ltjets ltjets ltjets ltjets"'/g' trashA_madspin.dat >trashB_madspin.dat
                    sed -e 's/Wdecaytag/'"ltjets ltjets"'/g' trashB_madspin.dat >WminusH_HToWW_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_madspin_card.dat
                    rm trashA_madspin.dat trashB_madspin.dat
                fi
                if [[ $DECAY == "gg" ]]; then
                    sed -e 's/Wtag/'"w-"'/g' sample_madspin_card.dat >trashA_madspin.dat
                    sed -e 's/Htag/'"a a"'/g' trashA_madspin.dat >trashB_madspin.dat
                    sed -e 's/Wdecaytag/'"lt lt"'/g' trashB_madspin.dat >WminusH_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_madspin_card.dat
                    rm trashA_madspin.dat trashB_madspin.dat
                fi
            fi
            if [[ $CHARGE == "plus" ]]; then
                if [[ $DECAY == "tt" ]]; then
                    sed -e 's/Wtag/'"w+"'/g' sample_madspin_card.dat >trashA_madspin.dat
                    sed -e 's/Htag/'"ta+ ta-"'/g' trashA_madspin.dat >trashB_madspin.dat
                    sed -e 's/Wdecaytag/'"lt lt"'/g' trashB_madspin.dat >WplusH_HTo2Tau_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_madspin_card.dat
                    rm trashA_madspin.dat trashB_madspin.dat
                fi
                if [[ $DECAY == "ww" ]]; then
                    sed -e 's/Wtag/'"w+"'/g' sample_madspin_card.dat >trashA_madspin.dat
                    sed -e 's/Htag/'"w+ w- > ltjets ltjets ltjets ltjets"'/g' trashA_madspin.dat >trashB_madspin.dat
                    sed -e 's/Wdecaytag/'"ltjets ltjets"'/g' trashB_madspin.dat >WplusH_HToWW_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_madspin_card.dat
                    rm trashA_madspin.dat trashB_madspin.dat
                fi
                if [[ $DECAY == "gg" ]]; then
                    sed -e 's/Wtag/'"w+"'/g' sample_madspin_card.dat >trashA_madspin.dat
                    sed -e 's/Htag/'"a a"'/g' trashA_madspin.dat >trashB_madspin.dat
                    sed -e 's/Wdecaytag/'"lt lt"'/g' trashB_madspin.dat >WplusH_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_madspin_card.dat
                    rm trashA_madspin.dat trashB_madspin.dat
                fi
            fi
        done
    done
done
#extramodels.dat | WH(WW) and WH(tautau) have the same model. We need a different model for H(aa)
for DECAY in $DECAYS
do
    for CHARGE in $CHARGES
    do
        for cMultiple in 1 40 80 120
        do
            if [[ $DECAY == "gg" ]]; then
                sed -e 's/Mtag/'"haa"'/g' sample_extramodels.dat >W${CHARGE}H_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_extramodels.dat 
            fi
            if [[ $DECAY == "ww" ]]; then
                sed -e 's/Mtag/'"py3"'/g' sample_extramodels.dat >W${CHARGE}H_HToWW_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_extramodels.dat                 
            fi
            if [[ $DECAY == "tt" ]]; then
                sed -e 's/Mtag/'"py3"'/g' sample_extramodels.dat >W${CHARGE}H_HTo2Tau_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_extramodels.dat                 
            fi
        done
    done
done 
#run_card.dat, FKS_params.dat | both cards are independent from the Yukawa coupling and the decay channel. These line are only there, to have all cards there with a consistent naming scheme
for CHARGE in $CHARGES
do
    for cMultiple in 1 40 80 120
    do
        cp sample_FKS_params.dat W${CHARGE}H_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_FKS_params.dat 
        cp sample_FKS_params.dat W${CHARGE}H_HToWW_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_FKS_params.dat
        cp sample_FKS_params.dat W${CHARGE}H_HTo2Tau_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_FKS_params.dat
        cp sample_run_card.dat W${CHARGE}H_HToGG_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_run_card.dat
        cp sample_run_card.dat W${CHARGE}H_HToWW_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_run_card.dat
        cp sample_run_card.dat W${CHARGE}H_HTo2Tau_TuneCP5_KappaC${cMultiple}_13TeV_amcatnloFxFx-pythia8_run_card.dat 
    done
done
