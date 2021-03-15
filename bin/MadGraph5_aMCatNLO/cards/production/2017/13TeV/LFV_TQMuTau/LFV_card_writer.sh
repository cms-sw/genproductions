processes=(LFV_ST_T LFV_TT_TTo)
general=QMuTau
quarktypes=(UMuTau CMuTau)
classes=(Scalar Vector Tensor)
sca_block=frblock8  # Clequ1
vec_block=(frblock10 frblock12 frblock5 frblock13) # Clq1, Clu, Ceu, Cqe
ten_block=frblock9  # Clequ3

declare -a c_types=("2 3 2 3" "2 3 3 2" "3 2 2 3" "3 2 3 2")
declare -a u_types=("2 3 1 3" "2 3 3 1" "3 2 1 3" "3 2 3 1")
declare -a u_antitypes=("1 3 2 3" "1 3 3 2" "3 1 2 3" "3 1 3 2")

for process in ${processes[*]}; do
    generalfolder=${process}${general}
    for qtype in ${quarktypes[*]}; do
        folder=${process}${qtype}
        echo ${folder}
        for class in ${classes[*]}; do
            classfolder=${folder}_${class}
            echo Copying folder to ${classfolder}
            cp -r ./${generalfolder} ./${classfolder}

            if [ ${qtype} = "UMuTau" ]
            then
                if [ $class = "Scalar" ]
                then
                    for tp in "${u_types[@]}"; do
                        echo "set param_card $sca_block $tp 1.00" >> ./${classfolder}/${generalfolder}_customizecards.dat
                    done
                elif [ $class = "Vector" ]
                then
                    for v in ${vec_block[*]}; do
                        if [ $v = "frblock13" ]
                        then
                            for tp in "${u_antitypes[@]}"; do
                                echo "set param_card $v $tp 1.00" >> ./${classfolder}/${generalfolder}_customizecards.dat
                            done
                        else
                            for tp in "${u_types[@]}"; do
                                echo "set param_card $v $tp 1.00" >> ./${classfolder}/${generalfolder}_customizecards.dat
                            done
                        fi
                    done
                elif [ $class = "Tensor" ]
                then
                    for tp in "${u_types[@]}"; do
                        echo "set param_card $ten_block $tp 1.00" >> ./${classfolder}/${generalfolder}_customizecards.dat
                    done
                fi
                if [ $process = "LFV_TT_TTo" ]
                then
                    echo "generate p p > t t~, (t > b w+, w+ > j j), (t~ > u~ mu+ ta-)" >> ./${classfolder}/${generalfolder}_proc_card.dat
                    echo "add process p p > t t~, (t > b w+, w+ > j j), (t~ > u~ mu- ta+)" >> ./${classfolder}/${generalfolder}_proc_card.dat
                    echo "add process p p > t t~, (t > u mu+ ta-), (t~ > b~ w-, w- > j j)" >> ./${classfolder}/${generalfolder}_proc_card.dat
                    echo "add process p p > t t~, (t > u mu- ta+), (t~ > b~ w-, w- > j j)" >> ./${classfolder}/${generalfolder}_proc_card.dat
                    echo "output LFV_TT_TToUMuTau_${class}" >> ./${classfolder}/${generalfolder}_proc_card.dat
                elif [ $process = "LFV_ST_T" ]
                then
                    sed -i "s/${generalfolder}/${classfolder}/g" ./${classfolder}/${generalfolder}_proc_card.dat
                fi
            elif [ ${qtype} = "CMuTau" ]
            then
                if [ $class = "Scalar" ]
                then
                    for tp in "${c_types[@]}"; do
                        echo "set param_card $sca_block $tp 1.00" >> ./${classfolder}/${generalfolder}_customizecards.dat
                    done
                elif [ $class = "Vector" ]
                then
                    for v in ${vec_block[*]}; do
                        for tp in "${c_types[@]}"; do
                            echo "set param_card $v $tp 1.00" >> ./${classfolder}/${generalfolder}_customizecards.dat
                        done
                    done
                elif [ $class = "Tensor" ]
                then
                    for tp in "${c_types[@]}"; do
                        echo "set param_card $ten_block $tp 1.00" >> ./${classfolder}/${generalfolder}_customizecards.dat
                    done
                fi
                if [ $process = "LFV_TT_TTo" ]
                then
                    echo "generate p p > t t~, (t > b w+, w+ > j j), (t~ > c~ mu+ ta-)" >> ./${classfolder}/${generalfolder}_proc_card.dat
                    echo "add process p p > t t~, (t > b w+, w+ > j j), (t~ > c~ mu- ta+)" >> ./${classfolder}/${generalfolder}_proc_card.dat
                    echo "add process p p > t t~, (t > c mu+ ta-), (t~ > b~ w-, w- > j j)" >> ./${classfolder}/${generalfolder}_proc_card.dat
                    echo "add process p p > t t~, (t > c mu- ta+), (t~ > b~ w-, w- > j j)" >> ./${classfolder}/${generalfolder}_proc_card.dat
                    echo "output LFV_TT_TToCMuTau_${class}" >> ./${classfolder}/${generalfolder}_proc_card.dat
                elif [ $process = "LFV_ST_T" ]
                then
                    sed -i "s/${generalfolder}/${classfolder}/g" ./${classfolder}/${generalfolder}_proc_card.dat
                fi
            fi
            rename ./${classfolder}/${generalfolder} ./${classfolder}/${classfolder} ./${classfolder}/${generalfolder}*
        done
    done
done

ls ./LF*/
