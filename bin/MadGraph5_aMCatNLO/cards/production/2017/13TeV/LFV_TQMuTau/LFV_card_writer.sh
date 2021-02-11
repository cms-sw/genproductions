processes=(LFV_ST_T LFV_TT_TTo)
general=QMuTau
quarktypes=(UMuTau CMuTau)
classes=(Scalar Vector Tensor)
vectors=(Clq Clu Cqe Ceu)
c_types=(2x3x2x3 2x3x3x2 3x2x2x3 3x2x3x2)
u_types=(2x3x1x3 2x3x3x1 3x2x1x3 3x2x3x1)
u_antitypes=(1x3x2x3 1x3x3x2 3x1x2x3 3x1x3x2)

for process in ${processes[*]}; do
    generalfolder=${process}${general}
    for qtype in ${quarktypes[*]}; do
        folder=${process}${qtype}
        echo ${folder}
        if [ ${qtype} = "UMuTau" ]
        then
            for class in ${classes[*]}; do
                classfolder=${folder}_${class}
                echo Copying folder to ${folder}_${class}
                cp -r ./${generalfolder} ./${folder}_${class}

                if [ $class = "Vector" ]
                then
                    for v in ${vectors[*]}; do
                        if [ $v = "Clq" ]
                        then
                            for t in ${u_types[*]}; do
                                sed -i "46,212s/0.000000e+00 # ${v}1${t}/1.000000e+00 # ${v}1${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                            done
                        elif [ $v = "Clu" ]
                        then
                            for t in ${u_types[*]}; do
                                sed -i "218,298s/0.000000e+00 # ${v}${t}/1.000000e+00 # ${v}${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                            done

                        elif [ $v = "Cqe" ]
                        then
                            for t in ${u_antitypes[*]}; do
                                sed -i "304,384s/0.000000e+00 # ${v}${t}/1.000000e+00 # ${v}${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                            done
                        elif [ $v = "Ceu" ]
                        then
                            for t in ${u_types[*]}; do
                                sed -i "504,584s/0.000000e+00 # ${v}${t}/1.000000e+00 # ${v}${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                            done
                        fi
                    done
                elif [ $class = "Scalar" ]
                then
                    for t in ${u_types[*]}; do
                        sed -i "762,842s/0.000000e+00 # Clequ1${t}/1.000000e+00 # Clequ1${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                    done
                elif [ $class = "Tensor" ]
                then
                    for t in ${u_types[*]}; do
                        sed -i "848,928s/0.000000e+00 # Clequ3${t}/1.000000e+00 # Clequ3${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                    done
                fi

                sed -i "s/${generalfolder}/${folder}_${class}/g" ./${folder}_${class}/${generalfolder}_proc_card.dat

                rename ./${folder}_${class}/${generalfolder} ./${folder}_${class}/${folder}_${class} ./${folder}_${class}/${generalfolder}*
            done
        elif [ ${qtype} = "CMuTau" ]
        then
            for class in ${classes[*]}; do
                echo Copying folder to ${folder}_${class}
                cp -r ./${generalfolder} ./${folder}_${class}

                if [ $class = "Vector" ]
                then
                    for v in ${vectors[*]}; do
                        if [ $v = "Clq" ]
                        then
                            for t in ${c_types[*]}; do
                                sed -i "46,212s/0.000000e+00 # ${v}1${t}/1.000000e+00 # ${v}1${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                            done
                        elif [ $v = "Clu" ]
                        then
                            for t in ${c_types[*]}; do
                                sed -i "218,298s/0.000000e+00 # ${v}${t}/1.000000e+00 # ${v}${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                            done

                        elif [ $v = "Cqe" ]
                        then
                            for t in ${c_types[*]}; do
                                sed -i "304,384s/0.000000e+00 # ${v}${t}/1.000000e+00 # ${v}${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                            done
                        elif [ $v = "Ceu" ]
                        then
                            for t in ${c_types[*]}; do
                                sed -i "504,584s/0.000000e+00 # ${v}${t}/1.000000e+00 # ${v}${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                            done
                        fi
                    done
                elif [ $class = "Scalar" ]
                then
                    for t in ${c_types[*]}; do
                        sed -i "762,842s/0.000000e+00 # Clequ1${t}/1.000000e+00 # Clequ1${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                    done
                elif [ $class = "Tensor" ]
                then
                    for t in ${c_types[*]}; do
                        sed -i "848,928s/0.000000e+00 # Clequ3${t}/1.000000e+00 # Clequ3${t}/g" ./${folder}_${class}/${generalfolder}_param_card.dat
                    done
                fi

                sed -i "s/${generalfolder}/${folder}_${class}/g" ./${folder}_${class}/${generalfolder}_proc_card.dat

                rename ./${folder}_${class}/${generalfolder} ./${folder}_${class}/${folder}_${class} ./${folder}_${class}/${generalfolder}*
            done
        fi
    done
done

ls ./LF*/

#    echo Deleting folder for ${folder}_${class}
#    rm -rf ./${folder}_${class}
#done
