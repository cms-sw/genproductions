
CARDS=(VBF_HH_CV_0_5_C2V_1_C3_1_13TeV-madgraph VBF_HH_CV_1_C2V_1_C3_0_13TeV-madgraph VBF_HH_CV_1_C2V_2_C3_1_13TeV-madgraph VBF_HH_CV_1_5_C2V_1_C3_1_13TeV-madgraph VBF_HH_CV_1_C2V_1_C3_1_13TeV-madgraph VBF_HH_CV_1_C2V_0_C3_1_13TeV-madgraph VBF_HH_CV_1_C2V_1_C3_2_13TeV-madgraph)

for card in "${CARDS[@]}"; do
    cp -r template_cards ${card}
    sed -i "s#<outdir>#${card}#g" ${card}/proc_card.dat
    
    if [ "$card" = "VBF_HH_CV_0_5_C2V_1_C3_1_13TeV-madgraph" ]; then
	CV="0.5"
	C2V="1.0"
	C3="1.0"
    elif [ "$card" = "VBF_HH_CV_1_C2V_1_C3_0_13TeV-madgraph" ]; then
	CV="1.0"
	C2V="1.0"
	C3="0.0"
    elif [ "$card" = "VBF_HH_CV_1_C2V_2_C3_1_13TeV-madgraph" ]; then
	CV="1.0"
	C2V="2.0"
	C3="1.0"
    elif [ "$card" = "VBF_HH_CV_1_5_C2V_1_C3_1_13TeV-madgraph" ]; then
	CV="1.5"
	C2V="1.0"
	C3="1.0"
    elif [ "$card" = "VBF_HH_CV_1_C2V_1_C3_1_13TeV-madgraph" ]; then
	CV="1.0"
	C2V="1.0"
	C3="1.0"
    elif [ "$card" = "VBF_HH_CV_1_C2V_0_C3_1_13TeV-madgraph" ]; then
	CV="1.0"
	C2V="0.0"
	C3="1.0"
    elif [ "$card" = "VBF_HH_CV_1_C2V_1_C3_2_13TeV-madgraph" ]; then
	CV="1.0"
	C2V="1.0"
	C3="2.0"
    fi
    
    sed -i "s#<CV>#${CV}#g" ${card}/customizecards.dat
    sed -i "s#<C2V>#${C2V}#g" ${card}/customizecards.dat
    sed -i "s#<C3>#${C3}#g" ${card}/customizecards.dat
done

