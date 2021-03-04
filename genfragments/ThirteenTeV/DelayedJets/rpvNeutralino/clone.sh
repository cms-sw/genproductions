TEMPLATE=GluinoGluinoToNeutralinoNeutralinoTo2T2B2S_template_cff.py
for M in $(seq 1400 200 3000); do
    for CTAU in 300 1000 10000 30000; do
    	NAME="GluinoGluinoToNeutralinoNeutralinoTo2T2B2S_M_${M}_CTau_${CTAU}mm_TuneCP2_13TeV_pythia8_cff.py"
        cp $TEMPLATE $NAME
        sed -i "s|@MASS_POINT|${M}|g" $NAME
        sed -i "s|@CTAU|${CTAU}|g" $NAME
    done
done
