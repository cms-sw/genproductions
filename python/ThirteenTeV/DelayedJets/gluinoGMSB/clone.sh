TEMPLATE=gluinoGMSB_template.py
#for M in $(seq 1000 100 3000); do
#    for CTAU in 300 1000 10000 30000; do
for M in $(seq 1000 100 1000); do
    for CTAU in 300; do
    	NAME="gluinoGMSB_M${M}_ctau${CTAU}p0_TuneCP2_13TeV_pythia8_cff.py"
        cp $TEMPLATE $NAME
        sed -i "s|@MASS_POINT|${M}|g" $NAME
        sed -i "s|@CTAU|${CTAU}|g" $NAME
    done
done
