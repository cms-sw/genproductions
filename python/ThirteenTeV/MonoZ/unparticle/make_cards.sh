TEMPLATE=Unpart_ZToEEAndMuMu_template.py
for DU in 1.01 1.02 1.04 1.06 1.09 1.10 1.20 1.30 1.40 1.50 1.60 1.70 1.80 1.90 2.00 2.20; do
    NAME="Unpart_ZToEEAndMuMu_SU_0_dU_$(echo $DU | sed 's|\.|p|')_LU_15_TuneCP5_13TeV_pythia8_cff.py"
    cp $TEMPLATE $NAME
    sed -i "s|@DU|${DU}|g" $NAME
done