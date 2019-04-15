TEMPLATE=ADDmonoZ_template.py
for MD in 1 2 3; do
    for N in $(seq 2 1 8); do
        NAME="ADDmonoZ_MD_${MD}_d_${N}_TuneCP5_13TeV_pythia8_cfi.py"
        cp $TEMPLATE $NAME
        sed -i "s|@N|${N}|g" $NAME
        sed -i "s|@MD|$((1000*${MD})).|g" $NAME
    done
done