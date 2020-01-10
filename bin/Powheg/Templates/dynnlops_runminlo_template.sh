#!/bin/bash
base=${rootfolder}/${folderName}/minlo-run

eosbase=${eosdir}
config=$$1
seed=$$2

cd $$base
eval `scram runtime -sh`
cd -

cp $$base/../$$config powheg.input
sed -i "s/^iseed.*/iseed $$seed/g" powheg.input
sed -i "s/^numevts.*/numevts 50000/g" powheg.input
echo "rwl_file 'pwg-rwl-scalesonly.dat'" >> powheg.input

cp $$base/../pwhg_main .
cp $$base/../lhef_analysis_3d .
cp $$base/../*.dat .

./pwhg_main &> log_$${seed}.txt
./lhef_analysis_3d

mkdir -p $${eosbase}/$$seed
cp MINLO*.top $${eosbase}/$$seed/

cp log_$${seed}.txt $${base}
