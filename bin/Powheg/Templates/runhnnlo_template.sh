#!/bin/bash
base=${rootfolder}/${folderName}/${scale}

config=$$1
seed=$$2

pushd $$base
eval `scram runtime -sh`
popd 

cat $$base/../$$config | sed -e "s#SEED#$$seed#g" > config.input
cat config.input | sed -e "s#MSTW2008nnlo68cl#NNPDF31_nnlo_hessian_pdfas#g" > config.input.temp
mv config.input.temp config.input

cp $$base/../hnnlo .
cp $$base/../br* .

./hnnlo < config.input &> log_$${seed}.txt

cp HNNLO-LHC13* $${base}

cp log_$${seed}.txt $${base}

