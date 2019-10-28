#!/bin/bash
base=${rootfolder}/${folderName}/${subfolderName}

config=$$1
seed=$$2

cd $$base
eval `scram runtime -sh`
cd -

cat $$base/../$$config | sed -e "s#SEED#$$seed#g" > config.input
cat config.input | sed -e "s#MSTW2008nnlo68cl#NNPDF31_nnlo_hessian_pdfas#g" > config.input.temp
mv config.input.temp config.input

cp $$base/../dynnlo .

./dynnlo < config.input &> log_$${seed}.txt

cp *.top $${base}

cp log_$${seed}.txt $${base}

