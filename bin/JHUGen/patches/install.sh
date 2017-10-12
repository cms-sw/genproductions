#!/bin/bash

sed -euo pipefail

if [ $# -eq 0 ]; then
    linkMELA=false
else
    linkMELA=$1
fi
if [ $linkMELA != true ] && [ $linkMELA != false ]; then
    echo "Argument to $0 has to be true to link MELA or false to not link it"
fi

wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/JHUGenerator.v7.0.9.tar.gz -O JHUGenerator.v7.0.9.tar.gz
tar xzvf JHUGenerator.v7.0.9.tar.gz
rm -rf AnalyticMELA
rm -f manJHUGenerator*.pdf
cd JHUGenerator
sed -i "s/UseLHAPDF=No/UseLHAPDF=Yes/" makefile
if ! $linkMELA; then
    sed -i "s/linkMELA = Yes/linkMELA = No/" makefile
    rm -rf ../JHUGenMELAv2
fi
make
rm ../JHUGenerator.v7.0.9.tar.gz

