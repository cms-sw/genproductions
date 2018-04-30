#!/bin/bash

set -euo pipefail

if [ $# -eq 0 ]; then
    linkMELA=false
else
    linkMELA=$1
fi
if [ $linkMELA != true ] && [ $linkMELA != false ]; then
    echo "Argument to $0 has to be true to link MELA or false to not link it"
fi

jhugenversion=v7.1.4

wget --no-check-certificate http://spin.pha.jhu.edu/Generator/JHUGenerator.${jhugenversion}.tar.gz -O JHUGenerator.${jhugenversion}.tar.gz
tar xzvf JHUGenerator.${jhugenversion}.tar.gz
rm -rf AnalyticMELA
rm -f manJHUGenerator*.pdf
cd JHUGenerator
sed -i "s/UseLHAPDF=No/UseLHAPDF=Yes/" makefile
if ! $linkMELA; then
    sed -i "s/linkMELA = Yes/linkMELA = No/" makefile
    rm -rf ../JHUGenMELA
fi
make
rm ../JHUGenerator.${jhugenversion}.tar.gz

