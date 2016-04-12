#!/bin/bash

wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/JHUGenerator.v6.9.5.tar.gz -O JHUGenerator.v6.9.8.tar.gz
tar xzvf JHUGenerator.v6.9.8.tar.gz
rm -rf AnalyticMELA
rm -rf JHUGenMELA
cd JHUGenerator
sed -i "s/UseLHAPDF=No/UseLHAPDF=Yes/" makefile
make
rm ../JHUGenerator.v6.9.8.tar.gz

