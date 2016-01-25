#!/bin/bash

wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/JHUGenerator.v6.8.5.tar.gz -O JHUGenerator.v6.8.5.tar.gz 
tar xzvf JHUGenerator.v6.8.5.tar.gz 
rm -rf AnalyticMELA
rm -rf JHUGenMELA
cd JHUGenerator
make
rm ../JHUGenerator.v6.8.5.tar.gz

