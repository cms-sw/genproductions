#!/bin/bash

wget --no-check-certificate  http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/JHUGenerator.v5.2.5.tar.gz -O JHUGenerator.v5.2.5.tar.gz
#cp patches/JHUGenerator.v5.2.5.tar.gz .
tar xzvf JHUGenerator.v5.2.5.tar.gz 
rm -rf AnalyticMELA
rm -rf JHUGenMELA
cd JHUGenerator
cp ../patches/mod_Parameters.F90 .
cp ../patches/boltdmdec/*.f .
make
gfortran -o boltdmdec boltdmdec.f readlhe_infile.f read_lhe_event_init.f
cd pdfs
wget http://pcteserver.mi.infn.it/~nnpdf/nnpdf30/NNPDF30_lo_as_0130.LHgrid.tgz
tar -zxvf ./NNPDF30_lo_as_0130.LHgrid.tgz
rm ./NNPDF30_lo_as_0130.LHgrid.tgz
cd ../
mkdir Bin
cp boltdmdec Bin
cp JHUGen    Bin/JHUGenRaw
mkdir Bin/pdfs/
ln -s  pdfs/NNPDF30_lo_as_0130.LHgrid Bin/pdfs/NNPDF30_lo_as_0130.LHgrid 
cp pdfs/*.tbl Bin/pdfs
cp pdfs/*.dat Bin/pdfs/
cp pdfs/*.dat Bin/pdfs/
cd ..
rm JHUGenerator.v5.2.5.tar.gz

