1;2c#!/bin/bash

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

#eval `scramv1 runtime -sh`
cd BASEDIR/pdfs
wget http://pcteserver.mi.infn.it/~nnpdf/nnpdf30/NNPDF30_lo_as_0130.LHgrid.tgz
tar -zxvf ./NNPDF30_lo_as_0130.LHgrid.tgz
rm ./NNPDF30_lo_as_0130.LHgrid.tgz
cd ..

GENCOMMAND
file=Out.lhe

head=`cat   $file | grep -in init | sed "s@:@ @g" | awk '{print $1-2}' | tail -1`
tail=`wc -l $file | awk -v tmp="$head" '{print $1-2-tmp}'`

tail -${tail} $file                                     >  ${file}_tail_dec
sed "s@\*\*\*@1.000@g"     ${file}_tail_dec              > ${file}_tail
./boltdmdec ${file}_tail
sed "s@-1000022@1000022@g" ${file}_tail_dec             >  ${file}_tail
head -$head $file                                       > cmsgrid.lhe
echo "  "XSECTION"   "XSECUNC"  1.00000000000E-00 10001" >> cmsgrid.lhe
echo "</init>"                                         >> cmsgrid.lhe
cat ${file}_tail                                       >> cmsgrid.lhe
rm ${file}
rm ${file}_tail
rm ${file}_tail_dec
mv cmsgrid_final.lhe ../
cd ..