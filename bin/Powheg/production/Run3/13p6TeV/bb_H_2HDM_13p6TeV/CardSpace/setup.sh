#!/bin/bash

mass=$1
width=$2
hdamp=`awk "BEGIN {print ($mass+8.36)/4}"`
if [ "$3" == 1 ]; then hdamp=`awk "BEGIN {print 2*($mass+8.36)/4}"`; fi
if [ "$3" == 2 ]; then hdamp=`awk "BEGIN {print 0.5*($mass+8.36)/4}"`; fi

workdir=$PWD/../
template=$workdir/bbH_mssm_template/
if [ "$3" == "" ]; then massdir=$workdir/m${mass}; mkdir -p $massdir; fi
if [ "$3" == 1 ]; then massdir=$workdir/m${mass}_up; mkdir -p $massdir; fi
if [ "$3" == 2 ]; then massdir=$workdir/m${mass}_down; mkdir -p $massdir; fi

echo "Copying files:"
# cp -v $template/pwg-rwl.dat $massdir
cp -v $template/bbH.input-* $massdir
# cp -v $template/JHUGen.input $massdir

sed -i "s/XHMASSX/${mass}/g" $massdir/bbH.input-*
sed -i "s/XHWIDTHX/${width}/g" $massdir/bbH.input-*
sed -i "s/XHDAMPX/${hdamp}/g" $massdir/bbH.input-*

mv $massdir/bbH.input-base $massdir/bbH.input
