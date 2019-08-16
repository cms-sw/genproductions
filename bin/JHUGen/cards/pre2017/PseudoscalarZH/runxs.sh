#!/bin/bash

Process=50
DecayMode=9
./JHUGenRaw Collider=1 Process=$Process DecayMode1=$DecayMode Unweighted=0 VegasNc0=100000 VegasNc2=50000 MReso=MED DataFile=tmp.dat > xsS.dat
./JHUGen    Collider=1 Process=$Process DecayMode1=$DecayMode Unweighted=0 VegasNc0=100000 VegasNc2=50000 MReso=MED DataFile=tmp.dat > xsX.dat
xsS=`cat xsS.dat   | grep integral | tail -1 | awk '{print $4}'`
xsX=`cat xsX.dat   | grep integral | tail -1 | awk '{print $4}'`
echo "SF : "$xsX" -- Scalar : "$xsS
xs=`echo $xsX $xsS | awk '{print $1/$2}'`
xsSU=`cat xsS.dat   | grep dev | tail -1 | awk '{print $5}'`
xsXU=`cat xsX.dat   | grep dev | tail -1 | awk '{print $5}'`
xsU=`echo $xsX $xsS $xsXU $xsSU | awk '{print sqrt($3*$3/$2/$2+$1*$1*$4*$4/$2/$2)}'`
echo $xs"  "$xsU > xsfile