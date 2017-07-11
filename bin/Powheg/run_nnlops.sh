#!/bin/bash

fail_exit() { echo "$@"; exit 1; }

#set -o verbose
EXPECTED_ARGS=0

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0`"
    exit 1
fi

echo "   ______________________________________     "
echo "           Running NNLOPS reweight            "
echo "   ______________________________________     "

prefix=jobPowheg

eval `scramv1 runtime -sh`
rm -f splitLheFiles mergeLheFiles lhefiles.txt
g++ -Wall -o splitLheFiles lheMacros/splitLheFiles.cpp
g++ -Wall -o mergeLheFiles lheMacros/mergeLheFiles.cpp

ls cmsgrid_final*.lhe > lhefiles.txt

comman="";
for fn in `cat lhefiles.txt`; do
    echo "the next file is $fn"
    comman="$comman ../$fn"    
done

cd jobPowheg1
../splitLheFiles ${comman}
mv out.lhe fornnlops
#mv powheg.input powheg.input.orig
#head -n 12 pwg-rwl.dat > temp1.dat
#tail -n 1 pwg-rwl.dat > temp2.dat
#cat temp1.dat temp2.dat > pwg-rwl-scaleonly.dat
#sed s/pwg-rwl/pwg-rwl-scaleonly/ powheg.input.orig > powheg.input
./nnlopsreweighter-newrwgt
../mergeLheFiles fornnlops.nnlo pdf.lhe
mv out.lhe ../cmsgrid_final.lhe
cd ..

echo "Reweighting done"
echo "End of job on " `date`
exit 0;
