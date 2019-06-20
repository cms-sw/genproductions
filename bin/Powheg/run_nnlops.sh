#!/bin/bash

fail_exit() { echo "$@"; exit 1; }

#set -o verbose
EXPECTED_ARGS=1

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` gridpack name"
    exit 1
fi

echo "   ______________________________________     "
echo "           Running NNLOPS reweight            "
echo "   ______________________________________     "

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

mkdir nnlops
cd nnlops
tar -xvf ../$1
../splitLheFiles ${comman}
mv out.lhe fornnlops
cp powheg.input.1_1 powheg.input
./nnlopsreweighter-newrwgt
../mergeLheFiles fornnlops.nnlo pdf.lhe
mv out.lhe ../cmsgrid_final.lhe
cd ..

echo "Reweighting done"
echo "End of job on " `date`
exit 0;
