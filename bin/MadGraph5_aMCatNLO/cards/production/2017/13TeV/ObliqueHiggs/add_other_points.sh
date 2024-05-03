#!/bin/bash

mainproc="TTTT_hhat_0p0"
# otherpoints="0.08 0.12 0.16"
otherpoints="0.08 0.12 0.16"
for pt in $otherpoints ; do
    ptnodot=$(echo $pt | sed 's/\./p/g')
    newproc=${mainproc/0p0/$ptnodot}
    [ -e $newproc ] || mkdir $newproc
    cp -rp $mainproc/*.dat $newproc/
    rename "0p0_" "${ptnodot}_" $newproc/*dat
    sed -i 's/0\.0/'"$pt"'/' $newproc/*customizecards.dat
    sed -i 's/'"$mainproc"'/'"$newproc"'/' $newproc/*proc_card.dat
done
