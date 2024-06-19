#!bin/bash
#the purpose of this file is to make a Fragment file with a variable mass value
#Makes a new File

cp Zprimetobb_MX_TuneCP5_13p6TeV_pythia8_cff.py  Zprimetobb_M$1_TuneCP5_13p6TeV_pythia8_cff.py
#Replaces first instance of mass
sed -i "s/'32:m0 = .*/'32:m0 = $1',/" Zprimetobb_M$1_TuneCP5_13p6TeV_pythia8_cff.py
#removes Windows end of line delimeter ^M
file=Zprimetobb_M$1_TuneCP5_13p6TeV_pythia8_cff.py
dos2unix $file
