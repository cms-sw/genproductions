#!bin/bash
#the purpose of this file is to make a Fragment file with a variable mass value
#Makes a new File
cp Bstartobg_MX_TuneCP5_13p6TeV_pythia8_cff.py Bstartobg_M$1_TuneCP5_13p6TeV_pythia8_cff.py
#Replaces first instance of mass
sed -i "s/'4000005:m0 =.*/'4000005:m0 =$1'/" Bstartobg_M$1_TuneCP5_13p6TeV_pythia8_cff.py
#replaces the second instance of mass
sed -i "s/'ExcitedFermion:Lambda =.*/'ExcitedFermion:Lambda = $1'/" Bstartobg_M$1_TuneCP5_13p6TeV_pythia8_cff.py
#removes Windows end of line delimeter ^M
file=Bstartobg_M$1_TuneCP5_13p6TeV_pythia8_cff.py
dos2unix $file
