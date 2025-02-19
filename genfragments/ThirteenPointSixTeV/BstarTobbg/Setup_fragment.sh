#!bin/bash
#the purpose of this file is to make a Fragment file with a variable mass value
#Makes a new File
cp Bstartobbg_MX_TuneCP5_13p6TeV_pythia8_cff.py  Bstartobbg_M$1_TuneCP5_13p6TeV_pythia8_cff.py
#Replaces first instance of mass
sed -i "s/'4000005:m0 =.*/'4000005:m0 =$1'/" Bstartobbg_M$1_TuneCP5_13p6TeV_pythia8_cff.py
#replaces the second instance of mass
sed -i "s/'ExcitedFermion:Lambda =.*/'ExcitedFermion:Lambda = $1'/" Bstartobbg_M$1_TuneCP5_13p6TeV_pythia8_cff.py
#removes Windows end of line delimeter ^M
file=Bstartobbg_M$1_TuneCP5_13p6TeV_pythia8_cff.py
dos2unix $file
