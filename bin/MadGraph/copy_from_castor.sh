#!/bin/bash
file=${1}
  l=1
    while read line
    do
          echo $line                             

if [ ! -f ${line} ] ;then
 
echo copying  from castor 
#/castor/cern.ch/user/f/fabiocos/Generator/madgraph/V5_1.1/7TeV_Summer11/zjets_smzerobmass/${line} to ${line}
#rfcp /castor/cern.ch/user/f/fabiocos/Generator/madgraph/V5_1.1/7TeV_Summer11/zjets_smzerobmass/${line} ${line}
rfcp /castor/cern.ch/user/l/lenzip/Madgraph/avjets_incl//${line} ${line}

fi                                 
    done < "${file}"





