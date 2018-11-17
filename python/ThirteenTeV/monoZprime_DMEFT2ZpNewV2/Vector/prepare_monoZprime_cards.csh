#!/bin/tcsh 

setenv pwd $PWD

### Central production grid
foreach MDM (1 10 50 150 500 1000)
  if(${MDM} == 1) then
      foreach MMED (10 20 50 100 200 300 500 1000 10000)
  else
      if(${MDM} == 10)   foreach MMED (10 15 50 100 10000)
      if(${MDM} == 50)   foreach MMED (10 50 95 200 300 10000)
      if(${MDM} == 150)  foreach MMED (10 200 295 500 1000 10000)
      if(${MDM} == 500)  foreach MMED (10 500 995 10000)
      if(${MDM} == 1000) foreach MMED (10 1000 10000)
endif

setenv CARDNAME DM_MonoZPrime_V_Mx${MDM}_Mv${MMED}

cp ${pwd}/extramodels.dat  ${pwd}/${CARDNAME}_extramodels.dat
cp ${pwd}/run_card.dat  ${pwd}/${CARDNAME}_run_card.dat

sed -e "s/_MMED_/${MMED}/" -e "s/_MDM_/${MDM}/" ${pwd}/customizecards.dat > ${pwd}/${CARDNAME}_customizecards.dat
sed "s/_NAME_/${CARDNAME}/" ${pwd}/proc_card.dat > ${pwd}/${CARDNAME}_proc_card.dat


end
end
