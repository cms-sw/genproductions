#!/bin/bash

#put this script under the log directory after crab submission, and execute 'sh nanaXS.sh'

Xs_arr=()
Xserr_arr=()
nTotal_arr=()
nTotalPos_arr=()
nTotalNeg_arr=()
nPass_arr=()
nPassPos_arr=()
nPassNeg_arr=()
count=0

for filein in $(ls ${PWD} | grep .gz);do
  tar zxf $filein
  num=$(echo $filein | cut -d '.' -f 1 | cut -d '_' -f 2)
  if [ $count -eq 0 ]
  then
    unit=$(cat cmsRun-stdout-${num}.log | grep 'final cross section' -n | cut -d ':' -f 3 | rev | cut -d ' ' -f 1 | rev)
  fi
  linetemp=$(cat cmsRun-stdout-${num}.log | grep 'Before matching' -n | cut -d ':' -f 1)
  let linetemp2=$linetemp-2
  xs_temp=$(cat cmsRun-stdout-${num}.log | tail -n +${linetemp2} | head -n 1 | cut -f 3 | cut -d ' ' -f 1)
  xserr_temp=$(cat cmsRun-stdout-${num}.log | tail -n +${linetemp2} | head -n 1 | cut -f 3 | cut -d ' ' -f 3)
  nTotal_temp=$(cat cmsRun-stdout-${num}.log | tail -n +${linetemp2} | head -n 1 | cut -f 8)
  nTotalPos_temp=$(cat cmsRun-stdout-${num}.log | tail -n +${linetemp2} | head -n 1 | cut -f 9)
  nTotalNeg_temp=$(cat cmsRun-stdout-${num}.log | tail -n +${linetemp2} | head -n 1 | cut -f 10)
  nPass_temp=$(cat cmsRun-stdout-${num}.log | tail -n +${linetemp2} | head -n 1 | cut -f 5)
  nPassPos_temp=$(cat cmsRun-stdout-${num}.log | tail -n +${linetemp2} | head -n 1 | cut -f 6)
  nPassNeg_temp=$(cat cmsRun-stdout-${num}.log | tail -n +${linetemp2} | head -n 1 | cut -f 7)

  Xs_arr[$count]=$xs_temp
  Xserr_arr[$count]=$xserr_temp
  nTotal_arr[$count]=$nTotal_temp
  nTotalPos_arr[$count]=$nTotalPos_temp
  nTotalNeg_arr[$count]=$nTotalNeg_temp
  nPass_arr[$count]=$nPass_temp
  nPassPos_arr[$count]=$nPassPos_temp
  nPassNeg_arr[$count]=$nPassNeg_temp
  let count=count+1
#  echo "count:",$count
done

length=${#Xs_arr[@]}
#echo 'length:',$length

#calculate xs and error
i=0
Xs_sum=0
Xserr2_sum=0
Total_sum=0
TotalPos_sum=0
TotalNeg_sum=0
Pass_sum=0
PassPos_sum=0
PassNeg_sum=0

while [ $i -lt ${#Xs_arr[@]} ]
do
temp_xs=$(printf "%10.3f" ${Xs_arr[$i]})
Xs_sum=$(echo "scale=3; $Xs_sum + $temp_xs"| bc)

temp_xserr=$(printf "%10.3f" ${Xserr_arr[$i]})
Xserr_sum=$(echo "scale=3; $Xserr2_sum + $temp_xserr"| bc)

temp_total=$(printf "%10.3f" ${nTotal_arr[$i]})
Total_sum=$(echo "scale=3; $Total_sum + $temp_total"| bc)

temp_pos=$(printf "%10.3f" ${nTotalPos_arr[$i]})
TotalPos_sum=$(echo "scale=3; $TotalPos_sum + $temp_pos"| bc)

temp_neg=$(printf "%10.3f" ${nTotalNeg_arr[$i]})
TotalNeg_sum=$(echo "scale=3; $TotalNeg_sum + $temp_neg"| bc)

temp_pass=$(printf "%10.3f" ${nPass_arr[$i]})
Pass_sum=$(echo "scale=3; $Pass_sum + $temp_pass"| bc)

temp_passpos=$(printf "%10.3f" ${nPassPos_arr[$i]})
PassPos_sum=$(echo "scale=3; $PassPos_sum + $temp_passpos"| bc)

temp_passneg=$(printf "%10.3f" ${nPassNeg_arr[$i]})
PassNeg_sum=$(echo "scale=3; $PassNeg_sum + $temp_passneg"| bc)
let i++
done

Xs_before=$(echo "scale=3; $Xs_sum/$length"|bc)
Xserr_before=$(echo "scale=3; $Xserr_sum/$length"|bc)

TotalPos_output=$(printf "%10.0f" $TotalPos_sum)
TotalNeg_output=$(printf "%10.0f" $TotalNeg_sum)
PassPos_output=$(printf "%10.0f" $PassPos_sum)
PassNeg_output=$(printf "%10.0f" $PassNeg_sum)
echo 'Total Positive events number before matching:' $TotalPos_output
echo 'Total Negative events number before matching:' $TotalNeg_output
echo 'Total Positive events number after matching:' $PassPos_output
echo 'Total Negative events number after matching:' $PassNeg_output

nabsTotal_temp=$(echo "scale=0; $TotalPos_sum - $TotalNeg_sum"|bc)
nabsTotal=$(printf "%10.0f" $nabsTotal_temp)
nabsPass=$(echo "scale=0; $PassPos_sum - $PassNeg_sum"|bc)

if [ $nabsTotal -lt 1 ]
then
  echo 'no event pass matching, exit!!'
else
  fracAcc=$(echo "scale=6; $nabsPass/$nabsTotal"|bc)
#  echo 'fracAcc:' $fracAcc
  Xs_after=$(echo "scale=6; $Xs_before*$fracAcc"|bc)
#  echo 'Xs_after:' $Xs_after
  effPos=$(echo "scale=6; $PassPos_sum/$TotalPos_sum"|bc)
#  echo 'effPos:' $effPos
  effPos_err2=$(echo "scale=6; (1 - $effPos)*$effPos"|bc)
#  echo 'effPos_err2:' $effPos_err2
  TotalNeg_temp=$(printf "%10.0f" $TotalNeg_sum)
  if [ $TotalNeg_temp -gt 0 ]
  then
    effNeg=$(echo "scale=6; $PassNeg_sum/$TotalNeg_sum"|bc)
    effNeg_err2=$(echo "scale=6; (1 - $effNeg)*$effNeg"|bc)
  else
    effNeg=0
    effNeg_err2=0
  fi

  if [ $TotalNeg_temp -gt 0 ]
  then
    eff_err2=$(echo "scale=8; ($TotalPos_sum*$effPos_err2 + $TotalNeg_sum*$effNeg_err2)/($nabsTotal*$nabsTotal)"|bc)
  else
   eff_err2=$(echo "scale=8; ($TotalPos_sum*$effPos_err2)/($nabsTotal*$nabsTotal)"|bc)
  fi
#  echo 'eff_err2:' $eff_err2
  delta2Veto=$(echo "scale=8; $eff_err2/($fracAcc*$fracAcc)"|bc)
#  echo 'delta2Veto:' $delta2Veto
  delta2Sum=$(echo "scale=6; $delta2Veto + $Xserr_before*$Xserr_before/($Xs_before*$Xs_before)"|bc)
#  echo 'delta2Sum:' $delta2Sum
  relErr=$(echo "scale=5; sqrt($delta2Sum)"|bc)
#  echo 'relErr:' $relErr
  Xserr_after=$(echo "scale=3; $Xs_after*$relErr"|bc)
#  echo 'Xserr_after:' $Xserr_after
  Xs_final=$(printf "%10.2f" $Xs_after)
  Xserr_final=$(printf "%10.2f" $Xserr_after)
  echo "Cross section after matching is:" $Xs_final " +- " $Xserr_final $unit
  rm *.log
  rm *.xml
fi
