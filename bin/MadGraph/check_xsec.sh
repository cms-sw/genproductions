#!/bin/bash

#Script needed for obtaing the <event> block information and collect process-by-process xsec and statistics
#Written by Alexis Kalogeropoulos on 12/7/2011
#Last revision 18/07/2011



file=$1


let num=0
unweight=`awk '/<\/?init>/{x = !x}x{a++}x && a > 2 && a <4{print $3}' "${file}"`
evts=`grep -c "<event>" "${file}"`
first_block=`awk '/<init>/{print NR }' "${file}"`
second_block=`awk '/<\/init>/{print NR }' "${file}"`
let num=${second_block}-${first_block}
xsec_check=`awk '/<\/?init>/{x = !x}x{a++}x && a > 2{sum+=$1}END{printf"%E" "\n", sum}' "${file}"`
num=$(( $num - 2 ))
weight_ratio=`awk -v e="$xsec_check" -v f="$evts" 'BEGIN{print (e / f)}'`

echo for file ${file} the number of events is  $evts
echo ""
echo The x-sec is $xsec_check pb from $num processes - The unit wgt is $weight_ratio and should be $unweight 
#awk '/<event>/ { getline ; list[ " process id  " $2 " has " $1 " particles "]++ } END { for (key in list) printf(" occurances %7d: %s\n", list[key], key ) }' "${file}"
echo ""
echo ~~~~~~~~~~~~~~~~~~ Testing of relative ratios based on number of entries for each subprocess~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo ""
awk '/<event>/ { getline  
		key = " process " $2 " with " $1 " particles "
		events = $3
                 # Number of times key exists
                   counts[key]++
                   sums[events]++
                 }
                 END { for (key in counts) {
                   # Clear list array
                  # Print the key and key count,
		  ratio1=counts[key]
		  ratio2=sums[events]
		  ratio=ratio1/ratio2
		  sumall=sumall+ratio
                    printf("%s: %d events, relative ratio : %E , sum of ratios : %E \n", key, counts[key] , ratio , sumall)  		    
    
 	}
	  }' "${file}"


echo ""
echo ~~~~~~~~~~~~~~~~~~ Testing of relative ratios based on cross-section for each subprocess~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo ""
let j=${num}
let add_ratio=0
while [ $j -ne "0" ]
do
counter=$((${num}-$j+1)) 
xsec_check1=`awk '/<\/?init>/{x = !x}x{a++}x && a > '$j'+1 && a < '$j'+3 {sum+=$1}END{printf"%E", sum}' "${file}"`
proc_count=`awk '/<event>/,/<\/event>/{if ($3 -eq $unweight  ) { print $1"  "$2"  "$3}}' "${file}"`
ratio=`awk -v a="$xsec_check1" -v b="$xsec_check" 'BEGIN{print (a / b)}'`
add_ratio=`awk -v c="$ratio" -v d="$add_ratio" 'BEGIN{printf"%E" ,(c + d)}'`
echo The x-section for the : $counter process is : ${xsec_check1} pb and the relative ratio is : ${ratio} and sum : ${add_ratio}
j=$(( $j - 1 ))
done


