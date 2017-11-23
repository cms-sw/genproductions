for file in BulkGraviton_hh_hVVhcc_fullLep_narrow_M*/BulkGraviton_hh_hVVhbb_fullLep_narrow_M*
do
    mv "$file" "${file/_hVVhbb_/_hVVhcc_}"
done

for file in BulkGraviton_hh_hVVhcc_fullLep_narrow_M*/BulkGraviton_hh_hVVhcc_fullLep_narrow_M*
do 
sed -i -e 's/generate p p > y , ( y > H H , H > lep lep nus nus, H > b b~)/generate p p > y , ( y > H H , H > lep lep nus nus, H > c c~)/g' "$file"
rm "$file~"
done 