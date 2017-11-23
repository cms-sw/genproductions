for file in BulkGraviton_hh_haahcc_narrow_M*/BulkGraviton_hh_haahbb_narrow_M*
do
    mv "$file" "${file/_haahbb_/_haahcc_}"
done

for file in BulkGraviton_hh_haahcc_narrow_M*/BulkGraviton_hh_haahcc_narrow_M*
do 
sed -i -e 's/generate p p > y , ( y > H H, H > a a , H > b b~)/generate p p > y , ( y > H H, H > a a , H > c c~)/g' "$file"
rm "$file~"
done 