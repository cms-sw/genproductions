#for file in BulkGraviton_hh_htatahcc_narrow_M*/BulkGraviton_hh_htatahbb_narrow_M*
#do
#    mv "$file" "${file/_htatahbb_/_htatahcc_}"
#done

for file in BulkGraviton_hh_htatahcc_narrow_M*/BulkGraviton_hh_htatahcc_narrow_M*
do 
sed -i -e 's/generate p p > y , ( y > H H, H > ta+ ta- , H > b b~)/generate p p > y , ( y > H H, H > ta+ ta- , H > c c~)/g' "$file"
rm "$file~"
done 