fileName=$(grep 'Config file .* created' | cut -d" " -f3)
sed -i 's/8E29/GRun/g' "$fileName"
echo "$fileName"
