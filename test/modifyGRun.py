fileName=$(sed 's/Config file/\n/' | tail -1 | sed 's/crea/\n/g' | sed -n 1'p')
sed 's/8E29/GRun/g' $fileName > temp.py
mv temp.py $fileName
