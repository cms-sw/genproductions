#!/bin/bash

find ./ -name "*.lhe" >> cleanlist.txt
find ./ -name "*.lhe.gz" >> cleanlist.txt
find ./ -name "*.f" >> cleanlist.txt
find ./ -name "*.F" >> cleanlist.txt
find ./ -name "*.cc" >> cleanlist.txt
find ./ -name "*.html" >> cleanlist.txt
find ./ -name "*.jpg" >> cleanlist.txt
find ./ -name "*.ps" >> cleanlist.txt
find ./ -name "*.lhe" >> cleanlist.txt
find ./ -name "gensym" >> cleanlist.txt
find ./ -name "ftn25" >> cleanlist.txt
find ./ -name "ftn26" >> cleanlist.txt
find ./ -wholename "*SubProcesses/*/*.o" >> cleanlist.txt
#find ./ -name "*.pyc" >> cleanlist.txt
#find ./ -name "*.pyo" >> cleanlist.txt


cat cleanlist.txt

for i in `cat cleanlist.txt` ; do
  echo "$i"
  rm -f "$i" >& /dev/null
done
rm -f cleanlist.txt >& /dev/null
