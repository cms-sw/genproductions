#!/bin/bash

echo "cleaning unneeded files from gridpack"

find ./ -name "*.lhe" | xargs -r rm
find ./ -name "*.lhe.gz" | xargs -r rm
find ./ -name "*.lhe.rwgt" | xargs -r rm
find ./ -name "check_poles" | xargs -r rm
find ./ -name "test_MC" | xargs -r rm
find ./ -name "test_ME" | xargs -r rm
find ./ -name "*.f" | xargs -r rm
find ./ -name "*.F" | xargs -r rm
find ./ -name "*.cc" | xargs -r rm
find ./ -name "*.html" | xargs -r rm
find ./ -name "gensym" | xargs -r rm
find ./ -name "ftn25" | xargs -r rm
find ./ -name "ftn26" | xargs -r rm
find ./ -name "core.*" | xargs -r rm
find ./ -name "LSFJOB_*" | xargs -r rm -r
find ./ -wholename "*SubProcesses/*/*.o" | xargs -r rm

find ./ -name "*.ps" | xargs -r rm 
find ./ -name "*.log" | xargs -r rm 
find ./ -name "log*.txt" | xargs -r rm 
find ./ -name "*.tex" | xargs -r rm 
find ./ -name "*.o" | xargs -r rm 
find ./ -name "*.tar.gz" | xargs -r rm 
find ./ -name "*.f90" | xargs -r rm 
find ./ -name "*.cpp" | xargs -r rm 
find ./ -name "*.h" | xargs -r rm 
find ./ -name "test_soft_col_limits" | xargs -r rm 

rm -r mgbasedir/tests/
rm -r mgbasedir/doc
rm -r mgbasedir/LICENSE 
rm -r mgbasedir/README 
rm -r mgbasedir/UpdateNotes.txt

if [ -d ./process/SubProcesses ]; then 
    rm `ls ./process/SubProcesses/P*/*.inc | grep -v born_leshouche`
else  
    rm `ls ./process/madevent/SubProcesses/P*/*.inc | grep -v born_leshouche`
fi

