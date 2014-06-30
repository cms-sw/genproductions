#!/bin/bash

find ./ -name "*.lhe" | xargs -r rm
find ./ -name "*.lhe.gz" | xargs -r rm
find ./ -name "*.f" | xargs -r rm
find ./ -name "*.F" | xargs -r rm
find ./ -name "*.cc" | xargs -r rm
find ./ -name "*.html" | xargs -r rm
find ./ -name "*.jpg" | xargs -r rm
find ./ -name "*.ps" | xargs -r rm
find ./ -name "*.lhe" | xargs -r rm
find ./ -name "gensym" | xargs -r rm
find ./ -name "ftn25" | xargs -r rm
find ./ -name "ftn26" | xargs -r rm
find ./ -wholename "*SubProcesses/*/*.o" | xargs -r rm
