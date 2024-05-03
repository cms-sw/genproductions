#!/bin/env bash

cards=( *.DAT )
numberofcards=${#cards[@]}
done=false

while [ $done = false ]; do if [ $(ls| grep ".tgz") ] && [ ${numberofcards} -eq $(ls -d ".tgz"|wc -l) ]; then done=true; else sleep 2m; fi; done

echo "All gridpacks are produced. Please check directory $PWD" | -mail "MCFM+JHUGen Gridpacks production done" "carolhungwt@gmail.com" 
