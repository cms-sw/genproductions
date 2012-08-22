#!/bin/bash

sed 's/template/'$1'/g' bsubmit_template_gridpacks_mg5v1.4.8.sh > bsubmit_${1}_gridpacks.sh
echo done...you can now send the jobs

#source  bsubmit_${1}_gridpacks.sh



