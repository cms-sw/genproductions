#/bin/bash

# example usage:
# calculateXSectionAndFilterEfficiency.sh -f datasets.txt -c Moriond17 -d MINIAODSIM -n 1000000 (-m)
# documentation
# https://twiki.cern.ch/twiki/bin/viewauth/CMS/HowToGenXSecAnalyzer#Automated_scripts_to_compute_the

# To obtain CERN SSO credentials (necessary to read from McM):
#   ./getCookie.sh

FILE='datasets.txt'
CAMPAIGN='Moriond17'
DATATIER='MINIAODSIM'
EVENTS='1000000'
MCM=False

DEBUG=False
# DEBUG=True

while getopts f:c:d:n:m option
do
    case "${option}"
    in
            f) FILE=${OPTARG};;
            c) CAMPAIGN=${OPTARG};;
            d) DATATIER=${OPTARG};;
            n) EVENTS=${OPTARG};;
            m) MCM=True;;
    esac
done

while read -r dataset
do
    name="$dataset"
    echo "Name read from file - $name"
    
    echo 'compute_cross_section.py -f '${dataset}' -c '${CAMPAIGN}' -n '${EVENTS}' -d '${DATATIER}' --mcm "'${MCM}'" --debug "'${DEBUG}'"'
    output=$(python compute_cross_section.py -f ${dataset} -c ${CAMPAIGN} -n ${EVENTS} -d ${DATATIER} --mcm "${MCM}" --debug "${DEBUG}")
    output=${output#*.txt}
    
    if [ "${DEBUG}" != "True" ]; then
      # echo 'output '${output} > test.log
      eval ${output}
    else
      echo 'output'
      echo ${output}
      exit 1
    fi
    
done < "$FILE"


