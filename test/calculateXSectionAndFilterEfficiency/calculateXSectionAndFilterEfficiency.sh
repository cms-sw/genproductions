#/bin/bash

# example usage:
# launch_cmsrun.sh -f datasets.txt -c Moriond17 -d MINIAODSIM -n 1000000

FILE='datasets.txt'
CAMPAIGN='Moriond17'
DATATIER='MINIAODSIM'
EVENTS='1000000'

DEBUG="False"

while getopts f:c:d:n option
do
        case "${option}"
        in
                f) FILE=${OPTARG};;
                c) CAMPAIGN=${OPTARG};;
                d) DATATIER=${OPTARG};;
                n) EVENTS=${OPTARG};;
        esac
done

while read -r dataset
do
    name="$dataset"
    echo "Name read from file - $name"
    
    echo 'compute_cross_section.py -f '${dataset}' -c '${CAMPAIGN}' -n '${EVENTS}' -d '${DATATIER}
    output=$(python compute_cross_section.py -f ${dataset} -c ${CAMPAIGN} -n ${EVENTS} -d ${DATATIER} --debug ${DEBUG})
    # echo 'output:'
    # echo ${output}
    
    if [ "${DEBUG}" != "True" ]; then
      eval ${output}
    else
      exit 1
    fi
    
done < "$FILE"


