#!/bin/bash

if [  $# -ne 0  ]  ;then
	echo
	echo ./bsubmit_xxxxx.sh
	echo 
	return
fi
#__________________________
name="template"

user=`whoami`
space=`echo ${user:0:1}`
AFSFOLD=/afs/cern.ch/user/$space/$user/gridpack_logs_148
queue=1nw
queue=1nd
#__________________________

if [  ! -d $AFSFOLD/${name} ] ;then
	mkdir -p ${AFSFOLD}/${name}
fi

Output=${AFSFOLD}/${name}/${name}_gridpack_148.log
Error=${AFSFOLD}/${name}/error_${name}_148.txt

bsub -q ${queue} -R "swp>200&&pool>500&&cpuf > 3.0"  -J ${queue}_template -eo $Error -oo $Output -N create_gridpack_148.sh  ${name} ${queue}
echo bsub -q ${queue} -R "swp>200&&pool>500&&cpuf > 3.0"  -J ${queue}_template -eo $Error -oo $Output -N create_gridpack_148.sh  ${name} ${queue}
