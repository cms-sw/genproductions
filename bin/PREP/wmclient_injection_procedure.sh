#!/bin/sh

set -o verbose 

# get the input scripts 

export INPUT=${1}
export NAME=`basename ${INPUT} | cut -f 1 -d'.'`
export RELEASE=${2}

# create a non AFS based temporary area

mkdir /tmp/wmclient_${USER}
cd /tmp/wmclient_${USER}

# download the WMAgent client 

svn co  svn+ssh://svn.cern.ch/reps/CMSDMWM/Infrastructure/trunk/Deployment

cd Deployment
./Deploy  -s prep -t v01 /tmp/wmclient_${USER} wmclient
./Deploy  -s sw -t v01 /tmp/wmclient_${USER} wmclient
./Deploy  -s post -t v01 /tmp/wmclient_${USER} wmclient

# setup the needed environment

cd /tmp/wmclient_${USER}
scram project CMSSW ${RELEASE}
cd ${RELEASE} ; mkdir work ; cd work
eval `scram runtime -sh`

source /tmp/wmclient_${USER}/v01/etc/wmclient.sh

# get the scripts to be run

cp ${INPUT} . ; tar xvzf `basename ${INPUT}` ; cd ${NAME}
ls -l

# run the upload script

./upload_configs.sh > configs.txt

./injectAndApprove.sh

exit 0
