#!/bin/sh

# create a non AFS based temporary area

mkdir /tmp/wmclient_${USER}
cd /tmp/wmclient_${USER}

# download the WMAgent client 

svn co  svn+ssh://svn.cern.ch/reps/CMSDMWM/Infrastructure -r HEAD

cd Infrastructure/trunk/Deployment
./Deploy  -s prep /tmp/wmclient WMClient
./Deploy  -s sw /tmp/wmclient WMClient
./Deploy  -s post /tmp/wmclient WMClient

# setup the needed environment

source /tmp/wmclient_${USER}/projects/wmclient/etc/wmclient.sh

# run the upload script

./upload_configs.sh > configs.txt

./injectAndApprove.sh

exit 0
