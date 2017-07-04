#!/bin/bash

# Script to create SSO cookie file with CERN permissions
# Run in clean environement because something in cmsenv causes it to fail
#
# Usage: ./getCookie.sh

env -i KRB5CCNAME="$KRB5CCNAME" cern-get-sso-cookie -u https://cms-pdmv.cern.ch/mcm/ -o ~/private/prod-cookie.txt --krb -r
env -i KRB5CCNAME="$KRB5CCNAME" cern-get-sso-cookie -u https://cms-pdmv-int.cern.ch/mcm/ -o ~/private/int-cookie.txt --krb -r 
env -i KRB5CCNAME="$KRB5CCNAME" cern-get-sso-cookie -u https://cms-pdmv-dev.cern.ch/mcm/ -o ~/private/dev-cookie.txt --krb -r

