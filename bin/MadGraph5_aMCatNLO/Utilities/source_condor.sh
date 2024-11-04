#!/bin/bash

# HTCondor python bindings are lost after cmsenv/scram
#PYTHON_BINDINGS="$(python3 -c 'import htcondor; import os; print(os.path.dirname(htcondor.__path__[0]))' 2>/dev/null)"
#if [ -z "$PYTHON_BINDINGS" ]; then
export _condor_SCHEDD_HOST=bigbird17.cern.ch
export _condor_CREDD_HOST=bigbird17.cern.ch 

if [ -d $CMSSW_BASE/venv ];
then 
  echo "venv exists"
else
  scram-venv
  cmsenv
  pip3 install htcondor --upgrade #FIXME need better way to interface HTCondor Python API for Python3.9
fi
