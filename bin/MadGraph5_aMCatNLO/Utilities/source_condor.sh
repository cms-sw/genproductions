#!/bin/bash

# HTCondor python bindings are lost after cmsenv/scram
PYTHON_BINDINGS="$(python3 -c 'import htcondor; import os; print(os.path.dirname(htcondor.__path__[0]))' 2>/dev/null)"
if [ -z "$PYTHON_BINDINGS" ]; then
  pip3 install htcondor --user #FIXME need better way to interface HTCondor Python API for Python3.9
fi
