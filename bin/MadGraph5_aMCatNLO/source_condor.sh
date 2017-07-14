#! /bin.bash

# HTCondor python bindings are lost after cmsenv/scram
# unless PYTHONPATH is set including its location
# Workaround: Include original location in the path
# if not there already.
PYTHON_BINDINGS="$(python -c 'import htcondor; import os; print os.path.dirname(htcondor.__file__)' 2>/dev/null)"
if [ -z "$PYTHON_BINDINGS" ]; then
    echo "Error: Could not find htcondor python binding (htcondor.so), please include the directory in PYTHONPATH."
    exit 1
else
  if [ -n "$PYTHONPATH" ]; then
    if [ -n "${PYTHONPATH##*${PYTHON_BINDINGS}*}" ]; then
      export PYTHONPATH="$PYTHON_BINDINGS:$PYTHONPATH"
    fi
  else
      export PYTHONPATH="$PYTHON_BINDINGS"
  fi
fi
