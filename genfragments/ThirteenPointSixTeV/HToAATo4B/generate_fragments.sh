#!/bin/bash

MASSES=(12 15 20 25 30 35 40 45 50 55 60)
PROCESSES=(
    "TTH_TTToAll_HToAATo4B"
    "WH_WToAll_HToAATo4B"
    "ZH_ZToAll_HToAATo4B"
    "VBFH_HToAATo4B"
    "GluGluH_01J_HToAATo4B"
    
)

TEMPLATE="template_fragment.py"

if [ ! -f "$TEMPLATE" ]; then
  echo "Template $TEMPLATE not found!"
  exit 1
fi

for PROC in "${PROCESSES[@]}"; do
  for MASS in "${MASSES[@]}"; do
    OUT="${PROC}_M-${MASS}_TuneCP5_13p6TeV-madgraph_pythia8-fragment.py"
    sed "s/PROCESS/${PROC}/g; s/MASS/${MASS}/g" "$TEMPLATE" > "$OUT"
    echo "Created $OUT"
  done
done
