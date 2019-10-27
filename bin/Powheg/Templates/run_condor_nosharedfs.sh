#!/bin/bash

export $$LHADF_DATA_PATH=${lhapdf_data_path}
if [ $$# -eq 0 ]; then
    ./pwhg_main
else
    echo $$1 | ./pwhg_main
fi

