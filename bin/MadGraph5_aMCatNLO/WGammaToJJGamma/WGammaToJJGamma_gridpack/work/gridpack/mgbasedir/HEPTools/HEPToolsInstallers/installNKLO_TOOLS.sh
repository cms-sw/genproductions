#!/bin/bash

run () {
    NKLO_TOOLS_INSTALLD="$1"
    cd $NKLO_TOOLS_INSTALLD
    make
   	cd ..
}

run "$@"
