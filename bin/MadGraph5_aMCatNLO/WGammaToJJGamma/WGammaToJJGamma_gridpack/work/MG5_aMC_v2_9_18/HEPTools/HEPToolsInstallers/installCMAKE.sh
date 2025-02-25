#!/bin/bash

run () {
    CMAKEINSTALLD="$1"
	TARBALLPATH="$2"
	CONFIGOPTIONS="$3"
    mkdir -p $CMAKEINSTALLD
    cd $CMAKEINSTALLD
    echo "Decompressing CMAKE"
	mkdir cmake	
	tar xzvf ${TARBALLPATH} -C cmake --strip-components 1
	echo "Enter cmake directory and install cmake."	
    cd cmake
    configStr="./configure --prefix=$CMAKEINSTALLD $CONFIGOPTIONS"
    echo "$configStr"
    $configStr
	make
	make install
	cd ..
    echo "Finished installing cmake."
}

run "$@"
