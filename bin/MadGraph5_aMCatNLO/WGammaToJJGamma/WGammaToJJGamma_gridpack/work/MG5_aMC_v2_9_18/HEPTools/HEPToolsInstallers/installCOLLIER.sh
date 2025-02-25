#!/bin/bash

run () {

    ONELOOPINSTALLD="$1"
    VERSION="$2"
	TARBALLPATH="$3"
	CMAKEPATH="$4"	
    mkdir -p $ONELOOPINSTALLD
    cd $ONELOOPINSTALLD
	# Make sure there is only a single uncompressed folder starting with COLLIER-
	for a in COLLIER-*; do mv $a OLD_$a; done;
    echo "Decompressing COLLIER"
    tar xvzf ${TARBALLPATH}
	echo "Enter COLLIER directory and installing COLLIER"
    cd COLLIER-*
	if [ "$(uname)" == "Darwin" ]; then
#      sed -i '' 's;set (basic "-Dcollierdd -DSING");set (basic "${extra_fortran_flags} -Dcollierdd -DSING");g' CMakeLists.txt		
      sed -i '' 's;add_definitions(-Dcollierdd -DSING);add_definitions(-Dcollierdd -DSING -fPIC);g' CMakeLists.txt
	else
#	    sed -i 's;set (basic "-Dcollierdd -DSING");set (basic "${extra_fortran_flags} -Dcollierdd -DSING");g' CMakeLists.txt
	    sed -i 's;add_definitions(-Dcollierdd -DSING);add_definitions(-Dcollierdd -DSING -fPIC);g' CMakeLists.txt
    fi
    cd build
	${CMAKEPATH}/bin/cmake -Dstatic=ON -DCMAKE_Fortran_FLAGS=-fPIC ..
	make
	cd ..
	echo "Copying the static library and modules generated to target directory"
	cp libcollier.a $ONELOOPINSTALLD
	mkdir $ONELOOPINSTALLD/include
	cp modules/*.mod $ONELOOPINSTALLD/include/
    cd ..
    echo "Finished installing COLLIER"
}

run "$@"
