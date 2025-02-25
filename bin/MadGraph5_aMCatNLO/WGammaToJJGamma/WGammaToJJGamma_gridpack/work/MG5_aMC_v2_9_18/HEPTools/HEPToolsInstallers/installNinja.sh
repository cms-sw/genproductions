#!/bin/bash

run () {

    NINJAINSTALLD="$1"
	TARBALLPATH="$2"
    ONELOOPPATH="$3"
    COMPILATIONFLAGS="$4"
	CPPSTANDARDLIB="$5"
	mkdir -p $NINJAINSTALLD
    cd $NINJAINSTALLD
    echo "Decompressing Ninja"
	mkdir Ninja
	tar xzvf ${TARBALLPATH} -C Ninja --strip-components 1
	echo "Entering Ninja directory and installing Ninja"
    cd Ninja
    if [ -f VERSION ]; then
        mv VERSION TEMPVERSION
    fi
    if [ "$(uname)" == "Darwin" ]; then
	./configure --prefix=${NINJAINSTALLD} --enable-higher_rank --with-avholo='-L'"${ONELOOPPATH}"' -lavh_olo' FCINCLUDE=-I${ONELOOPPATH} CXX=${CXX} CXXFLAGS="${COMPILATIONFLAGS}" CPPFLAGS='-DNINJA_NO_EXCEPTIONS -fPIC' --disable-quadninja LIBS=${CPPSTANDARDLIB}
    else
	./configure --prefix=${NINJAINSTALLD} --enable-higher_rank --with-avholo='-L'"${ONELOOPPATH}"' -lavh_olo' FCINCLUDE=-I${ONELOOPPATH} CXX=${CXX} CXXFLAGS="${COMPILATIONFLAGS}" CPPFLAGS='-DNINJA_NO_EXCEPTIONS -fPIC' --enable-quadninja LIBS=${CPPSTANDARDLIB}
    fi
    
	echo "== Content of config.log =="
	cat config.log
	echo "== End of content of config.log =="
	make
	make install
    if [ -f TEMPVERSION ]; then
        mv TEMPVERSION VERSION
    fi
    cd ..
    cd ${NINJAINSTALLD}
    ./bin/ninja-config --version > VERSION
    ./bin/ninja-config --version > Ninja/VERSION
    echo "Finished installing Ninja"
}

run "$@"
