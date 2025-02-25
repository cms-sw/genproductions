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
    mv VERSION TEMPVERSION
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
    mv TEMPVERSION VERSION
    cd ..
    echo "Finished installing Ninja"
}

run "$@"
