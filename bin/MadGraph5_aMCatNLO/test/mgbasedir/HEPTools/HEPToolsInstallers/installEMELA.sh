#!/bin/bash

run () {

    INSTALLD="$1"
    VERSION="$2"
    TARBALLPATH="$3"
    CMAKEPATH="$4"
    LHAPDFPATH="$5"
    BOOSTPATH="$6"

    echo "link lhadf $5"
    echo "link boost $6"
    mkdir -p $INSTALLD
    cd $INSTALLD
    # Make sure there is only a single uncompressed folder starting with EMELA-
    for a in `ls -d eMELA-*`; do mv $a OLD_${a}_`date '+%Y%m%d%H%M%S'`; done;
    echo "Decompressing EMELA"
    tar xvzf ${TARBALLPATH}
    echo "Enter EMELA directory and installing EMELA"
    cd eMELA-*
    mkdir build
    cd build
    ${CMAKEPATH}/bin/cmake .. -DCMAKE_INSTALL_PREFIX=$INSTALLD -DWITH_LHAPDF=ON -DCMAKE_CXX_FLAGS="-L $LHAPDFPATH/lib/ -lLHAPDF -I $LHAPDFPATH/include -I $BOOSTPATH" #-DCMAKE_VERBOSE_MAKEFILE=ON

    make && make install
    cd ..
    echo "Finished installing EMELA"
}

run "$@"
