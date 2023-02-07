#!/bin/bash

run () {
    GINAC_MG5_INTERFACE_INSTALLD="$1"
	FOLDERPATH="$2"
	COMPILER="$3"
	HEPTOOLSD="$4"
	cp -R $FOLDERPATH/ $GINAC_MG5_INTERFACE_INSTALLD
    cd $GINAC_MG5_INTERFACE_INSTALLD/ginac_mg5_interface
    echo "Setting up ginac_mg5_interface"
    ./ginac_mg5_interface_compile.sh $COMPILER
    echo "Finished installing ginac_mg5_interface."
    echo "Linking the library"
    ln -s $GINAC_MG5_INTERFACE_INSTALLD/ginac_mg5_interface/ginac_mg5_interface.so $HEPTOOLSD/lib/ginac_mg5_interface.so
    echo "Symbolic link $HEPTOOLSD/lib/ginac_mg5_interface.so created"
   	cd ..
}

run "$@"
