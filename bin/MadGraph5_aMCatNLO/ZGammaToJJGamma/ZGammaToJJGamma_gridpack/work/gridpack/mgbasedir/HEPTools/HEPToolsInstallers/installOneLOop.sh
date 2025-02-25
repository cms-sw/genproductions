#!/bin/bash

run () {

    ONELOOPINSTALLD="$1"
    VERSION="$2"
	TARBALLPATH="$3"
    mkdir -p $ONELOOPINSTALLD
    cd $ONELOOPINSTALLD
    echo "Decompressing OneLOop"
    tar xvzf ${TARBALLPATH}
	echo "Enter OneLOop directory and installing OneLOop"
    cd OneLOop-${VERSION}
	echo "Modding Config file to activate quadruple precision"
	if [ "$(uname)" == "Darwin" ]; then
	  sed -i '' 's;#QPKIND = 16;QPKIND = 16;g' Config
	  sed -i '' 's;FFLAGS = -O;FFLAGS = -O -fPIC;g' Config	  
	  sed -i '' 's;FC = gfortran;FC = '"$FC"';g' Config
    else
	  sed -i 's;#QPKIND = 16;QPKIND = 16;g' Config
	  sed -i 's;FFLAGS = -O;FFLAGS = -O -fPIC;g' Config	  
	  sed -i 's;FC = gfortran;FC = '"$FC"';g' Config
    fi
	$HEP_PYTHON ./create.py static
	echo "Copying the static library and modules generated to target directory"
	cp libavh_olo.a $ONELOOPINSTALLD
	cp *.mod $ONELOOPINSTALLD
    cd ..
    echo "Finished installing OneLOop"

}

run "$@"
