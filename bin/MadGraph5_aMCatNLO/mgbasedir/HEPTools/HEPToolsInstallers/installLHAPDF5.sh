#!/bin/bash

set_environment () {

  echo " Set environment variables"

  # Here, define your installation paths, versions etc.
  gccversion="$(gcc -dumpversion)"
  INSTALLD="$1"
  VERSION="$2"
  TARBALL="$3"
  LOCAL=$INSTALLD

  # set SLC5 platform name:
  LCG_PLATFORM=i686
  if [[ "$(uname -m)" == "x86_64" ]] ; then
    LCG_PLATFORM=x86_64
  fi
}

run () {

  workd=$(pwd)

  echo " Unpack LHAPDF5"
  tar xvzf $TARBALL

  echo " Enter LHAPDF directory"
  cd lhapdf-${VERSION}/

  echo " Configure LHAPDF5"
  LIBRARY_PATH=$LD_LIBRARY_PATH ./configure CXXFLAGS="-static-libstdc++" --prefix=$LOCAL --bindir=$LOCAL/bin --datadir=$LOCAL/share --libdir=$LOCAL/lib --enable-static --disable-octave

  echo " Compile LHAPDF5"
  LIBRARY_PATH=$LD_LIBRARY_PATH make

  echo " Install LHAPDF5"
  LIBRARY_PATH=$LD_LIBRARY_PATH make install

  echo "copy index and conf file"
  cd $INSTALLD
  index="$(find . -name 'PDFsets.index')"
  cp $index $INSTALLD/share/LHAPDF/

  echo " Finished LHAPDF5 installation"
  cd $workd

}

set_environment "$@"
run "$@"

