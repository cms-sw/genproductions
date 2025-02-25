#!/bin/bash

set_environment () {

  echo " Set environment variables"

  # Here, define your installation paths, versions etc.
  gccversion="$(gcc -dumpversion)"
  INSTALLD="$1"
  VERSION="$2"
  TARBALL="$3"
  CXXFLAGS="$4"
  LOCAL=$INSTALLD

  # set SLC5 platform name:
  LCG_PLATFORM=i686
  if [[ "$(uname -m)" == "x86_64" ]] ; then
    LCG_PLATFORM=x86_64
  fi
}

run () {

  workd=$(pwd)

  echo " Unpack YODA"
  tar xvzf $TARBALL



  echo " Enter directory"
  cd YODA-*/

  echo " Configure YODA"
  LIBRARY_PATH=$LD_LIBRARY_PATH ./configure CXXFLAGS="$CXXFLAGS" --prefix=$LOCAL --bindir=$LOCAL/bin --libdir=$LOCAL/lib --enable-static

  echo " Compile YODA"
  LIBRARY_PATH=$LD_LIBRARY_PATH make

  echo " Install YODA"
  LIBRARY_PATH=$LD_LIBRARY_PATH make install

  echo " Finished YODA installation"
  cd $workd

}

set_environment "$@"
run "$@"

