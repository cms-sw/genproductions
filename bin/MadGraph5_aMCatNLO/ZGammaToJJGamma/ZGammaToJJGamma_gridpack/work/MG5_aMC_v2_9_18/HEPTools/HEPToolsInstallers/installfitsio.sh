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
#if [ $(UNAME) = "Darwin" ]
#then
#   if [ -n `which brew`]
#   then
#       brew install cfitsio
#   fi
#   if [ -n `which port`]
#   then
#       port install cfitsio
#   fi
#fi
  workd=$(pwd)

  echo " Unpack fitsio"
  tar xvzf $TARBALL

  echo " Enter fitsio directory"
  cd cfitsio/

  echo " Configure fitsio"
  ./configure --prefix=$LOCAL

  echo " Compile fitsio"
  make 

  echo " Install fitsio"
  make install

  echo " Compile fitsio shared"
  make shared

  echo " Install fitsio"
  make install


  echo " Finished fitsio installation"
  cd $workd

}

set_environment "$@"
run "$@"

