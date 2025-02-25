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

  echo " Unpack fastjet contrib"
  tar xvzf $TARBALL

  echo " Enter fastjet contrib directory"
  cd fjcontrib-$2/

  echo " Configure fastjet contrib"
  ./configure --fastjet-config=$LOCAL/bin/fastjet-config 

  echo " Compile fastjet contrib"
  make fragile-shared-install

  echo " Install fastjet contrib"
  make install

  echo " Finished fastjet contrib installation"
  cd $workd

}

set_environment "$@"
run "$@"

