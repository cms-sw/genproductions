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

  echo " Unpack GSL"
  tar xvzf $TARBALL

  echo " Enter GSL directory"
  cd gsl-${VERSION}/

  echo " Configure GSL"
  ./configure --prefix=$LOCAL

  echo " Compile GSL"
  make
  make check
  LIBRARY_PATH=$LD_LIBRARY_PATH make

  echo " Install GSL"
  make install

  echo " Finished GSL installation"
  cd $workd

}

set_environment "$@"
run "$@"

