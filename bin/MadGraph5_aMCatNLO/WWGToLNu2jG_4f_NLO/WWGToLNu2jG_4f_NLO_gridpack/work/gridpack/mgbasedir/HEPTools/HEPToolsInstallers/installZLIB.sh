#!/bin/bash

set_environment () {

  echo " Set environment variables"

  INSTALLD="$1"
  VERSION="$2"
  TARBALL="$3"

  # set SLC5 platform name:
  LCG_PLATFORM=i686
  if [[ "$(uname -m)" == "x86_64" ]] ; then
    LCG_PLATFORM=x86_64
  fi


}

run () {
  echo " Unpack zlib"
  tar xvzf ${TARBALL}

  echo " Enter zlib directory"
  cd zlib-${VERSION}/

  echo " Configure zlib"
  ./configure --prefix=$INSTALLD

  echo " Compile zlib"
  make

  echo " Install zlib"
  make install

  echo " Finished zlib installation"
  cd ..

}

set_environment "$@"
run "$@"
