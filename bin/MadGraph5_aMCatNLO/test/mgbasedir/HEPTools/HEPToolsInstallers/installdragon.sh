#!/bin/bash

set_environment () {

  echo " Set environment variables"

  # Here, define your installation paths, versions etc.
  gccversion="$(gcc -dumpversion)"
  INSTALLD="$1"
  VERSION="$2"
  TARBALL="$3"
  GSL="$4"
  FITSIO="$5"
  LOCAL=$INSTALLD

  # set SLC5 platform name:
  LCG_PLATFORM=i686
  if [[ "$(uname -m)" == "x86_64" ]] ; then
    LCG_PLATFORM=x86_64
  fi
}

run () {



  workd=$(pwd)

  echo " Unpack Dragon"
  unzip $TARBALL

  echo " Enter Dragon directory"
  cd DRAGON-master

  echo " Configure DRAGON"
  echo " with GSL $GSL"
  echo " with FITSIO $FITSIO"
  ./start.sh
if [ "$(uname -s)" == "Darwin" ]
then
  LD_LIBRARY_PATH=$GSL/lib:$FITSIO/lib:$LD_LIBRARY_PATH \
  ./configure --prefix=$LOCAL --with-cfitsio=$FITSIO --with-gsl-path=$GSL/bin \
  CC="gcc -arch x86_64" CXX="c++ -arch x86_64"
else
  LD_LIBRARY_PATH=$GSL/lib:$FITSIO/lib:$LD_LIBRARY_PATH \
  ./configure --prefix=$LOCAL --with-cfitsio=$FITSIO --with-gsl-path=$GSL/bin  --disable-openmp
fi
  echo " Compile DRAGON"
  make
  make install

  mkdir $LOCAL/output

  echo " Finished DRAGON installation"
  cd $workd

}

set_environment "$@"
run "$@"

