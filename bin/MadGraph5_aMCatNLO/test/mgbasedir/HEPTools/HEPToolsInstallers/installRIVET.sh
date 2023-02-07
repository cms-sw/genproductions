#!/bin/bash

set_environment () {

  echo " Set environment variables"

  # Here, define your installation paths, versions etc.
  gccversion="$(gcc -dumpversion)"
  INSTALLD="$1"
  VERSION="$2"
  TARBALL="$3"
  YODAPATH="$4"
  HEPMCPATH="$5"
  FASTJETPATH="$6"
  CXXFLAGS="$7"
  LOCAL=$INSTALLD

  # set SLC5 platform name:
  LCG_PLATFORM=i686
  if [[ "$(uname -m)" == "x86_64" ]] ; then
    LCG_PLATFORM=x86_64
  fi
}

run () {

  workd=$(pwd)

  echo " Unpack rIVET"
  tar xvzf $TARBALL



  echo " Enter directory"
  cd Rivet-${VERSION}/

  echo " Configure RIVET"
  echo "LIBRARY_PATH=$LD_LIBRARY_PATH YODAPATH=$YODAPATH HEPMCPATH=$HEPMCPATH FASTJETPATH=$FASTJETPATH ./configure CXXFLAGS=$CXXFLAGS --prefix=$LOCAL --bindir=$LOCAL/bin --libdir=$LOCAL/lib --enable-static"
  
  LIBRARY_PATH=$LD_LIBRARY_PATH YODAPATH=$YODAPATH HEPMCPATH=$HEPMCPATH FASTJETPATH=$FASTJETPATH ./configure CXXFLAGS="$CXXFLAGS" --prefix=$LOCAL --bindir=$LOCAL/bin --libdir=$LOCAL/lib --enable-static 

  echo " Compile RIVET"
  LIBRARY_PATH=$LD_LIBRARY_PATH make

  echo " Install RIVET"
  LIBRARY_PATH=$LD_LIBRARY_PATH make install

  echo " Finished RIVET installation"
  cd $workd

}

set_environment "$@"
run "$@"

