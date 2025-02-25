#!/bin/bash

set_environment () {

  echo " Set environment variables"

  INSTALLD="$1"
  VERSION="$2"
  TARBALL="$3"

}

run () {

  wget $TARBALL
  tar xvzf ${TARBALL}
  cd HepMC3-${VERSION}
  cmake -DHEPMC3_ENABLE_ROOTIO=OFF -DHEPMC3_ENABLE_PYTHON=OFF -DCMAKE_INSTALL_PREFIX=$INSTALLD ./

  make -j8 install

  cd $INSTALLD
  if [ -d lib ]; then
      echo "test passed"
  else
      ln -s lib64 lib
  fi
}

set_environment "$@"
run "$@"

