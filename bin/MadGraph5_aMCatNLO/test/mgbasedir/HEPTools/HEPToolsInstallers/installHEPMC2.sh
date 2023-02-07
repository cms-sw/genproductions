#!/bin/bash

set_environment () {

  echo " Set environment variables"

  # Here, define your installation paths, versions etc.
  #INSTALLD=/nfs/farm/g/theory/qcdsim/sp/HEP/HEPMC2
  INSTALLD="$1"
  VERSION="$2"
  TARBALL="$3"
#  USR=/usr
#  LOCAL=$USR/local/
#  export LD_LIBRARY_PATH=/usr/lib:/usr/lib64:$LOCAL/lib:$LOCAL/64/lib:$LOCAL/lib64:$LD_LIBRARY_PATH

# Detect the path of this script which will be used to retrievd the hacked HEPMC files WeightContainer.cc and WeightContainer.h
  pushd `dirname $0` > /dev/null
  CURRDIR=`pwd -P`
  popd > /dev/null

  # set SLC5 platform name:
  LCG_PLATFORM=i686
  if [[ "$(uname -m)" == "x86_64" ]] ; then
    LCG_PLATFORM=x86_64
  fi


  # Set flag to correct compiler, good if more than one compiler is available.
  # Script will also work with this commented !change!
#  export LDFLAGS="-L=/usr/local/lib/gcc/i686-pc-linux-gnu/4.3.2"
#  export LDFLAGS="-L=/usr/lib64/gcc/x86_64-suse-linux/4.5"

#  version="2.06.09"

}

run () {
  echo " Unpack HEPMC"
  tar xvzf ${TARBALL}

  echo " Enter HEPMC directory"
  cd hepmc${VERSION}/

  echo " Copying the hacked files 'WeightContainer.cc' and 'WeightContainer.h' to be capable of writing named weights in HepMC."
  cp ${CURRDIR}/WeightContainer.cc src/WeightContainer.cc
  cp ${CURRDIR}/WeightContainer.h HepMC/WeightContainer.h

  if [ -f ./configure ]; then
   echo " The autotools configure script already present; there is no need to run autoreconf manually."
  else
   echo " The autotools configure script is missing in HEPMC standard distribution."
   echo " This installer tool will attempt to generate it automatically but it will fail if 'autoreconf' is not available on your system (in which case you should install it)."
   autoreconf -i
  fi

  echo " Configure HEPMC"
  configStr="./configure --prefix=$INSTALLD --with-momentum=GEV --with-length=MM"
  echo "$configStr"
  $configStr

  echo " Compile HEPMC"
  make

  echo " Install HEPMC"
  make install

  echo " Finished HEPMC installation"
  cd ..

  cd $INSTALLD
  if [ -d lib ]; then
      echo "test passed"
  else
      ln -s lib64 lib
  fi
}

set_environment "$@"
run "$@"

