#!/bin/bash

set_environment () {

  echo " Set environment variables"

  INSTALLPATH="$1"  
  TARBALL="$2"
  HEPMC2PATH="$3"
  HEPMC2INCLUDEPATH=$HEPMC2PATH/include
  GZIPPATH="$4"
  MG5PATH="$5"
  CXXCOMMON="$6"
  OPTIONALDEPENDENCES="$7"


}

run () {

  workd=$(pwd)

  echo " >> Unpack PYTHIA8"
  tar xzf $TARBALL

  echo " >> Enter PYTHIA8 directory"
  local version="$(ls -1d pythia* | grep -v tgz | sed 's/pythia//g')"
  cd pythia${version}/

  if [ "$MG5PATH" != "None" ];
  then
      echo " >> copy plugin needed for FxFx"
      cp ${MG5PATH}/Template/NLO/MCatNLO/Scripts/JetMatching.h include/Pythia8Plugins/JetMatching.h
  fi
  
  echo " >> Configure PYTHIA8"
  make distclean
  configStr="./configure --prefix=$INSTALLPATH --with-hepmc2=$HEPMC2PATH --with-hepmc2-include=$HEPMC2INCLUDEPATH --with-gzip=$GZIPPATH $OPTIONALDEPENDENCES"  
  if [ "$CXXCOMMON" == "" ];
  then
	  ${configStr}
  else
	  ${configStr} --cxx-common="${CXXCOMMON}"
	  configStr='./configure '$configStr' --cxx-common='"${CXXCOMMON}"
  fi
  echo "$configStr"  
# Small fix for Pythia8.2 version. This is harmless to subsequent versions
  unamestr=`uname`
  echo $CXX
  if [[ "$unamestr" == 'Darwin' && "$CXX" != 'clang' ]]; then
	sed -i '' 's/-Qunused-arguments//g' Makefile.inc 
  fi

  echo " >> Compile PYTHIA8"
  make
  make install

#  echo " Compile PYTHIA8 examples"
#  cd $INSTALLPATH/share/Pythia8/examples
#  ls -1 main*.cc | while read line
#  do
#    make "$(echo "$line" | sed "s,\.cc,,g")"  
#  done

  #
  cd $INSTALLPATH/bin
  sed -e s/"if \[ \"\$VAR\" = \"LDFLAGS\" ]\; then OUT+=\" -ld\""/"if \[ \"\$VAR\" = \"LDFLAGS\" ]\; then OUT+=\" -ldl\""/g pythia8-config> pythia8-config.tmp
  mv pythia8-config.tmp pythia8-config
  chmod ug+x pythia8-config
  echo " Finished PYTHIA8 installation $workd"
  cd $workd/pythia${version}/
  cp Makefile.inc $INSTALLPATH

}

set_environment "$@"
echo "environment set"
run "$@"
