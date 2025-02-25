#!/bin/bash

set_environment () {

  echo " Set environment variables"

  # Here, define your installation paths, versions etc.
  gccversion="$(gcc -dumpversion)"
  INSTALLD="$1"
  VERSION="$2"
  TARBALL="$3"
  RIVETPATH="$4"
  YODAPATH="$5"
  HEPMCPATH="$6"
  LOCAL=$INSTALLD
  PYVAR="$7"
  CXXFLAGS="$8"
  echo "PYVAR $PYVAR"
  echo "all $@"
  # set SLC5 platform name:
  LCG_PLATFORM=i686
  if [[ "$(uname -m)" == "x86_64" ]] ; then
    LCG_PLATFORM=x86_64
  fi
  mkdir $LOCAL/python$PYVAR
  mkdir $LOCAL/python$PYVAR/bin
  export PYTHONPATH=$PYTHONPATH:$LOCAL/python$PYVAR
  ln -s `which python$PYVAR` $LOCAL/python$PYVAR/bin/python
  export PATH=$LOCAL/python$PYVAR/bin/:$PATH

}

run () {

  workd=$(pwd)
  
  echo " Unpack CONTUR $PYVAR"
  tar xvzf $TARBALL
#  shopt -s expand_aliases
#  alias python=`which python$PYVAR`
#  shopt -s expand_aliases

  echo " Enter directory"
  mv contur-contur-${VERSION}/ $LOCAL/contur
  cd $LOCAL/contur

  echo " Check python dependency and install locally the missing one"
  
  for module in tqdm numpy scipy configobj matplotlib pyslha pandas contur
  do
      echo "module= $module"
      if  `python -c "import $module"`; then
          echo "working"
      else
         echo "use pip to install: pip install $module  --target=$LOCAL/python$PYVAR"
         pip install $module  --target=$LOCAL/python$PYVAR --upgrade

      fi
  done
  
#  echo " Compile CONTUR $PYVAR"
#  export PATH=$PATH:$YODAPATH/bin;
#  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$YODAPATH/lib:$YODAPATH/lib64
#  export PYTHONPATH=$PYTHONPATH:$YODAPATH/lib/python$PYVAR/site-packages
#  export PYTHONPATH=$PYTHONPATH:$YODAPATH/lib64/python$PYVAR/site-packages
#
#  export PATH=$PATH:$RIVETPATH/bin;
#  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$LD_LIBRARY_PATH:$RIVETPATH/lib:$RIVETPATH/lib64
#  export PYTHONPATH=$PYTHONPATH:$RIVETPATH/lib/python$PYVAR/site-packages
#  export PYTHONPATH=$PYTHONPATH:$RIVETPATH/lib64/python$PYVAR/site-packages
#
#  source setupContur.sh
#  export PATH=$LOCAL/bin:$PATH
#  
#  YODAPATH=$YODAPATH HEPMCPATH=$HEPMCPATH RIVETPATH=$RIVETPATH LIBRARY_PATH=$LD_LIBRARY_PATH PATH=$PATH  make
#
#  
#  echo " Install CONTUR"
#  bash setupContur.sh
#  #LIBRARY_PATH=$LD_LIBRARY_PATH make install

  echo " Finished CONTUR installation"
  cd $workd

}

set_environment "$@"
python() {
    `which python$PYVAR` "$@"
}
export -f python
pip(){
    `which pip$PYVAR` "$@"
}
export -f pip
run "$@"

