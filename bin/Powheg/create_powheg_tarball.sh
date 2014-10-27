#!/bin/bash

fail_exit() { echo "$@" 1>&2; exit 1; }

#set -o verbose
EXPECTED_ARGS=7

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` source_repository source_tarball_name process card tarballName Nevents RandomSeed"
    echo "Example: `basename $0` slc6_amd64_gcc481/powheg/V1.0/src powhegboxv1.0_Oct2013 Z slc6_amd64_gcc481/powheg/V1.0/8TeV_Summer12/DYToEE_M-20_8TeV-powheg/v1/DYToEE_M-20_8TeV-powheg.input Z_local 1000 1212" 
    exit 1
fi

echo "   ______________________________________________________    "
echo "         Running Powheg  create_powheg_tarball.sh            "
echo "   ______________________________________________________    "

repo=${1}
echo "%MSG-POWHEG source repository = $repo"

name=${2} 
echo "%%MSG-POWHEG source tarball name = $name"

process=${3}
echo "%MSG-POWHEG process = $process"

cardinput=${4}
echo "%MSG-POWHEG location of the card = $cardinput"

tarball=${5}
echo "%MSG-POWHEG tar ball file name = ${tarball}_tarball.tar.gz"

nevt=${6}
echo "%MSG-POWHEG number of events requested = $nevt"

rnum=${7}
echo "%MSG-POWHEG random seed used for the run = $rnum"

seed=$rnum
file="events"

# Release to be used to define the environment and the compiler needed
export RELEASE=${CMSSW_VERSION}
export WORKDIR=`pwd`

# initialize the CMS environment 
if [[ -e ${name} ]]; then
  echo -e "The directory ${name} exists! Move the directory to old_${name}\n"
  mv ${name} old_${name}
  mv output.lhe old_output.lhe
  rm -rf ${myDir}
  echo -e "Move the tar ball to old_${tarball}.tar.gz\n"
  mv ${tarball}_tarball.tar.gz old_${tarball}_tarball.tar.gz
fi

scram project -n ${name} CMSSW ${RELEASE}; cd ${name} ; mkdir -p work ; 
eval `scram runtime -sh`
cd work
export PATH=`pwd`:${PATH}
 
# LHAPDF
# lhapdf-config comes with the paths used at build time.
# we need this to replace with the correct paths obtained from scram tool info fastjet
 
newinstallationdirlha=`scram tool info lhapdf | grep LHAPDF_BASE |cut -d "=" -f2`
cp ${newinstallationdirlha}/bin/lhapdf-config ./lhapdf-config.orig
oldinstallationdirlha=`cat lhapdf-config.orig | grep prefix | head -n 1 | cut -d"=" -f2`
sed -e "s#prefix=${oldinstallationdirlha}#prefix=${newinstallationdirlha}#g" lhapdf-config.orig > lhapdf-config

# if LHAPDF6 exists, use this
LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
if [ -e $LHAPDF6TOOLFILE ]; then
   LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
   cp $LHAPDFCONFIG .
fi

chmod +x lhapdf-config

#
## Get the input card
wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${cardinput} -O powheg.input  || cp -p ${cardinput} powheg.input || fail_exit "Failed to get powheg input card " ${card}

myDir=`pwd`
card=${myDir}/powheg.input


### retrieve the powheg source tar ball
wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${repo}/${name}.tar.gz  -O ${name}.tar.gz || fail_exit "Failed to get powheg tar ball " ${name}
tar xzf ${name}.tar.gz
#
cd POWHEG-BOX/${process}

### Corrections needed

# This is just to please gcc 4.8.1
mkdir -p include

# Use dynamic linking and lhapdf
mv Makefile Makefile.orig
cat Makefile.orig | sed -e "s#STATIC[ \t]*=[ \t]*-static#STATIC=-dynamic#g" | sed -e "s#PDF[ \t]*=[ \t]*native#PDF=lhapdf#g" > Makefile

#Use gfortran, not other compilers which are not free/licensed
mv Makefile Makefile.interm
cat Makefile.interm | sed -e "s#COMPILER[ \t]*=[ \t]*ifort#COMPILER=gfortran#g" > Makefile

# Find proper histo booking routine (two of them exist)
BOOK_HISTO="pwhg_bookhist-multi.o"
if [ `echo ${name} | cut -d "_" -f 1` = "powhegboxV1" ]; then
   BOOK_HISTO="pwhg_bookhist.o"
fi 
if [ "$process" = "trijet" ]; then 
   BOOK_HISTO+=" observables.o"
fi  

# Remove ANY kind of analysis with parton shower
if [ `grep particle_identif pwhg_analysis-dummy.f` = ""]; then
   cp ../pwhg_analysis-dummy.f .
fi
mv Makefile Makefile.interm
cat Makefile.interm | sed -e "s#PWHGANAL[ \t]*=[ \t]*#\#PWHGANAL=#g" | sed -e "s#ANALYSIS[ \t]*=[ \t]*#\#ANALYSIS=#g" | sed -e "s#LHAPDF_CONFIG[ \t]*=[ \t]*#\#LHAPDF_CONFIG=#g" > Makefile
mv Makefile Makefile.interm
cat Makefile.interm | sed -e "s#pwhg_bookhist.o# #g" | sed -e "s#pwhg_bookhist-new.o# #g" | sed -e "s#pwhg_bookhist-multi.o# #g" > Makefile
if [ "$process" = "ttJ" ]; then
  mv Makefile Makefile.interm
  cat Makefile.interm | sed -e "s#_PATH) -L#_PATH) #g" | sed -e "s# -lvirtual#/libvirtual.so.1.0.0#g" > Makefile
fi
  
echo "ANALYSIS=none 
PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o
LHAPDF_CONFIG=${myDir}/lhapdf-config" >> tmpfile
mv Makefile Makefile.interm
cat tmpfile Makefile.interm > Makefile
rm -f Makefile.interm tmpfile

# Add libraries
echo "LIBS+=-lz -lstdc++" >> Makefile

## ???
#LHA_BASE="`readlink -f "$LHAPATH/../../../"`"
#LHA_BASE_OLD="`$LHA_BASE/bin/lhapdf-config --prefix`"
#cat > lhapdf-config-wrap <<EOF
##!/bin/bash
#"$LHA_BASE/bin/lhapdf-config" "\$@" | sed "s|$LHA_BASE_OLD|$LHA_BASE|g"
#EOF

#chmod a+x lhapdf-config-wrap

#make LHAPDF_CONFIG="`pwd`/lhapdf-config-wrap" pwhg_main || fail_exit "Failed to compile pwhg_main"
make pwhg_main || fail_exit "Failed to compile pwhg_main"

mkdir workdir
cd workdir
localDir=`pwd`

if [ -e  ${WORKDIR}/vbfnlo.input ]; then
    cp -p ${WORKDIR}/vbfnlo.input .
fi 

cat ${card} | sed -e "s#SEED#${seed}#g" | sed -e "s#NEVENTS#${nevt}#g" > powheg.input
cat powheg.input
if [[ -e ../pwhg_main-gnu ]]; then
  mv ../pwhg_main-gnu ../pwhg_main
  chmod a+x ../pwhg_main
fi
../pwhg_main &> log_${process}_${seed}.txt

#remove the spurious random seed output that is non LHE standard 
cat pwgevents.lhe | grep -v "Random number generator exit values" > ${file}_final.lhe
ls -l ${file}_final.lhe
pwd
cp ${file}_final.lhe ${WORKDIR}/.

myDir=powhegbox_${process}
mkdir ${WORKDIR}/${myDir}
cp -p ../pwhg_main ${WORKDIR}/${myDir}/.
cp -p pwg*.dat ${WORKDIR}/${myDir}/.
if [ -e  ${WORKDIR}/vbfnlo.input ]; then
    cp -p ${WORKDIR}/vbfnlo.input ${WORKDIR}/${myDir}/.
fi

cd ${WORKDIR}/${myDir}
cp -p ${card} .

if [ ! -e  ${WORKDIR}/runcmsgrid_powheg.sh ]; then
 fail_exit "Did not find " ${WORKDIR}/runcmsgrid_powheg.sh 
fi

sed -e 's/PROCESS/'${process}'/g' ${WORKDIR}/runcmsgrid_powheg.sh > runcmsgrid.sh
chmod 755 runcmsgrid.sh
tar cpzsf ${tarball}_tarball.tar.gz *
mv ${tarball}_tarball.tar.gz ${WORKDIR}/.
cd ${WORKDIR}
tar tvf ${tarball}_tarball.tar.gz
rm -rf ${myDir}

echo "Tarball ${tarball}_tarball.tar.gz ready with log_${process}_${seed}.txt and ${file}_final.lhe at ${localDir}"
echo "End of job on " `date`
exit 0;
