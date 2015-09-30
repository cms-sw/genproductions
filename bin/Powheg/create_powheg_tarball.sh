#!/bin/bash

fail_exit() { echo "$@" 1>&2; exit 1; }

#set -o verbose
EXPECTED_ARGS=5

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` process card othercard Nevents RandomSeed"
    echo "Example: `basename $0` Z slc6_amd64_gcc481/powheg/V1.0/8TeV_Summer12/DYToEE_M-20_8TeV-powheg/v1/DYToEE_M-20_8TeV-powheg.input none 1000 1212" 
    exit 1
fi

echo "   ______________________________________________________    "
echo "         Running Powheg  create_powheg_tarball.sh            "
echo "   ______________________________________________________    "

repo=slc6_amd64_gcc481/powheg/V2.0/src
echo "%MSG-POWHEG source repository = $repo"

name=powhegboxV2_Sep2015
echo "%MSG-POWHEG source tarball name = $name"

process=${1}
echo "%MSG-POWHEG process = $process"

cardinput=${2}
echo "%MSG-POWHEG location of the card = $cardinput"

tarball=${process}
echo "%MSG-POWHEG tar ball file name = ${tarball}_tarball.tar.gz"

usejhugen=${3}
echo "%MSG-POWHEG JHUGen datacard for decays = ${usejhugen}"

nevt=${4}
echo "%MSG-POWHEG number of events requested = $nevt"

rnum=${5}
echo "%MSG-POWHEG random seed used for the run = $rnum"

skipgen=${6}
echo "%MSG-POWHEG if not null skip generation = $skipgen"

seed=$rnum
file="events"
jhugenversion="v5.2.5"

temp1=${cardinput%%.input*}
temp2=${temp1##*/}
jobfolder=${name}_${temp2}_${tarball}

echo "%MSG-POWHEG creating sub work directory ${jobfolder}"

# Release to be used to define the environment and the compiler needed
export RELEASE=${CMSSW_VERSION}
export WORKDIR=`pwd`


# initialize the CMS environment 
if [[ -e ${jobfolder} ]]; then
  fail_exit "The directory ${jobfolder} exists! Please clean up your work directory before running!!"
fi

if [[ -e ${tarball}_tarball.tar.gz ]]; then
  fail_exit "The tarball ${tarball}_tarball.tar.gz exists! Please rename it or move it somewhere else before running!!"
fi

if [[ -e events_final.lhe ]]; then
  fail_exit "The LHE file events_final.lhe exists! Please remove this file before running!!"
fi

scram project -n ${jobfolder} CMSSW ${RELEASE}; cd ${jobfolder} ; mkdir -p work ; 
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

## Get the input card
wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${cardinput} -O powheg.input  || cp -p ${cardinput} powheg.input || fail_exit "Failed to get powheg input card " ${card}

mv powheg.input powheg.input.temp
cat powheg.input.temp | sed -e "s#--#-#g" > powheg.input

myDir=`pwd`
card=${myDir}/powheg.input
cardj=${myDir}/JHUGen.input

## Try to get the JHUGen card
wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${usejhugen} -O JHUGen.input || cp -p ${usejhugen} JHUGen.input

jhugen=0
if [[ -s ./JHUGen.input ]]; then
  jhugen=$(expr $jhugen + 1)
  echo "JHUGen activated!"
fi

### retrieve the powheg source tar ball
wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${repo}/${name}.tar.gz  -O ${name}.tar.gz || fail_exit "Failed to get powheg tar ball " ${name}
tar xzf ${name}.tar.gz
#
patch -l -p0 -i ${WORKDIR}/patches/pdfweights.patch
cd POWHEG-BOX
tar xvzf ${process}.tgz
cd ${process}

# This is just to please gcc 4.8.1
mkdir -p include

# Use dynamic linking and lhapdf
mv Makefile Makefile.orig
cat Makefile.orig | sed -e "s#STATIC[ \t]*=[ \t]*-static#STATIC=-dynamic#g" | sed -e "s#PDF[ \t]*=[ \t]*native#PDF=lhapdf#g" > Makefile

# Use gfortran, not other compilers which are not free/licensed
mv Makefile Makefile.interm
cat Makefile.interm | sed -e "s#COMPILER[ \t]*=[ \t]*ifort#COMPILER=gfortran#g" > Makefile

# Find proper histo booking routine (two of them exist)
BOOK_HISTO="pwhg_bookhist-multi.o"
if [ `echo ${name} | cut -d "_" -f 1` = "powhegboxV1" ]; then
   BOOK_HISTO="pwhg_bookhist.o"
fi 
if [ "$process" = "trijet" ]; then 
   BOOK_HISTO+=" observables.o"
   rm -rf ../progress/bbinit.f
fi  
if [ "$process" = "VBF_HJJJ" ]; then 
  mv pwhg_analysis-dummy.f pwhg_analysis-dummy.f.orig
  sed 's/..\/pwhg_book.h/pwhg_book.h/g' pwhg_analysis-dummy.f.orig > pwhg_analysis-dummy.f
fi  
if [ "$process" = "VBF_H" ]; then 
  sed -i '/pwhginihist/d' pwhg_analysis-dummy.f 
fi  
if [ "$process" = "Wgamma" ] || [ "$process" = "W_ew-BMNNP" ]; then
    patch -l -p0 -i ${WORKDIR}/patches/pwhg_analysis_driver.patch 
fi
if [ "$process" = "ttb_NLO_dec" ]; then
    patch -l -p0 -i ${WORKDIR}/patches/pwhg_analysis_driver_offshellmap.patch
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
if [ "$process" = "ttH" ]; then
    sed -i 's/O2/O0/g' Makefile
    sed -i 's/4.5d0/4.75d0/g' init_couplings.f
fi
if [ "$process" = "gg_H_MSSM" ]; then 
  mv nloreal.F nloreal.F.orig
  sed 's/leq/le/g' nloreal.F.orig > nloreal.F
  cp -p ../gg_H_quark-mass-effects/SLHA.h .
  cp -p ../gg_H_quark-mass-effects/SLHADefs.h .
fi  
  
echo "ANALYSIS=none " >> tmpfile
if [ "$process" = "Wgamma" ]; then
    echo "PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o uti.o " >> tmpfile
else
    echo "PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o " >> tmpfile
fi
echo "LHAPDF_CONFIG=${myDir}/lhapdf-config" >> tmpfile
mv Makefile Makefile.interm
cat tmpfile Makefile.interm > Makefile
rm -f Makefile.interm tmpfile


# Add libraries
echo "LIBS+=-lz -lstdc++" >> Makefile

# Add extra packages
if [ $jhugen = 1 ]; then
  wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/JHUGenerator.${jhugenversion}.tar.gz
  tar xzf JHUGenerator.${jhugenversion}.tar.gz
  cd JHUGenerator
  mv makefile makefile.interm
  cat makefile.interm | sed -e "s#Comp = ifort#Comp = gfort#g" > makefile
  make
  cd ..
fi
if [ "$process" = "gg_H_2HDM" ] || [ "$process" = "gg_H_MSSM" ]; then
  echo "Adding CHAPLIN 1.2 library"
  wget http://chaplin.hepforge.org/code/chaplin-1.2.tar
  tar xvf chaplin-1.2.tar
  cd chaplin-1.2
  ./configure --prefix=`pwd`/..
  make install
  cd .. 
  echo "LIBS+=-L`pwd`/lib/ -L`pwd`/lib64/" >> Makefile   # be safe
  export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH}
  if [ "$process" = "gg_H_MSSM" ]; then
    echo "Adding FeynHiggs 2.10 library"
    wget http://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/newversion/FeynHiggs-2.10.2.tar.gz
    tar xvf FeynHiggs-2.10.2.tar.gz
    cd FeynHiggs-2.10.2
    ./configure --prefix=`pwd`/..
    make 
    make install
    cd ..
  fi
fi  

make pwhg_main || fail_exit "Failed to compile pwhg_main"

if [[ -e ../pwhg_main-gnu ]]; then
  mv ../pwhg_main-gnu ../pwhg_main
  chmod a+x ../pwhg_main
fi

if [ ! -z $skipgen ]; then
    exit 0
fi

mkdir workdir
cd workdir
localDir=`pwd`

# Copy additional files
if [ -e  ${WORKDIR}/vbfnlo.input ]; then
  cp -p ${WORKDIR}/vbfnlo.input .
fi 
if [ -e ${WORKDIR}/br.a3_2HDM ]; then
  cp -p ${WORKDIR}/br*2HDM .
fi
if [ -e  ${WORKDIR}/powheg-fh.in ]; then
  cp -p ${WORKDIR}/powheg-fh.in .
fi
if [ -e  ${WORKDIR}/cteq6m ]; then
    cp -p ${WORKDIR}/cteq6m .
fi 

cat ${card} | sed -e "s#SEED#${seed}#g" | sed -e "s#NEVENTS#${nevt}#g" > powheg.input
cat powheg.input

 
# Use one stage procedure by default and two stage procedure if ttH process is used
if [ "$process" != "ttH" ]; then
	echo "Default one stage procedure is used"
	#make sure env variable for pdfsets points to the right place
	export LHAPDF_DATA_PATH=`${myDir}/lhapdf-config --datadir`
	../pwhg_main &> log_${process}_${seed}.txt
else
	# Save powheg input before starting two stage
	cp powheg.input powheg.input.original
	echo "Two stage procedure is used for ttH process"

	# Check if required options are set in input card, otherwise set them
	## use-old-grid must be set to 1
	if grep -Fq "use-old-grid" powheg.input; then
		echo "use-old-grid was already set, making sure it is set to 1"
		sed -i '/use-old-grid/c\use-old-grid    1 ! if 1 use old grid if file pwggrids.dat is present (<> 1 regenerate)' powheg.input
	else
		echo "Inserted use-old-grid 1 needed for the two stage procedure."
		echo -e "use-old-grid    1 ! if 1 use old grid if file pwggrids.dat is present (<> 1 regenerate)" >> powheg.input
	fi
	## use-old-ubound must be set to 1
	if grep -Fq "use-old-ubound" powheg.input; then
		echo "use-old-ubound was already set, making sure it is set to 1"
		sed -i '/use-old-ubound/c\use-old-ubound  1 ! if 1 use norm of upper bounding function stored in pwgubound.dat, if present; <> 1 regenerate' powheg.input
	else
		echo "Inserted use-old-ubound 1 needed for the two stage procedure."
		echo -e "use-old-ubound  1 ! if 1 use norm of upper bounding function stored in pwgubound.dat, if present; <> 1 regenerate" >> powheg.input
	fi

	# Preparation for first stage, ncall2 = 0 and fakevirt =1 must be set
	## ncall2 must be set to 0, but before a value must be defined for the second stage
	if grep -Fq "ncall2" powheg.input; then
		ncallTwoOriginal=$(grep "^ncall2 " powheg.input | cut -d' ' -f2-3)
		echo "ncall2 was set to $ncallTwoOriginal, set it to 0 for first stage." 
		sed -i '/ncall2/c\ncall2  0  ! number of calls for computing the integral and finding upper bound' powheg.input
	else
		cp powheg.input.orginal powheg.input
		fail_exit "Required ncall2 value was not defined"
	fi
	## fakevirt must be set to 1
	if grep -Fq "fakevirt" powheg.input; then
		echo "fakevirt was already set, making sure it is set to 1"
		sed -i '/fakevirt/c\fakevirt   1      ! (default 0) if 1 use Born for virtuals' powheg.input
	else
		echo "Inserted fakevirt 1 needed for first stage."
		echo -e "fakevirt   1      ! (default 0) if 1 use Born for virtuals" >> powheg.input
	fi

	# Save first stage powheg.input to allow a check later
	cp powheg.input powheg.input.stage1

	#make sure env variable for pdfsets points to the right place before starting first stage
	export LHAPDF_DATA_PATH=`${myDir}/lhapdf-config --datadir`
	../pwhg_main &> log_${process}_${seed}_stage1.txt

	# Preparation for second stage, ncall2 = [OriginalValue] and fakevirt = 0 must be set
	## Revert ncall2 to original value
	if grep -Fq "ncall2" powheg.input; then
		echo "ncall2 was set back to $ncallTwoOriginal as required for second stage." 
		sed -i "/ncall2/c\ncall2  $ncallTwoOriginal  ! number of calls for computing the integral and finding upper bound" powheg.input
	else
		fail_exit "Something strange happened, ncall2 parameter was not found in second stage."
	fi
	## fakevirt must be set to 1
	if grep -Fq "fakevirt" powheg.input; then
		echo "fakevirt set to 0 as required for second stage."
		sed -i '/fakevirt/c\fakevirt   0      ! (default 0) if 1 use Born for virtuals' powheg.input
	else
		fail_exit "Something strange happened, fakevirt parameter was not found in second stage."
	fi

	# Save second stage powheg.input to allow a check later
	cp powheg.input powheg.input.stage2

	#make sure env variable for pdfsets points to the right place before starting first stage
	export LHAPDF_DATA_PATH=`${myDir}/lhapdf-config --datadir`
	../pwhg_main &> log_${process}_${seed}_stage2.txt
fi

#remove the spurious random seed output that is non LHE standard 
cat pwgevents.lhe | grep -v "Random number generator exit values" > ${file}_final.lhe
ls -l ${file}_final.lhe
sed -i 's/Input file powheg.input contained:/Process: '$process'\nInput file powheg.input contained:/g' ${file}_final.lhe
pwd
cp ${file}_final.lhe ${WORKDIR}/.

myDir=powhegbox_${process}
mkdir ${WORKDIR}/${myDir}
cp -p ../pwhg_main ${WORKDIR}/${myDir}/.
cp -pr ../lib ${WORKDIR}/${myDir}/.
cp -pr ../lib64 ${WORKDIR}/${myDir}/.
cp -p pwg*.dat ${WORKDIR}/${myDir}/.
cp -p *.top ${WORKDIR}/${myDir}/.
cp -p ${WORKDIR}/vbfnlo.* ${WORKDIR}/${myDir}/.
cp -p ${WORKDIR}/br.* ${WORKDIR}/${myDir}/.
cp -p ${WORKDIR}/*fh.in ${WORKDIR}/${myDir}/.
cp -p ${WORKDIR}/cteq6m ${WORKDIR}/${myDir}/.

cp -p ../JHUGenerator/JHUGen ${WORKDIR}/${myDir}/.
cp -pr ../JHUGenerator/pdfs ${WORKDIR}/${myDir}/.

cd ${WORKDIR}/${myDir}
cp -p ${card} .

if [ $jhugen = 1 ]; then
  cp -p ${cardj} .
  if [ ! -e  ${WORKDIR}/runcmsgrid_powhegjhugen.sh ]; then
   fail_exit "Did not find " ${WORKDIR}/runcmsgrid_powhegjhugen.sh 
  fi
  sed -e 's/PROCESS/'${process}'/g' ${WORKDIR}/runcmsgrid_powhegjhugen.sh > runcmsgrid.sh
else
  if [ ! -e  ${WORKDIR}/runcmsgrid_powheg.sh ]; then
   fail_exit "Did not find " ${WORKDIR}/runcmsgrid_powheg.sh 
  fi
  sed -e 's/PROCESS/'${process}'/g' ${WORKDIR}/runcmsgrid_powheg.sh > runcmsgrid.sh
fi

chmod 755 runcmsgrid.sh
tar cpzsf ${tarball}_tarball.tar.gz *
mv ${tarball}_tarball.tar.gz ${WORKDIR}/.
cd ${WORKDIR}
tar tvf ${tarball}_tarball.tar.gz
rm -rf ${myDir}

echo "Tarball ${tarball}_tarball.tar.gz ready with log_${process}_${seed}.txt and ${file}_final.lhe at ${localDir}"
echo "End of job on " `date`
exit 0;
