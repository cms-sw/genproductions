#!/bin/bash

fail_exit() { echo "$@" 1>&2; exit 1; }

#set -o verbose
EXPECTED_ARGS=7

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` source_repository source_tarball_name process card tarballName Nevents RandomSeed"
    echo "Example: ./create_powheg_tarball.sh slc6_amd64_gcc481/powheg/V1.0/src powhegboxv1.0_Oct2013 Z slc6_amd64_gcc481/powheg/V1.0/8TeV_Summer12/DYToEE_M-20_8TeV-powheg/v1/DYToEE_M-20_8TeV-powheg.input Z_local 1000 1212" 
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
echo "%MSG-POWHEG tar ball file name = $tarball"

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
  mv ${tarball}.tar.gz old_${tarball}.tar.gz
fi

scram project -n ${name} CMSSW ${RELEASE}; cd ${name} ; mkdir -p work ; 
eval `scram runtime -sh`
cd work
export PATH=`pwd`:${PATH}

# FastJet and LHAPDF
#fastjet-config comes with the paths used at build time.
#we need this to replace with the correct paths obtained from scram tool info fastjet

newinstallationdir=`scram tool info fastjet | grep FASTJET_BASE |cut -d "=" -f2`
cp ${newinstallationdir}/bin/fastjet-config ./fastjet-config.orig

oldinstallationdir=`cat fastjet-config.orig | grep installationdir | head -n 1 | cut -d"=" -f2`
sed -e "s#${oldinstallationdir}#${newinstallationdir}#g" fastjet-config.orig > fastjet-config 
chmod +x fastjet-config

#same for lhapdf
newinstallationdirlha=`scram tool info lhapdf | grep LHAPDF_BASE |cut -d "=" -f2`
cp ${newinstallationdirlha}/bin/lhapdf-config ./lhapdf-config.orig
oldinstallationdirlha=`cat lhapdf-config.orig | grep prefix | head -n 1 | cut -d"=" -f2`
sed -e "s#prefix=${oldinstallationdirlha}#prefix=${newinstallationdirlha}#g" lhapdf-config.orig > lhapdf-config
chmod +x lhapdf-config

# Get the input card
wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${cardinput} -O powheg.input  || fail_exit "Failed to obtain input card" ${cardinput}
myDir=`pwd`
card=${myDir}/powheg.input

### retrieve the powheg source tar ball
wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${repo}/${name}.tar.gz  -O ${name}.tar.gz || fail_exit "Failed to get powheg tar ball " ${name}
tar xzf ${name}.tar.gz

cd POWHEG-BOX/${process}

mv Makefile Makefile.orig
cat Makefile.orig | sed -e "s#STATIC[ \t]*=[ \t]*-static#STATIC=-dynamic#g" | sed -e "s#PDF[ \t]*=[ \t]*native#PDF=lhapdf#g"  | sed -e '/FJCXXFLAGS/s=--cxxflags)=--cxxflags) -std\=c++11='> Makefile
echo "LIBS+=-lz -lstdc++" >> Makefile


LHA_BASE="`readlink -f "$LHAPATH/../../../"`"


LHA_BASE_OLD="`$LHA_BASE/bin/lhapdf-config --prefix`"
cat > lhapdf-config-wrap <<EOF
#!/bin/bash
"$LHA_BASE/bin/lhapdf-config" "\$@" | sed "s|$LHA_BASE_OLD|$LHA_BASE|g"
EOF

chmod a+x lhapdf-config-wrap

make LHAPDF_CONFIG="`pwd`/lhapdf-config-wrap" pwhg_main || fail_exit "Failed to compile pwhg_main"


mkdir workdir
cd workdir
localDir=`pwd`

if [ -e  ${WORKDIR}/vbfnlo.input ]; then
    cp -p ${WORKDIR}/vbfnlo.input .
fi

cat ${card} | sed -e "s#SEED#${seed}#g" | sed -e "s#NEVENTS#${nevt}#g" > powheg.input
cat powheg.input
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
tar cpzsf ${tarball}.tar.gz *
mv ${tarball}.tar.gz ${WORKDIR}/.
cd ${WORKDIR}
tar tvf ${tarball}.tar.gz
rm -rf ${myDir}

echo "Tarball ${tarball}.tar.gz ready with log_${process}_${seed}.txt and ${file}_final.lhe at ${localDir}"
echo "End of job on " `date`
exit 0;
