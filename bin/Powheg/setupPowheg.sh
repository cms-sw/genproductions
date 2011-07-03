#!/bin/bash

#set -o verbose

echo "Starting job on " `date`
echo "Running on " `uname -a`
echo "System release " `cat /etc/redhat-release`

seed=$1
process="Z"
card="/afs/cern.ch/user/l/lenzip/scratch0/powheg/Configuration/GenProduction/bin/Powheg/powheg.input" 
store="/castor/cern.ch/user/l/lenzip/powheg/leshouches/Z/tevatron/muon/"

# Release to be used to define the environment and the compiler needed

export PRODHOME=`pwd`
export SCRAM_ARCH=slc5_amd64_gcc434
export RELEASE=CMSSW_4_3_0_pre5

# initialize the CMS environment 
# !!! To be set according to the local installation !!!

#source $VO_CMS_SW_DIR/cmsset_default.sh

# location of the madgraph tarball and of the output main storage directory

name="cmssw_powheg"
scram project -n ${name} CMSSW ${RELEASE} ; cd ${name} ; mkdir -p work ; cd work
eval `scram runtime -sh`

# force the f77 compiler to be the CMS defined one

ln -s `which gfortran` f77
ln -s `which gfortran` g77
export PATH=`pwd`:${PATH}

#workaround to fix fastjet-config.
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

svn checkout --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/POWHEG-BOX
cd POWHEG-BOX/${process}
mv Makefile Makefile.orig
cat Makefile.orig | sed -e "s#STATIC[ \t]*=[ \t]*-static#STATIC=-dynamic#g" | sed -e "s#PDF[ \t]*=[ \t]*native#PDF=lhapdf#g"> Makefile
make pwhg_main
mkdir workdir
cd workdir
cat ${card} | sed -e "s#SEED#${seed}#g" > powheg.input
ls 
cat powheg.input
../pwhg_main
mv pwgevents.lhe pwgevents_${seed}.lhe
rfcp pwgevents_${seed}.lhe ${store}

echo "End of job on " `date`
exit 0;
