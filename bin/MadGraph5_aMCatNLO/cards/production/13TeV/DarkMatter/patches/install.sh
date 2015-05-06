#!/bin/bash

#exit on first error
set -e

RUNHOME=`pwd`

echo "Starting job on " `date` #Only to display the starting of production date
echo "Running on " `uname -a` #Only to display the machine where the job is running
echo "System release " `cat /etc/redhat-release` #And the system release

AFSFOLD=${RUNHOME}/${name}
AFS_GEN_FOLDER=${RUNHOME}/${name}
CARDSDIR=${RUNHOME}/${carddir}

MGBASEDIR=mgbasedir

MG=MG5_aMC_v2.2.2.tar.gz
MGSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$MG

#syscalc is a helper tool for madgraph to add scale and pdf variation weights for LO processes
SYSCALC=SysCalc_V1.1.0.tar.gz
SYSCALCSOURCE=https://cms-project-generators.web.cern.ch/cms-project-generators/$SYSCALC

MGBASEDIRORIG=MG5_aMC_v2_2_2

isscratchspace=0
user=`id -u -n`
if [ ! -d "/tmp/$user/CMSSW_7_1_15_patch1" ]; then
    cd /tmp/$user
    scramv1 project CMSSW CMSSW_7_1_15_patch1
fi 
cd /tmp/$user/CMSSW_7_1_15_patch1/src
eval `scram runtime -sh`
cd -
echo $CMSSW_BASE

#Copy, Unzip and Delete the MadGraph tarball#
wget --no-check-certificate ${MGSOURCE}
tar xzf ${MG}
rm $MG

#Apply any necessary patches on top of official release
patch -l -p0 -i $RUNHOME/patches/mgfixes.patch
patch -l -p0 -i $RUNHOME/patches/models.patch

cd $MGBASEDIRORIG
LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml
if [ -e $LHAPDF6TOOLFILE ]; then
    LHAPDFCONFIG=`cat $LHAPDF6TOOLFILE | grep "<environment name=\"LHAPDF6_BASE\"" | cut -d \" -f 4`/bin/lhapdf-config
else
    LHAPDFCONFIG=`echo "$LHAPDF_DATA_PATH/../../bin/lhapdf-config"`
fi
#make sure env variable for pdfsets points to the right place
export LHAPDF_DATA_PATH=`$LHAPDFCONFIG --datadir`
LHAPDFINCLUDES=`$LHAPDFCONFIG --incdir`
LHAPDFLIBS=`$LHAPDFCONFIG --libdir`
BOOSTINCLUDES=`scram tool tag boost INCLUDE`
echo  $LHAPDFCONFIG
echo "set auto_update 0"                  >  mgconfigscript
echo "set automatic_html_opening False"   >> mgconfigscript
echo "set output_dependencies internal"   >> mgconfigscript
echo "set lhapdf $LHAPDFCONFIG"           >> mgconfigscript

if [ -n "$queue" ]; then
    echo "set run_mode  1"                 >> mgconfigscript
    echo "set cluster_type lsf"            >> mgconfigscript
    echo "set cluster_queue $queue"        >> mgconfigscript
    echo "set cluster_status_update 60 30" >> mgconfigscript
    echo "set cluster_nb_retry 5"          >> mgconfigscript
    echo "set cluster_retry_wait 300"      >> mgconfigscript 
    if [[ ! "$RUNHOME" =~ ^/afs/.* ]]; then
        echo "local path is not an afs path, batch jobs will use worker node scratch space instead of afs"
        echo "set cluster_temp_path `echo $RUNHOME`" >> mgconfigscript 
        isscratchspace=1
    fi      
else
    echo "set run_mode 2" >> mgconfigscript
fi

echo "save options" >> mgconfigscript
./bin/mg5_aMC mgconfigscript

#get syscalc and compile
wget --no-check-certificate ${SYSCALCSOURCE}
tar xzf ${SYSCALC}
rm $SYSCALC
cd SysCalc
sed -i "s#INCLUDES =  -I../include#INCLUDES =  -I../include -I${LHAPDFINCLUDES} -I${BOOSTINCLUDES}#g" src/Makefile
sed -i "s#LIBS = -lLHAPDF#LIBS = ${LHAPDFLIBS}/libLHAPDF.a #g" src/Makefile
make
cd ..
cd $WORKDIR

