#!/bin/bash

nevt=${1}
echo "%MSG-MG5 number of events requested = $nevt"

rnum=${2}
echo "%MSG-MG5 random seed used for the run = $rnum"

ncpu=${3}
echo "%MSG-MG5 number of cpus = $ncpu"

LHEWORKDIR=`pwd`
use_gridpack_env=true
if [ -n "$4" ]
  then
  use_gridpack_env=$4
fi

if [ "$use_gridpack_env" = true ]
  then
    if [ -n "$5" ]
      then
        scram_arch_version=${5}
      else
        scram_arch_version=SCRAM_ARCH_VERSION_REPLACE
    fi
    echo "%MSG-MG5 SCRAM_ARCH version = $scram_arch_version"

    if [ -n "$6" ]
      then
        cmssw_version=${6}
      else
        cmssw_version=CMSSW_VERSION_REPLACE
    fi
    echo "%MSG-MG5 CMSSW version = $cmssw_version"
    export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
    source $VO_CMS_SW_DIR/cmsset_default.sh
    export SCRAM_ARCH=${scram_arch_version}
    scramv1 project CMSSW ${cmssw_version}
    cd ${cmssw_version}/src
    eval `scramv1 runtime -sh`
fi
cd $LHEWORKDIR

#eval `scramv1 runtime -sh`
cd BASEDIR/pdfs
wget http://pcteserver.mi.infn.it/~nnpdf/nnpdf30/NNPDF30_lo_as_0130.LHgrid.tgz
tar -zxvf ./NNPDF30_lo_as_0130.LHgrid.tgz
rm ./NNPDF30_lo_as_0130.LHgrid.tgz
cd ..

GENCOMMAND
file=Out.lhe

head=`cat   $file | grep -in init | sed "s@:@ @g" | awk '{print $1-2}' | tail -1`
tail=`wc -l $file | awk -v tmp="$head" '{print $1-2-tmp}'`

tail -${tail} $file                                     >  ${file}_tail_dec
sed "s@\*\*\*@1.000@g"     ${file}_tail_dec              > ${file}_tail
./boltdmdec ${file}_tail
sed "s@-1000022@1000022@g" ${file}_tail_dec             >  ${file}_tail
head -$head $file                                       > cmsgrid.lhe
echo "  "XSECTION"   "XSECUNC"  1.00000000000E-00 10001" >> cmsgrid.lhe
echo "</init>"                                         >> cmsgrid.lhe
cat ${file}_tail                                       >> cmsgrid.lhe
rm ${file}
rm ${file}_tail
rm ${file}_tail_dec
mv cmsgrid.lhe ../cmsgrid_final.lhe
cd ..
