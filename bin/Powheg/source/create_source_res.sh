#!/bin/bash
fail_exit() { echo "$@" 1>&2; exit 1; }

#set -o verbose                                                                                                                                                                   
EXPECTED_ARGS=1

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` workdir_prefix"
    echo "Example: `basename $0` my"
    exit 1
fi

echo "   ______________________________________________________    "
echo "         Running Powheg  "$basename"                         "
echo "   ______________________________________________________    "

topdir=$PWD
prefix=$1
workdir=$topdir/temp_$prefix

if [[ -e ${workdir} ]]; then
  fail_exit "The directory ${workdir} exists! Please clean up your work directory before running!!"
fi

mkdir -p $workdir
cd $workdir

### Checkout main powheg source directory
svn checkout --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/POWHEG-BOX-RES POWHEG-BOX

powhegdir=$workdir/POWHEG-BOX

cd $powhegdir
version=`svn info | grep -a "Revision" | awk '{print $2}'`
date=`date +%Y%m%d`
cd -
output=powhegboxRES_rev${version}_date${date}

### Check out user process
svn co --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/User-Processes-RES
cd User-Processes-RES

for file in $(ls $workdir/User-Processes-RES)
do
    echo $file
    tar cpzf $powhegdir/${file}.tgz $file  
done

cd $workdir
tar cpzf ${output}.tar.gz POWHEG-BOX

sourcedir=/eos/project/c/cmsweb/www/generators/directories/cms-project-generators/slc6_amd64_gcc481/powheg/V2.0/src
mv ${output}.tar.gz $sourcedir/${output}.tar.gz 

cd $topdir

echo "The tar ball is now at "$sourcedir/${output}.tar.gz
echo "End of job on " `date`
exit 0;
