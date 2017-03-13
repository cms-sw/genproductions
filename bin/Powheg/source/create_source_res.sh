#!/bin/bash
fail_exit() { echo "$@" 1>&2; exit 1; }

#set -o verbose                                                                                                                                                                   
EXPECTED_ARGS=1

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` outputtarball_prefix"
    echo "Example: `basename $0` powhegboxV2_Oct2015"
    exit 1
fi

echo "   ______________________________________________________    "
echo "         Running Powheg  "$basename"                         "
echo "   ______________________________________________________    "

topdir=$PWD
output=$1
workdir=$topdir/temp_$output

if [[ -e ${workdir} ]]; then
  fail_exit "The directory ${workdir} exists! Please clean up your work directory before running!!"
fi

mkdir -p $workdir
cd $workdir

### Checkout main powheg source directory
svn checkout --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/POWHEG-BOX-RES POWHEG-BOX

powhegdir=$workdir/POWHEG-BOX

### Check out user process
svn co --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/User-Processes-RES
cd User-Processes-RES

for file in $(ls $workdir/User-Processes-RES)
do
    echo $file
    tar cspzf $powhegdir/${file}.tgz --exclude .svn $file  
done

cd $workdir
tar cspzf ${output}.tar.gz --exclude .svn POWHEG-BOX

sourcedir=/afs/cern.ch/cms/generators/www/slc6_amd64_gcc481/powheg/V2.0/src

mv ${output}.tar.gz $sourcedir/${output}.tar.gz 

cd $topdir

echo "The tar ball is now at "$sourcedir/${output}.tar.gz
echo "End of job on " `date`
exit 0;
