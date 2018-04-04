#!/bin/bash
<<<<<<< HEAD
PTYHONPATH=$PYTHONPATH:/usr/lib64/python2.6/site-packages
source source_condor.sh
=======
source Utilities/source_condor.sh
>>>>>>> upstream/mg26x

name=$1
carddir=$2
workqueue="condor"
scram_arch=$3
cmssw_version=$4
<<<<<<< HEAD
bash gridpack_generation.sh ${name} ${carddir} ${workqueue} ${scram_arch} ${cmssw_version}
=======
bash gridpack_generation.sh ${name} ${carddir} ${workqueue} ALL ${scram_arch} ${cmssw_version}
>>>>>>> upstream/mg26x
