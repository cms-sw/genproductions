#!/bin/bash

fail_exit() { echo "$@"; exit 1; }

#set -o verbose
EXPECTED_ARGS=3

if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` PowhegTarfile Njobs NeventsPerJob"
    echo "Example: `basename $0` my_ggh_nnpdf30.tgz 10 1000" 
    exit 1
fi

echo "   ______________________________________     "
echo "      Producing Powheg LHE in parallel        "
echo "   ______________________________________     "

tar=${1}
echo "%MSG-POWHEG tar file = $tar"

numj=${2}
echo "%MSG-POWHEG number of jobs = $numj"

nume=${3}
echo "%MSG-POWHEG number of events per job = $nume"

#start=${4}
#echo "%MSG-POWHEG Starting from job = $start"

eval `scramv1 runtime -sh`

echo -e "Prepare jobs \n"
export WORKDIR=`pwd`
rm -f submitEvents.sh events.condorConf
touch submitEvents.sh
echo -e "#!/bin/bash" > submitEvents.sh
echo -e "fail_exit() { echo "\$\@" 1>&2; exit 1; }" >> submitEvents.sh
echo -e "echo \"Start of job on \" `date`" >> submitEvents.sh
echo -e "source /cvmfs/cms.cern.ch/cmsset_default.sh" >> submitEvents.sh
echo -e "tar -xzvf ${WORKDIR}/${tar}" >> submitEvents.sh
echo -e "./runcmsgrid.sh ${nume} \$1 1" >> submitEvents.sh
echo -e "mv cmsgrid_final.lhe ${WORKDIR}/cmsgrid_final_\$1.lhe" >> submitEvents.sh
chmod a+x submitEvents.sh 

cat << EOF >> events.condorConf
   executable              = submitEvents.sh
   arguments               = \$(ClusterId)\$(ProcId)
   output                  = events_\$(ProcId).out
   error                   = events_\$(ProcId).err 
   log                     = events_\$(ProcId).log 
   +JobFlavour             = "microcentury" 
   periodic_remove         = JobStatus == 5  
   WhenToTransferOutput    = ON_EXIT_OR_EVICT
   queue ${numj}        
EOF
condor_submit events.condorConf 

echo "End of job on " `date`
exit 0;
