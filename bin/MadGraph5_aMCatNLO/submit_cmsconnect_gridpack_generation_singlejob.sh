#!/bin/bash

source cmsconnect_utils.sh
source source_condor.sh

create_gridpack_jdl(){
cat<<-EOF
	Universe = vanilla 
	Executable = $gridpack_exe
	Arguments = $card_name $card_dir
	
	Error = condor_log/job.err.\$(Cluster)-\$(Process) 
	Output = condor_log/job.out.\$(Cluster)-\$(Process) 
	Log = condor_log/job.log.\$(Cluster) 
	
	#transfer_input_files = $input_files, gridpack_generation.sh, runcmsgrid_LO.sh, runcmsgrid_NLO.sh, cleangridmore.sh, /usr/bin/unzip
	transfer_input_files = $input_files, gridpack_generation.sh
	transfer_output_files = ${card_name}.log
	should_transfer_files = YES
	when_to_transfer_output = ON_EXIT_OR_EVICT
	+WantIOProxy=true
	periodic_release = (JobStatus == 5) && StringListMember(HoldReasonCode,"26,13,256,12,6") && (NumJobStarts < 11)
	
	+REQUIRED_OS = "rhel6"
	request_cpus = $cores
	request_memory = $memory
	+MaxWallTimeMins = $condor_maxwalltime
	Queue 1
EOF
}

create_gridpack_exe(){
cat<<-EOF
	#! /bin/bash

	# Untar input files
	tar xfz "$input_files"
	
	# Setup CMS framework
	export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
	source \$VO_CMS_SW_DIR/cmsset_default.sh

	# Purdue wokaround
	unset CXX CC FC
	# Run
	PATH=\$PATH:$PWD iscmsconnect=1 bash -x gridpack_generation.sh "${card_name}" "${card_dir}" "${workqueue}" ALL "${scram_arch}" "${cmssw_version}"
	exitcode=\$?

	if [ \$exitcode -ne 0 ]; then
	    echo "Something went wrong while running madgraph. Exiting now."
	    exit \$exitcode
	fi

        # Tarball gridpack working area
        # Should not be needed, except for debugging purposes
        # skip this unless confirmed this would be needed.

	# Stage-out tarball
	# First, try XRootD via stash.osgconnect.net
	echo ">> Copying tarball via XRootD"
        TARBALL_FULLPATH=\$(ls \${condor_scratch}/*.xz | head -n 1)
        if [ "x\${TARBALL_FULLPATH}" == "x" ]; then
            echo "[Error] Final tarball was not create. Please, see the logfile for more info."
            exit 211
        fi
        TARBALL=\$(basename "\${TARBALL_FULLPATH}")

	xrdcp -f "\${TARBALL_FULLPATH}" "root://stash.osgconnect.net:1094/${stash_tmpdir##/stash}/\$TARBALL"
	exitcode=\$?
	if [ \$exitcode -eq 0 ]; then
	    exit 0
	else
	    echo "The xrdcp command below failed:"
	    echo "xrdcp -f "\${TARBALL_FULLPATH} root://stash.osgconnect.net:1094/${stash_tmpdir##/stash}/\$TARBALL"
	fi
	# Second, try condor_chirp
	echo ">> Copying output via condor_chirp"
	CONDOR_CHIRP_BIN=\$(command -v condor_chirp)
	if [ \$? != 0 ]; then
	    if [ -n "\${CONDOR_CONFIG}" ]; then
	        CONDOR_CHIRP_BIN="\$(dirname \$CONDOR_CONFIG)/main/condor/libexec/condor_chirp"
	    fi
	fi
	"\${CONDOR_CHIRP_BIN}" put -perm 644 "\${TARBALL_FULLPATH}" "\${TARBALL}"
	exitcode=\$?
	if [ \$exitcode -ne 0 ]; then
	    echo "condor_chirp failed. Exiting with error code 210."
	    exit 210
	fi
	rm "\${TARBALL_FULLPATH}"

EOF
}


# Run git status before submitting condor job
if [ -z "$PRODHOME" ]; then
  PRODHOME=`pwd`
fi 
cd $PRODHOME
git status
echo "Current git revision is:"
git rev-parse HEAD
git diff | cat
cd -

# Start proxy certificate watcher
mintime=172800
timeleft=$(voms-proxy-info -timeleft 2>&1)
if [ $? != 0 ]; then
    echo -e "Proxies are needed. Please execute the command below and submit your gridpack again:\n\nvoms-proxy-init -voms cms -valid 192:00"
    exit 100
elif [ "$timeleft" -lt "$mintime" ]; then
    echo "Your proxy time left is less that 48 hours. Please renew it before submitting your gridpack."
    exit 101
fi

echo -e "Start proxy watcher job.\nThis will be automatically removed at the end of the script. If you kill this process before though, please execute:\nproxy-watcher -remove"
proxy-watcher -start


card_name=$1
card_dir=$2
workqueue="local"
# How many cores and how much memory should we request for the gridpack generation?
# Using 8 cores and 16 Gb by default.
cores="${3:-8}"
memory="${4:-16 Gb}"
condor_maxwalltime="${5:-2400}"
scram_arch="${6:-}"
cmssw_version="${7:-}"

parent_dir=$PWD
##############################################
# SUBMITTING JOB
##############################################
echo ">> Start gridpack generation"
## Name condor executable and submit files
gridpack_exe="gridpack_${card_name}.sh"
gridpack_jdl="gridpack_${card_name}.jdl"

# Tarball workflow cards and Madgraph patches.
# Those will be input files for the job
input_files="input_${card_name}.tar.gz"
patches_directory="./patches"

if [ -e "$input_files" ]; then rm "$input_files"; fi
tar -zcf "$input_files" "$card_dir" "$patches_directory" "runcmsgrid_LO.sh" "runcmsgrid_NLO.sh" "cleangridmore.sh" "/usr/bin/unzip"

## Create a submit file for a single job
# create_gridpack_exe arguments are:
# - input_files, card_name, card_dir, workqueue, sandbox_output, cores, memory
# create_gridpack_jdl arguments are:
# - gridpack_exe, card_name, input_files, sandbox_output, cores, memory, condor_maxwalltime

# Clean up old files first, just in case.
if [ -e "${card_name}.log" ]; then rm "${card_name}.log"; fi

# Create a temp directory in user's stash area.
# Modify permissions so that XRootD can write to it.
# @TODO: Find a better way to do this.
stash_tmpdir=$(mktemp -d --tmpdir=/stash/user/$USER)
chmod 777 "$stash_tmpdir"

create_gridpack_exe > "$gridpack_exe"
chmod +x $gridpack_exe
create_gridpack_jdl > "$gridpack_jdl"

# Submit condor job and wait for it to finish
echo ">> Submitting gridpack condor job and wait"
mkdir -p condor_log
CLUSTER_ID=$(condor_submit "${gridpack_jdl}" | tail -n1 | rev | cut -d' ' -f1 | rev)
LOG_FILE=$(condor_q -format '%s\n' UserLog $CLUSTER_ID | tail -n1)
condor_wait "$LOG_FILE" "$CLUSTER_ID"

# If querying job exitcode fails, retry
status_n_retries=10
for ((i=0; i<=$status_n_retries; ++i)); do
    condor_exitcode=$(condor_history ${CLUSTERID} -limit 1 -format "%s" ExitCode)
    if [ "x$condor_exitcode" != "x" ]; then
        break
    fi
    sleep 10
done

# Check condor job exit code before going to the next step
if [ "x${condor_exitcode}" != "x" ]; then
    echo "condor job failed with exitcode: $condor_exitcode. Please, check logfiles in condor_log/ directory (Job Id: $CLUSTER_ID). Exiting now."
    exit $condor_exitcode
fi

# Check sandbox
tarball_output="$(ls $stash_tmpdir/*.tar.xz | head -n 1)"
if [ "x${tarball_output}" != "x" ]; then
    mv "${tarball_output}" "$parent_dir"
    rmdir "$stash_tmpdir"
else
    if [ -d "$stash_tmpdir" ]; then rmdir "$stash_tmpdir"; fi
    echo "CODEGEN condor job sandbox output could not be located. Exiting now."
    exit 210
fi

echo "Your gridpack ran sucessfully!."

# Clean up condor exe, jdl and condor logfiles.
rm "$gridpack_exe" "$gridpack_jdl"
rm condor_log/job.*.${CLUSTER_ID%%.}-* condor_log/job.log.${CLUSTER_ID%%.}

# Clean up input files
rm "$input_files"

echo "Remove proxy watcher job"
proxy-watcher -remove
