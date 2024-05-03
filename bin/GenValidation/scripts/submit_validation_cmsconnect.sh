#!/bin/bash

# step1 make sandbox

# number of events per job 
NEVTS=5000
WORKDIR=`pwd -P`
SCRAM_ARCH=slc6_amd64_gcc630
RELEASE=CMSSW_9_3_16
FRAGMENTDIR=${WORKDIR}/Fragments

OTAGLIST=()
GENFRAGMENTLIST=()

OTAGLIST+=( dyee01j_2p6p5 )
OTAGLIST+=( dyee01j_2p6p7 )
GENFRAGMENTLIST+=(Hadronizer_TuneCP5_13TeV_MLM_5f_pythia8_cff )
GENFRAGMENTLIST+=(Hadronizer_TuneCP5_13TeV_MLM_5f_pythia8_cff )

### setup release 
if [ -r ${WORKDIR}/${RELEASE}/src ] ; then 
    echo release ${RELEASE} already exists
else
    scram p CMSSW ${RELEASE}
fi
cd ${WORKDIR}/${RELEASE}/src
eval `scram runtime -sh`

mkdir -p MY/PRO/python
mkdir -p MY/PRO/test
scram b
cd MY/PRO/python

NTAG=`echo "scale=0; ${#OTAGLIST[@]} -1 " | bc`
for ITAG in `seq 0 ${NTAG}`; do
	OTAG=${OTAGLIST[${ITAG}]}
	GENFRAGMENT=$FRAGMENTDIR\/${GENFRAGMENTLIST[${ITAG}]}
	### check that fragments are available 
	echo "Check that fragments are available ..."
	if [ ! -s ${GENFRAGMENT}.py ] ; then
	    echo "... cannot find ${GENFRAGMENT}.py"
	    exit 0;
	else
	    echo "... found required fragments!"
	fi
	
	### create generator fragment 
	CONFIG=${OTAG}_cff.py
	if [ "${ITAG}" != 0 ] ; then
	    cd ${CMSSW_BASE}/src/MY/PRO/python
	fi
	if [ -f ${CONFIG} ] ; then
	    rm ${CONFIG}
	fi

	cat > ${CONFIG} <<EOF
import FWCore.ParameterSet.Config as cms
externalLHEProducer = cms.EDProducer('ExternalLHEProducer', 
args = cms.vstring('demo.tarball.tar.xz'),
nEvents = cms.untracked.uint32(5000),
numberOfParameters = cms.uint32(1),  
outputFile = cms.string('cmsgrid_final.lhe'),
scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)
EOF
	cat ${GENFRAGMENT}.py >> ${CONFIG}

	cd ${CMSSW_BASE}/src/MY/PRO/test/
	### make validation fragment 
	cmsDriver.py MY/PRO/python/${CONFIG} \
        -n ${NEVTS} --mc --no_exec --python_filename cmsrun_${OTAG}.py \
        -s LHE,GEN,VALIDATION:genvalid_dy --datatier GEN,GEN-SIM,DQMIO --eventcontent LHE,RAWSIM,DQM \
        --conditions 93X_mc2017_realistic_v3 --beamspot Realistic25ns13TeVEarly2017Collision
done

cd ${CMSSW_BASE}/src/
scram b

cd ../../
# make sandbox
cmssw-sandbox create -a $RELEASE
SANDBOX=`ls | grep sandbox`
# move the sandbox to your stash public on cmsconnect
mv $SANDBOX ~/stash/public/$SANDBOX

# step2 set up run file
echo "set up run script..."
Wrapper_name=run_wrapper.sh
cat > ${Wrapper_name} <<EOF
#!/bin/bash
exit_on_error() {
result=\$1
code=\$2
message=\$3
if [ \$1 != 0 ]; then
echo \$3
exit \$2
fi
}
USER=melu
INIT_PATH=\`pwd -P\`
OTAGLIST=()
OTAGLIST+=( dyee01j_2p6p5 )
OTAGLIST+=( dyee01j_2p6p7 )
GRIDPACKLIST=()
GRIDPACKLIST+=( DYJetsToEE_LO_MLM_mee50_slc6_amd64_gcc630_CMSSW_9_3_16_265_tarball.tar.xz )
GRIDPACKLIST+=( DYJetsToEE_LO_MLM_mee50_slc6_amd64_gcc630_CMSSW_9_3_16_267_tarball.tar.xz )
NTAG=\`echo "scale=0; \${#OTAGLIST[@]} -1 " | bc\`
#### FRAMEWORK SANDBOX SETUP ####
# Load cmssw_setup function
source cmssw_setup.sh
# Setup CMSSW Base
export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch
source \$VO_CMS_SW_DIR/cmsset_default.sh
# Download sandbox, replace it when you have different sandbox_name
sandbox_name="$SANDBOX"
# Change to your own http
echo "download sandbox"
wget --no-check-certificate --progress=bar "http://stash.osgconnect.net/+\${USER}/\${sandbox_name}" || exit_on_error \$? 150 "Could not download sandbox."
# Setup framework from sandbox
echo "unpack sandbox"
cmssw_setup \$sandbox_name || exit_on_error \$? 151 "Could not unpack sandbox"
#### END OF FRAMEWORK SANDBOX SETUP ####
# Enter script directory
cd \$CMSSW_BASE/src/MY/PRO/test
Current_path=\`pwd -P\`
echo "download gridpack"
echo \${GRIDPACKLIST[0]}
echo \${GRIDPACKLIST[1]}
wget --no-check-certificate --progress=bar "http://stash.osgconnect.net/+\${USER}/\${GRIDPACKLIST[0]}" || exit_on_error \$? 150 "Could not download gridpack."
wget --no-check-certificate --progress=bar "http://stash.osgconnect.net/+\${USER}/\${GRIDPACKLIST[1]}" || exit_on_error \$? 150 "Could not download gridpack."
eval \`scram runtime -sh\`
### loop on samples #####
for ITAG in \`seq 0 \${NTAG}\`; do
#for ITAG in \`seq 0 0\`; do
	sed -i "s|^.*tarball.tar.xz.*$|   args=cms.vstring(\'\$Current_path\/\${GRIDPACKLIST[\${ITAG}]}\'),|" cmsrun_\${OTAGLIST[\${ITAG}]}.py
	date
	cd \${Current_path}
	LINE=\`egrep -n Configuration.StandardSequences.Services_cff cmsrun_\${OTAGLIST[\${ITAG}]}.py | cut -d: -f1 \`
	echo 'lines:',\${LINE}
	SEED=\`echo "5267+\${1}" | bc\`
	echo 'seed:',\${SEED}
	sed -i "\${LINE}"aprocess.RandomNumberGeneratorService.generator.initialSeed=\${SEED} cmsrun_\${OTAGLIST[\${ITAG}]}.py
	SEED=\`echo "289634+\${1}" | bc\`
	sed -i "\${LINE}"aprocess.RandomNumberGeneratorService.externalLHEProducer.initialSeed=\${SEED} cmsrun_\${OTAGLIST[\${ITAG}]}.py
	# cmsRun
	echo "cmsRun ", cmsrun_\${OTAGLIST[\${ITAG}]}.py
	cmsRun cmsrun_\${OTAGLIST[\${ITAG}]}.py
	rename .root _run\${1}.root *.root
	cd \${INIT_PATH}
	mv \${CMSSW_BASE}/src/MY/PRO/test/*.root .
	cd -
done
# Clean up
rm \${GRIDPACKLIST[0]}
rm \${GRIDPACKLIST[1]}
cd \${INIT_PATH}
# Move the file you need to the initial path
rm \$sandbox_name
EOF
chmod +x ${Wrapper_name}

## step3 set up submit file
echo "set up submit file..."
submit_name=submit.jdl
cat > ${submit_name} <<EOF
Universe = vanilla
Executable = ${Wrapper_name}
should_transfer_files = YES
Arguments=\$(Process)
transfer_input_files = /etc/ciconnect/templates/cmssw_setup.sh
Error = log/job.err.\$(Cluster)-\$(Process)
Output = log/job.out.\$(Cluster)-\$(Process)
Log = log/job.log.\$(Cluster)
when_to_transfer_output = ON_EXIT
# Specify CPU,Memory and Disk
# Default units if not specified:
# Disk: Kb, Memory:Mb
#request_cpus = 1
#request_disk = 100 Mb
#request_memory = 2 Gb
#+JobFlavour = "nextweek"
# Global Pool parameters
+DESIRED_Sites = "T2_US_Nebraska,T2_US_UCSD,T1_US_FNAL"
Queue 2
EOF

## step4 submit the jobs
condor_submit submit.jdl
