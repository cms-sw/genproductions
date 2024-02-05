#!/bin/bash

# SCRIPT TO TEST COMPILATION OF POWHEG PROCESSES CONTAINED IN A GIVEN SOURCE TARBALL
# THE SCRIPT IS MEANT TO BE RUN ON CONDOR
# THE OUTPUT IS A LOG FILE COLLECTING ALL THE COMPILATION LOGs, TO APPEAR IN THE WORKING DIR

EXPECTED_ARGS=3
if [ $# -ne $EXPECTED_ARGS ]
then
    echo "Usage: `basename $0` source scram_arch CMSSW_version"
    echo "Example: sh `basename $0` powhegboxV2_rev3429_date20170726.tar.gz slc6_amd64_gcc630 CMSSW_9_3_0"
    exit 1
fi

echo "   ______________________________________________________    "
echo "         Running Powheg script "$(basename "$0")"                          "
echo "   ______________________________________________________    "

source_name=$1
source_name="${source_name%.*}"
source_name="${source_name%.*}"

##################################################################################
##################################################################################

# ONE CAN DEFINE A (SUB)SET OF PROCESSES TO TEST DEFINING THE ARRAY "processes" 
# IF THE VARIABLE "processes" IS UNSET (i.e. COMMENTED), 
# THE SCRIPT IT WILL LOOP ON ALL THE .tgz FILES IN THE SOURCE TARBALL

# processes=('DMGG' 'DMS' 'DMS_tloop' 'DMV' 'HJ' 'HJJ' 'HW' 'HWJ' 'HZ' 'HZJ' \
           # 'ST_sch' 'ST_tch' 'ST_tch_4f' 'ST_wtch_DR' 'ST_wtch_DS' 'VBF_H' 'VBF_HJJJ' \
           # 'VBF_Wp_Wm' 'VBF_Z' 'VBF_Z_Z' 'W' 'W2jet' 'WW' 'WZ' 'W_ew-BMNNP' 'Wbb_dec' \
           # 'Wbbj' 'Wgamma' 'Wj' 'Wp_Wp_J_J' 'Z' 'Z2jet' 'ZZ' 'Z_ew-BMNNPV' 'Zj' 'bbH' \
           # 'dijet' 'dislepton-jet' 'disquark' 'ggHH' 'ggHZ' 'gg_H' 'gg_H_2HDM' 'gg_H_MSSM' \
           # 'gg_H_quark-mass-effects' 'hvq' 'trijet' 'ttH' 'ttb_NLO_dec' 'ttb_dec' 'vbf_wp_wp' 'weakinos');

#processes=('Wbbj' 'Wbb_dec' 'trijet' 'HZJ' 'ggHH' 'gg_H' 'disquark');

##################################################################################
##################################################################################

source_dir=/afs/cern.ch/cms/generators/www/slc6_amd64_gcc481/powheg/V2.0/src/$1

# check whether the script needs to run on all the processes 
# or on a (sub)set defined in the variable "processes"
if [ -z "$processes" ]; then 
    process_list=`tar -tvf $source_dir 'POWHEG-BOX/*.tgz' | awk '{print $6}'`
    process_list=$(echo $process_list | sed 's/POWHEG\-BOX\///g')
    process_list=$(echo $process_list | sed 's/\.tgz//g')
else 
    process_list=$(printf " %s" "${processes[@]}")
    process_list=${process_list:1}
fi

process_list=`echo "$process_list"` # DYNNLOPS is not meant to be compiled

echo "PROCESS LIST: "${process_list}

for file in ${process_list}
do
    process="${file%.*}"
    cat << EOF > source_compilation_${process}_$2_$3.sh
#!/bin/bash

# Define a few variables
process=${process}
genproduction_dir=$PWD/../../..
topdir=$PWD
source_file=$1
source_dir=/afs/cern.ch/cms/generators/www/slc6_amd64_gcc481/powheg/V2.0/src/$1
scram_arch_version=$2
cmssw_version=$3
workdir=test

EOF

    cat << 'EOF' >> source_compilation_${process}_$2_$3.sh

echo "source file: "$source_dir

source_name=$(basename "$source_file")
source_name="${source_name%.*}"
source_name="${source_name%.*}"

# store the lxplus node to retrieve the output in case of disconnection
#hostname > lxplus_node.log

# Download the CMSSW release
mkdir -p $workdir
cd $workdir
export SCRAM_ARCH=${scram_arch_version}
scramv1 project CMSSW ${cmssw_version}
cd ${cmssw_version}/src
eval `scramv1 runtime -sh`
echo "PDF REPOSITORY/VERSION: "${LHAPDF_DATA_PATH}

# Copy the POWHEG scripts
cp    $genproduction_dir/bin/Powheg/*.py .
cp    $genproduction_dir/bin/Powheg/*.sh .
cp -r $genproduction_dir/bin/Powheg/patches .
cp -r $genproduction_dir/bin/Powheg/Templates .
cp -r $genproduction_dir/bin/Powheg/Utilities .

# Replace the standard source tarball with the one passed as argument to the script
sed -i "/export\ POWHEG_SOURCE\=/c export\ POWHEG_SOURCE\=${source_file}" run_pwg_condor.py
sed -i "/export\ POWHEGRES_SOURCE\=/c export\ POWHEGRES_SOURCE\=${source_file}" run_pwg_condor.py

# copy random powheg.input (since only the compilation is tested, this should be OK)
cp ${genproduction_dir}/bin/Powheg/examples/V2/gg_H_quark-mass-effects_NNPDF30_13TeV/gg_H_quark-mass-effects_NNPDF30_13TeV.input powheg.input

rm ${topdir}/compile_report_-_${process}_-_${scram_arch_version}_-_${cmssw_version}.log

# Loop on the processes, compile and fetch the last lines of the compilation log
echo "compiling ${process}"
echo ${PWD}
echo "python3 ./run_pwg_condor.py -p 0 -i powheg.input -m ${process} -f my_${process} -d 1"
python3 ./run_pwg_condor.py -p 0 -i powheg.input -m ${process} -f my_${process} -d 1
echo "=========== LAST 30 COMPILATION LINES FOR PROCESS ${process} ===========" >> ${topdir}/compile_report_-_${process}_-_${scram_arch_version}_-_${cmssw_version}.log
echo "" >> ${topdir}/compile_report_-_${process}_-_${scram_arch_version}_-_${cmssw_version}.log
tail -n 30 run_src_my_${process}.log >> ${topdir}/compile_report_-_${process}_-_${scram_arch_version}_-_${cmssw_version}.log
echo "" >> ${topdir}/compile_report_-_${process}_-_${scram_arch_version}_-_${cmssw_version}.log
rm -rf my_${process}

EOF

    cat << EOF > condor_${process}_$2_$3.sub

executable              = source_compilation_${process}_$2_$3.sh
output                  = \$(ClusterId).\$(ProcId).out
error                   = \$(ClusterId).\$(ProcId).err
log                     = \$(ClusterId).log
+JobFlavour             = "testmatch"

should_transfer_files = YES
when_to_transfer_output = ON_EXIT

Queue 1

EOF

    condor_submit condor_${process}_$2_$3.sub
done

#echo "If you want, select some specific processes in source_compilation_${source_name}_$2_$3.sh"
#echo "When ready run: \"condor_submit condor_${source_name}_$2_$3.sub\" "
