#!/bin/bash
fail_exit() { echo "$@" 1>&2; exit 1; }

#set -o verbose                                                                                                                                                                   
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

# processes=('DMGG' 'DMS' 'DMS_tloop' 'DMV' 'DYNNLOPS' 'HJ' 'HJJ' 'HW' 'HWJ' 'HZ' 'HZJ' \
           # 'ST_sch' 'ST_tch' 'ST_tch_4f' 'ST_wtch_DR' 'ST_wtch_DS' 'VBF_H' 'VBF_HJJJ' \
           # 'VBF_Wp_Wm' 'VBF_Z' 'VBF_Z_Z' 'W' 'W2jet' 'WW' 'WZ' 'W_ew-BMNNP' 'Wbb_dec' \
           # 'Wbbj' 'Wgamma' 'Wj' 'Wp_Wp_J_J' 'Z' 'Z2jet' 'ZZ' 'Z_ew-BMNNPV' 'Zj' 'bbH' \
           # 'dijet' 'dislepton-jet' 'disquark' 'ggHH' 'ggHZ' 'gg_H' 'gg_H_2HDM' 'gg_H_MSSM' \
           # 'gg_H_quark-mass-effects' 'hvq' 'trijet' 'ttH' 'ttb_NLO_dec' 'ttb_dec' 'vbf_wp_wp' 'weakinos');

processes=('Wbbj' 'Wbb_dec' 'trijet' 'HZJ' 'ggHH' 'gg_H' 'disquark');

genproduction_dir=`awk -F genproductions '{print $1}' <<< $PWD`"genproductions"
topdir=$PWD
source_file="/afs/cern.ch/cms/generators/www/slc6_amd64_gcc481/powheg/V2.0/src/"${1}
echo $source_file
scram_arch_version=$2
cmssw_version=$3
# workdir=$topdir/temp_$prefix
workdir=/tmp/$USER/"${0%.*}"

source_name=$(basename "$source_file")
source_name="${source_name%.*}"
source_name="${source_name%.*}"

# if [[ -e ${workdir} ]]; then
  # fail_exit "The directory ${workdir} exists! Please clean up your work directory before running!!"
# fi

hostname > lxplus_node.log

mkdir -p $workdir
cd $workdir
export SCRAM_ARCH=${scram_arch_version}
scramv1 project CMSSW ${cmssw_version}
cd ${cmssw_version}/src
eval `scramv1 runtime -sh`
echo ${LHAPDF_DATA_PATH}

# cd $workdir
cp    $genproduction_dir/bin/Powheg/*.py .
cp    $genproduction_dir/bin/Powheg/*.sh .
cp -r $genproduction_dir/bin/Powheg/patches .

sed -i "/export\ POWHEGSRC\=/c export\ POWHEGSRC\=$1" run_pwg.py

# tar xf $source_file
# cd POWHEG-BOX
# for file in $(ls *.tgz)
# do
    # process="${file%.*}"
    # echo "unpacking $process"
    # tar xf ${file}
# done

process_list=`tar -tvf $source_file 'POWHEG-BOX/*.tgz' | awk '{print $6}'`
process_list=$(echo $process_list | sed 's/POWHEG\-BOX\///g')
process_list=$(echo $process_list | sed 's/\.tgz//g')
echo ${process_list}
# process_list="Z"


# touch powheg.input
cp ${genproduction_dir}/bin/Powheg/examples/V2/Z_ee_NNPDF30_13TeV/Z_ee_NNPDF30_13TeV.input powheg.input

for file in ${process_list}
do
    process="${file%.*}"
    echo "compiling $process"
    echo ${PWD}
    echo "python ./run_pwg.py -p 0 -i powheg.input -m ${process} -f my_${process} -d 1"
    python ./run_pwg.py -p 0 -i powheg.input -m ${process} -f my_${process} -d 1
    # rm -rf my_${process}
    # exit 1
    # # cd $workdir/POWHEG-BOX/$process
    # # make 2>&1 | tee ../../${process}_compile_${scram_arch_version}_${cmssw_version}.log
    # # echo "=========== FINAL COMPILATION LINES FOR PROCESS ${process} ===========" >> ../../compile_report_${source_name}_${scram_arch_version}_${cmssw_version}.log
    # # tail ../../${process}_compile_${scram_arch_version}_${cmssw_version}.log >> ../../compile_report_${source_name}_${scram_arch_version}_${cmssw_version}.log
    
done
