#! /bin/bash

# Utilities for CMS Connect submission

cmssw_setup() {
    ####################################
    #Setup CMSSW framework
    # Usage: setup_cmssw sandbox.tar.bz2
    ###################################
    #Download and extract sandbox
    SANDBOX_HOME=$PWD
    RELEASE_TMP="cmssw-tmp"
    mkdir $RELEASE_TMP
    cd $RELEASE_TMP
    pwd
    echo "-Untarring ../$1"
    tar xJf "../$1"

    basedir=$PWD
    # Keep track of release sandbox version
    # Warning: This trusts the sandbox name has the format: ${card_name}_output.tar.xz
    # and that the work area created by gridpack_generation.sh is ${card_name}_gridpack
    rel="${1%%_output.tar.*}_gridpack"
    #rel=$(dirname $(tar -tvjf "${SANDBOX_HOME}/$1" | head -1 | awk '{print $NF}'))
    #if [ $rel == "." ]; then
    #    rel=$(basename $(tar -tvjf "${SANDBOX_HOME}/$1" | head -1 | awk '{print $NF}'))
    #fi

    arch="$(ls $rel/.SCRAM/|grep slc)"
    export SCRAM_ARCH="$arch"
    old_release_top=$(awk -F= '/RELEASETOP/ {print $2}' $rel/.SCRAM/slc*/Environment)
    rel_version=$(basename $old_release_top)
    tmp=$basedir/$rel
    
    # Creating new release
    # This is done so e.g CMSSW_BASE and other variables are not hardcoded to the sandbox setting paths
    # which will not exist here
    echo ">>> creating new release $rel_version"
    cd -
    echo scramv1 project -f -n $rel CMSSW $rel_version
    scramv1 project -f -n $rel CMSSW $rel_version
    new_release_top=$(awk -F= '/RELEASETOP/ {print $2}' $rel/.SCRAM/slc*/Environment)
    #echo "--new_release_top = $new_release_top"
    cd $rel
    echo ">>> preparing sandbox release $rel"
    for i in bin lib python src work; do
        rm -rf "$i"
        mv "$basedir/$rel/$i" .
    done
   
    echo ">>> fixing python paths"
    for f in $(find -iname __init__.py); do
        sed -i -e "s@$old_release_top@$new_release_top@" "$f"
    done

    echo ">>> fixing mg5 configurations"
    # Getting old and new MG5 parent directories
    old_mg5_path=$(cat $basedir/_condor_scratch_dir.txt)
    new_mg5_path="$(dirname $SANDBOX_HOME)"

    for f in $(find work -iname "*.txt"); do
        sed -i -e "s@$old_mg5_path@$new_mg5_path@g" "$f"
    done
    for f in $(find work -iname "*.py"); do
        sed -i -e "s@$old_mg5_path@$new_mg5_path@g" "$f"
    done
    for f in $(find work -iname "*.pyo"); do
        sed -i -e "s@$old_mg5_path@$new_mg5_path@g" "$f"
    done
    
    echo ">>> deleting old cmssw framework"
    cd $SANDBOX_HOME
    # Delete old cmssw framework
    rm -rf $basedir

    ## echo "sourcing new release area"
    ## eval $(scramv1 runtime -sh)
    ## cd "$basedir"
    
    echo "[$(date '+%F %T')] wrapper ready"
    # echo "current directory: $PWD"
}
# Usage
# setup_cmssw sandbox-CMSSW_7_2_3-86c7ff0.tar.xz
