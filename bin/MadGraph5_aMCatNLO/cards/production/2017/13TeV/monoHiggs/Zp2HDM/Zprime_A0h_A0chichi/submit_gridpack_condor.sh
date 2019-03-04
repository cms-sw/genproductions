#!/bin/bash

name=${1}
carddir=${2}
folder=`pwd`

echo ${name} ${carddir} ${folder} > file_args.txt

condor_submit submit_gridpack.sub
