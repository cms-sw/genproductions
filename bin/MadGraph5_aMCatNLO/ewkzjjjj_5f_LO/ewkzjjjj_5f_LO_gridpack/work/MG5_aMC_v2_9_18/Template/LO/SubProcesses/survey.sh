#!/bin/bash

# For support of LHAPATH in cluster mode
if [ $CLUSTER_LHAPATH ]; then 
  export LHAPATH=$CLUSTER_LHAPATH;
fi

# Add CVMFS libraries to LD_LIBRARY_PATH, if not present already
if [ -n "$SRT_LD_LIBRARY_PATH_SCRAMRT" ]; then
  if [ -n "${LD_LIBRARY_PATH##*${SRT_LD_LIBRARY_PATH_SCRAMRT}*}" ]; then
    export LD_LIBRARY_PATH="$SRT_LD_LIBRARY_PATH_SCRAMRT:$LD_LIBRARY_PATH"
  fi
fi

if [ -n "$SRT_LD_LIBRARY_PATH_SCRAMRTDEL" ]; then
  if [ -n "${LD_LIBRARY_PATH##*${SRT_LD_LIBRARY_PATH_SCRAMRTDEL}*}" ]; then
    export LD_LIBRARY_PATH="$SRT_LD_LIBRARY_PATH_SCRAMRTDEL:$LD_LIBRARY_PATH"
  fi
fi

# If TMPDIR is unset, set it to the condor scratch area if present and fallback to /tmp
export TMPDIR=${TMPDIR:-${_CONDOR_SCRATCH_DIR:-/tmp}}

if [[ -e MadLoop5_resources.tar.gz && ! -e MadLoop5_resources ]]; then
tar -xzf MadLoop5_resources.tar.gz;
fi

k=run1_app.log;
script=ajob1;                      
offset=$1;
shift;
subdir=$offset;
for i in $@ ; do
     j=G$i;
     if [[ $offset == *.* ]];then
	 subdir=${offset%.*};
	 offset=${offset##*.};
	 j=G${i}_${subdir};     
     elif [[ $offset -gt 0 ]]; then
	 j=G${i}_${subdir};
     fi
     if [[ ! -e $j ]]; then
         mkdir $j;
     fi
     cd $j;
     if [[ $offset -eq 0 ]]; then
	 rm -f ftn25 ftn26 ftn99;
	 rm -f $k;
     else
	 echo   "$offset"  > moffset.dat;
     fi
     if [[ $offset -eq $subdir ]]; then
	 rm -f ftn25 ftn26 ftn99;
	 rm -f $k;
     else
        if [[ -e ../ftn25 ]]; then
	    cp ../ftn25 .;
	fi
     fi
     if [[ ! -e input_app.txt  ]]; then
	 cat ../input_app.txt >& input_app.txt;
     fi
     echo $i >> input_app.txt;

     for((try=1;try<=10;try+=1)); 
     do
     ../madevent 2>&1 >> $k <input_app.txt | tee -a $k;
     status_code=${PIPESTATUS[0]};
     if [ -s $k ]
     then
         break;
     else
     sleep 1;
     fi
     done
#     rm -f ftn25 ftn99
#     rm -f ftn26
     echo "" >> $k; echo "ls status:" >> $k; ls >> $k;
     cp $k log.txt;
# Perform some cleaning to keep less file on disk/transfer less file.
     if [[ $subdir -ne 1 &&  -s results.dat && $MG5DEBUG -ne true ]]; then
	 rm -f ftn25 &> /dev/null
         rm -f ftn26 &> /dev/null
	 rm -f log.txt &> /dev/null
         rm -f *.log &> /dev/null
	 rm -f moffset.dat &> /dev/null
     fi
     if [[ $status_code -ne 0 ]]; then 
	 rm results.dat
	 echo "ERROR DETECTED"
	 echo "end code not correct $status_code" >> log.txt
   echo "+ Hostname:" >> log.txt
   hostname >> log.txt
   echo "+ Printing job environment:" >> log.txt
   env >> log.txt
   cd ../
   exit $status_code
     fi
     cd ../;

 
done;

# Cleaning 



