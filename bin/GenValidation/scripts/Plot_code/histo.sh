#!/bin/bash

while getopts "q:" opt; do
 case "$opt" in
   q) WHAT=$OPTARG
	;;
 esac
done

if [ -z "$WHAT" ]; then
 echo "histo.sh -q <LOCAL/SUBMIT>";
fi


name="histo"
inDIR="/eos/user/m/melu/TEMP/"
case $WHAT in

 LOCAL)
   PWD=`pwd`
   outDIR=${inDIR}"Output"
   suffix=0
   mkdir -p $outDIR
   
   for f in ${PWD}${inDIR}*.root;do
    python histo.py -i $f -o ${name}_$suffix
    mv $name"_"$suffix".root" $outDIR
    let suffix=suffix+1
   done
   ;;

 SUBMIT)
   suffix=0
   PWD=`pwd`
   queue="tomorrow"
   echo "prepare condor file--->>>"
   OpSysAndVer=`cat /etc/redhat-release`
   if [[ "$OpSysAndVer" == *"SLC"* ]]; then
     OpSysAndVer="SLCern6"
   else
     OpSysAndVer="CentOS7"
   fi

   subDIR=${PWD}"/CondorSub"
   mkdir $subDIR
   outDIR=${PWD}"/Output"
   mkdir $outDIR
   subfile=condor.sub
   if [ -f "$subfile" ]; then
      echo "$subfile already exist, remove it"
      rm $subfile
   fi
   echo "executable = $subDIR/\$(cfgFile).sh">>$subfile
   echo "output = $subDIR/output.out">>$subfile
   echo "error = $subDIR/output.err">>$subfile
   echo "log = $subDIR/output.log">>$subfile
   echo "requirements = (OpSysAndVer =?= \""$OpSysAndVer"\")">>$subfile
   echo "+JobFlavour = "$queue"">>$subfile
  
   for f in $(ls ${inDIR} | grep .root);do
      temp=$f
      temp1=${temp%.root}
      read -r -a prename <<< "$temp1"
      echo "cfgFile=${prename}">>$subfile
      echo "queue 1">>$subfile
      exefile=$subDIR\/$prename.sh
      echo "#!/bin/bash">>$exefile
      echo "WORKDIR=\`pwd\`">>$exefile
      echo "echo \"Working directory is \$WORKDIR\"">>$exefile
      echo "python ${PWD}/histo.py -i ${inDIR}$f -o ${name}_$suffix">>$exefile
      echo "mv -v \$WORKDIR/${name}_$suffix.root $outDIR">>$exefile
	  let suffix=suffix+1
   done
   condor_submit $subfile
   ;;
esac
