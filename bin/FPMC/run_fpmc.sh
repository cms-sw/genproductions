#! /bin/bash

#
# Description:
# Bash script to install and compile FPMC event generator
# and setup the necessary files to produce an event sample
# in LXBATCH.
#
# If argument "run" is True, this script will produce
# samples with ALL the cards present at the cards folder.
#
# The necessary arguments are set within the lines below,
# which are:
# 
# 1 -- carddir: folder containing the input cards for FPMC (e.g. cards/)
# 2 -- name: any label for the working directory
# 3 -- queue: LXBATH queue (e.g. 2nd for quick production)
# 4 -- run?: if the user wants to produce the event samples or
#            just setup FPMC
#
ARGS=4
RUN=False

RED='\033[0;31m'
NC='\033[0m'
GREEN='\033[0;32m'

# Checking necessary arguments:
if [ $# -ne $ARGS ];
then
  printf "${RED}You must set the following arguments:${NC}\n"
  printf "${RED}1 -- carddir: path to directory with input cards to run${NC}\n"
  printf "${RED}2 -- name: label for the working directory${NC}\n"
  printf "${RED}3 -- queue: LXBATH queue${NC}\n"
  printf "${RED}4 -- run?: whether you want to produce a sample via LXBATCH (True) or just setup FPMC (False)${NC}\n"
  printf "${RED}Example: ./run_fpmc.sh cards workdir 2nd True${NC}\n"
  exit
else
  #Screen output:
  echo "Set options:"
  echo "Cards dir = $1"
  echo "Label work dir = fpmc/build/$2"
  echo "LXBATCH queue = $3"
  echo "Run jobs? = $4"
fi

# Parsing arguments:
carddir=$1
name=$2
queue=$3
run=$4

#Start downloding FPMC source code and compiling it:
basedir=$(pwd)

#Proper GCC environment:
source /cvmfs/sft.cern.ch/lcg/external/gcc/6.1.0/x86_64-slc6/setup.sh

#Saving previous FPMC builds:
if [ -d "fpmc" ];
then
  mv fpmc fpmc_"$(date +"%Y-%m-%d_%H:%M:%S")"
fi
  
#Start fresh new FPMC area:
printf "Cloning git repo of FPMC event generator: \n"
git clone https://github.com/fpmc-hep/fpmc.git
printf "Wait ... \n"
cd fpmc/
mkdir $name
cd $name
cmake ..
printf "Compiling FPMC... \n"
make
printf "${GREEN}FPMC is ready${NC}\n"

#Get cards in cards/:
arr=(`ls $basedir/$carddir/* | xargs -n 1 basename`)
echo ""
echo "Cards ready for production:"
for ((i=0; i<${#arr[@]}; i++));
do
  echo ${i}" : "${arr[$i]}
done
echo ""

#Run jobs:
if [ "$run" = True ];
then
  printf "You chose to generate events:\n"
  printf "Preperaing scripts to be submitted to LXBATCH...\n"
  for ((i=0; i<${#arr[@]}; i++));
  do
    jobfile="${arr[$i]}"
    ecms=`awk '/ECMS/ { print $0 } ' $basedir/$carddir/$jobfile | cut -d ' ' -f9`
    printf "The ECMS is ${ecms}\n"
    nevt=`awk '/MAXEV/ { print $0 } ' $basedir/$carddir/$jobfile | cut -d ' ' -f8`
    printf "The #events requested is ${nevt}\n"
    touch ${jobfile}.sh
    > ${jobfile}.sh
    echo "#!/bin/bash" >> ${jobfile}.sh
    echo "" >> ${jobfile}.sh
    echo "source /cvmfs/sft.cern.ch/lcg/external/gcc/6.1.0/x86_64-slc6/setup.sh" >> ${jobfile}.sh
    echo "cd ${basedir}/fpmc/$name/" >> ${jobfile}.sh
    echo "./fpmc-lhe < $basedir/$carddir/${arr[$f]}" >> ${jobfile}.sh
    chmod +x ${jobfile}.sh
    printf "Submitting job... \n"
    bsub -q $queue -o $basedir/fpmc/$name/${jobfile}.out -e $basedir/fpmc/$name/${jobfile}.err $basedir/fpmc/$name/${jobfile}.sh
  done;
else
  echo "Not generating events."
  echo "Executable fpmc-lhe is available at fpmc/build";
fi
