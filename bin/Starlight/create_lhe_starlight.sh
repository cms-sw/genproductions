#!/bin/sh

#set -o verbose
EXPECTED_ARGS=4
rebuildFromSource=true
runLocally=false
if [ $runLocally == false ]; then
EXPECTED_ARGS=$((EXPECTED_ARGS-2))
fi

if [ $# -ne $EXPECTED_ARGS ]
then
  if [ $runLocally == true ]; then 
  echo "For local running, Usage: `basename $0` Production_type Channel Nevents RandomSeed "
  else echo "For gridpack production, Usage: `basename $0` Production_type Channel "
  fi
  echo "Production type can range from 1-4; available channels can be found on https://starlight.hepforge.org/"  
  exit 1
fi

echo "   ______________________________________     "
echo "         Running Starlight...                 "
echo "   ______________________________________     "

if [ $runLocally == true ]; then
echo "Generating $3 events with production type $1, channel $2"
echo "Random seed set to $4"
else
echo "Generating GRIDPACK with production type $1, channel $2"
fi

if [ $CMSSW_BASE == "" ]; then
echo "CMSSW_BASE not found! Please run cmsenv first!"
exit 1
fi


rebuildFromSource=true
runLocally=false

if [ $rebuildFromSource == true ]; then
echo "rebuilding starlight from source..."
else
echo "using precombiled starlight setup..."
fi
if [ $runLocally == true ]; then
echo "will run starlight locally. Please disable for gridpack prod only"
else
echo "Just producing a starlight gridpack..."
fi

prodType=${1}
channel=${2}
nevts=${3}
seed=${4}

name="starlightProd"

# Release to be used to define the environment and the compiler needed
export PRODHOME=`pwd`
export RELEASE=${CMSSW_VERSION}
export WORKDIR=`pwd`

# initialize the CMS environment 
#scram project -n ${name} CMSSW ${RELEASE} ; cd ${name} ; mkdir -p work ; cd work
cd ${CMSSW_BASE}
eval `scram runtime -sh`
cd -

# # retrieve the latest Starlight version from SVN if you want
if [ $rebuildFromSource == true ]
then
   svn co http://starlight.hepforge.org/svn/trunk
   mv trunk starlightTrunk
   cd starlightTrunk
   mkdir -p build
   cd build
   cmake ..
   gmake
fi

#or get the precompiled version on lxplus
if [ $rebuildFromSource == false ]
then
  #wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${repo}/${name}.tar.gz  -O ${name}.tar.gz
  tar xf ${name}.tar
  cd starlightTrunk/build
fi

#get the standard slight.in template
#if [ $rebuildFromSource == true ]; then
cat > slightTemplate.in <<EOFILE
baseFileName = slight   #suite of output files will be saved with this base name
BEAM_1_Z = 82    #Z of projectile
BEAM_1_A = 208   #A of projectile
BEAM_2_Z = 82   #Z of target
BEAM_2_A = 208   #A of target
BEAM_1_GAMMA = 1470.0 #Gamma of the colliding ion 1
BEAM_2_GAMMA = 1470.0 #Gamma of the colliding ion 2
W_MAX = -1   #Max value of w
W_MIN = -1    #Min value of w
W_N_BINS = 50    #Bins i w
RAP_MAX = 9.    #max y
RAP_N_BINS = 200    #Bins i y
CUT_PT = 0 #Cut in pT? 0 = (no, 1 = yes)
PT_MIN = 1.0 #Minimum pT in GeV
PT_MAX = 3.0 #Maximum pT in GeV
CUT_ETA = 0 #Cut in pseudorapidity? (0 = no, 1 = yes)
ETA_MIN = -10 #Minimum pseudorapidity
ETA_MAX = 10 #Maximum pseudorapidity
PROD_MODE = PRODMODE     #gg or gP switch (1 = 2-photon, 2 = coherent vector meson (narrow), 3 = coherent vector meson (wide), 4 = incoherent vector meson)
N_EVENTS = NEVT   #Number of events
PROD_PID = PRODUCTIONCHANNEL   #Channel of interest; this is j/psi --> mu+ mu-
RND_SEED = RNDSEED
BREAKUP_MODE = 5     #Controls the nuclear breakup; a 5 here makes no requirement on the breakup of the ions
INTERFERENCE = 0     #Interference (0 = off, 1 = on)
IF_STRENGTH = 1.    #% of intefernce (0.0 - 0.1)
INT_PT_MAX = 0.24  #Maximum pt considered, when interference is turned on
INT_PT_N_BINS =120   #Number of pt bins when interference is turned on
XSEC_METHOD = 1 # Set to 0 to use old method for calculating gamma-gamma luminosity
PYTHIA_FULL_EVENTRECORD = 0 # Write full pythia information to output (vertex, parents, daughter etc).  

EOFILE

#fi

if [ $runLocally == true ]; then
cat slightTemplate.in | sed -e "s#RNDSEED#${seed}#g" | sed -e "s#NEVT#${nevts}#g" | sed -e "s#PRODUCTIONCHANNEL#${channel}#g" | sed -e "s#PRODMODE#${prodType}#g" > slight.in
else
cat slightTemplate.in | sed -e "s#PRODUCTIONCHANNEL#${channel}#g" | sed -e "s#PRODMODE#${prodType}#g" > slight.in
fi

cd ../..

tar -czf slightGridpack_ProdType${prodType}_Channel${channel}.tgz starlightTrunk convert_SL2LHE.C

if [ $runLocally == true ]
then
echo "*** STARTING STARLIGHT PRODUCTION ***"
./starlight &> log_${prodType}_${channel}_${seed}.txt
#remove the spurious random seed output that is non LHE standard 
cp slight.out ${WORKDIR}/.
cd ${WORKDIR}
echo "***STARLIGHT COMPLETE***"

#now convert the starlight file to a HepMC file
#curl https://raw.githubusercontent.com/kurtejung/genproductions/starlight_dev/bin/Starlight/convert_SL2LHE.C > convert_SL2LHE.C
root -l -b << EOF
.x convert_SL2LHE.C+(1,"slight.out","slight_${prodType}_${channel}_${seed}") 
.q
EOF
echo "*** LHE CONVERSION COMPLETE ***"

#optionally convert the hepMC file to a gen file
#curl https://raw.githubusercontent.com/cms-sw/cmssw/master/IOMC/Input/test/hepmc2gen.py > hepmc2gen.py
#mv hepmc2gen.py hepmc2genOrig.py
#cat hepmc2genOrig.py | sed -e "s/if True/if False/g" | sed -e "/file:\/tmp/c\     fileNames = cms.untracked.vstring(options.inputFiles[0])," | sed -e "s/'HepMC_GEN.root'/options.outputFile/g" > hepmc2gen.py
#rm hepmc2genOrig.py
#echo "Converting slight_${prodType}_${channel}_${seed}.hepmc0.hepmc to gen file..."
#cmsRun hepmc2gen.py maxEvents=-1 inputFiles=file:slight_${prodType}_${channel}_${seed}.hepmc0.hepmc outputFile=slight_${prodType}_${channel}_${seed}_GEN.root
#echo "*** GEN CONVERSION COMPLETE ***"

echo "Output ready with log_${prodType}_${channel}_${seed}.txt and slight_${prodType}_${channel}_${seed}_GEN.root at `pwd` and $WORKDIR"
fi
echo "End of job on " `date`
exit 0
