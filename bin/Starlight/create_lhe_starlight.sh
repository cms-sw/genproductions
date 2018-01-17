#!/bin/sh

#set -o verbose
EXPECTED_ARGS=17
rebuildFromSource=true
runLocally=true
if [ $runLocally == false ]; then
EXPECTED_ARGS=$((EXPECTED_ARGS-2))
fi

if [ $# -ge $EXPECTED_ARGS ]
then
  if [ $runLocally == true ]; then 
  echo "For local running, Usage: `basename $0` Production_type Channel Beam1_Z Beam2_Z Beam1_A Beam2_A Beam1_Gamma Beam2_Gamma Eta_cut EtaMin EtaMax BreakupMode Nevents RandomSeed "
  else echo "For gridpack production, Usage: `basename $0` Production_type Channel Beam1_Z Beam2_Z Beam1_A Beam2_A Beam1_Gamma Beam2_Gamma Eta_cut EtaMin EtaMax BreakupMode"
  fi
  echo "Production type can range from 1-4; available channels can be found on https://starlight.hepforge.org/"  
  exit 1
fi

#setting all default values...
prodType=${1:-5}
channel=${2:-11}
Beam1Z=${3:-82}
Beam2Z=${4:-82}
Beam1A=${5:-208}
Beam2A=${6:-208}
Beam1Gamma=${7:-2560.0}
Beam2Gamma=${8:-2560.0}
BreakupMode=${9:-5}
EtaCut=${10:-0}
EtaMin=${11:--10}
EtaMax=${12:-10}
PtCut=${13:-0}
PtMin=${14:-1.0}
PtMax=${15:-3.0}
nevts=${16:-10000}
seed=${17:-121212}

echo "   ______________________________________     "
echo "         Running Starlight...                 "
echo "   ______________________________________     "

if [ $runLocally == true ]; then
echo "Generating $nevts events with production type $prodType, channel $channel"
echo "Random seed set to $seed"
else
echo "Generating GRIDPACK with production type $prodType, channel $channel"
fi
echo "Beam1: Z=$Beam1Z, A=$Beam1A, Gamma=$Beam1Gamma;  Beam2: Z=$Beam2Z, A=$Beam2A, Gamma=$Beam2Gamma"
echo "BreakupMode: $BreakupMode"
if [ $EtaCut == 0 ]; then echo "Eta Cuts OFF"
else echo "Eta Cuts ON from $EtaMin to $EtaMax"
fi
if [ $PtCut == 0 ]; then echo "Pt Cuts OFF"
else echo "Pt Cuts ON from $PtMin to $PtMax"
fi

if [ ${CMSSW_BASE} == "" ]; then
echo "CMSSW_BASE not found! Please run cmsenv first!"
exit 1
fi

if [ $rebuildFromSource == true ]; then
echo "rebuilding starlight from source..."
else
echo "using precompiled starlight setup..."
fi
if [ $runLocally == true ]; then
echo "will run starlight locally. Please turn off the \"runLocally\" flag for gridpack production"
else
echo "Just producing a starlight gridpack..."
fi

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

#first get DPMJet... need fpe.c from starlight!
#wget...
#svn co http://starlight.hepforge.org/svn/trunk
#mv trunk starlightTrunk
#tar -xvf dpmjet3.0-5.tar
#mv dpmjet3.0-5 dpmjetV305
#cp starlightTrunk/external/fpe.c dpmjetV305
#cd dpmjetV305
#make
#cd ..
#export DPMJETDIR="$(pwd)/dpmjetV305"
#export FLUPRO="$(pwd)/fluka"

# # retrieve the latest Starlight version from SVN if you want
if [ $rebuildFromSource == true ]
then
   svn co http://starlight.hepforge.org/svn/trunk
   mv trunk starlightTrunk
   tar -xvf dpmjet3.0-5.tar
   mv dpmjet3.0-5 dpmjetV305
   cp phojet1.12-35c4.f dpmjetV305
   cp dpmjetMakefile dpmjetV305/Makefile 
   cp starlightTrunk/external/fpe.c dpmjetV305
   cd dpmjetV305
   make
   gcc fpe.c -Wall -g -c
   make
   cd ..
   export DPMJETDIR="$(pwd)/dpmjetV305"
   cd starlightTrunk
   mkdir -p build
   cd build
   cmake .. -DENABLE_DPMJET=ON
   gmake
fi

#or get the precompiled version on lxplus
if [ $rebuildFromSource == false ]
then
  #wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${repo}/${name}.tar.gz  -O ${name}.tar.gz
  #tar xf ${name}.tar
  cd starlightTrunk/build
fi

#get the standard slight.in template
#if [ $rebuildFromSource == true ]; then
cat > slightTemplate.in <<EOFILE
baseFileName = slight   #suite of output files will be saved with this base name
BEAM_1_Z = B1Z    #Z of projectile
BEAM_1_A = B1A   #A of projectile
BEAM_2_Z = B2Z   #Z of target
BEAM_2_A = B2A   #A of target
BEAM_1_GAMMA = B1G #Gamma of the colliding ion 1
BEAM_2_GAMMA = B2G #Gamma of the colliding ion 2
W_MAX = -1   #Max value of w
W_MIN = -1    #Min value of w
W_N_BINS = 50    #Bins i w
RAP_MAX = 9.    #max y
RAP_N_BINS = 200    #Bins i y
CUT_PT = PtCut #Cut in pT? 0 = (no, 1 = yes)
PT_MIN = PtMin #Minimum pT in GeV
PT_MAX = PtMax #Maximum pT in GeV
CUT_ETA = EtaCut #Cut in pseudorapidity? (0 = no, 1 = yes)
ETA_MIN = EtaMin #Minimum pseudorapidity
ETA_MAX = EtaMax #Maximum pseudorapidity
PROD_MODE = PRODMODE     #gg or gP switch (1 = 2-photon, 2 = coherent vector meson (narrow), 3 = coherent vector meson (wide), 4 = incoherent vector meson)
N_EVENTS = NEVT   #Number of events
PROD_PID = PRODUCTIONCHANNEL   #Channel of interest; this is j/psi --> mu+ mu-
RND_SEED = RNDSEED
BREAKUP_MODE = BreakupMode     #Controls the nuclear breakup; a 5 here makes no requirement on the breakup of the ions
INTERFERENCE = 0     #Interference (0 = off, 1 = on)
IF_STRENGTH = 1.    #% of intefernce (0.0 - 0.1)
INT_PT_MAX = 0.24  #Maximum pt considered, when interference is turned on
INT_PT_N_BINS =120   #Number of pt bins when interference is turned on
XSEC_METHOD = 1 # Set to 0 to use old method for calculating gamma-gamma luminosity
PYTHIA_FULL_EVENTRECORD = 0 # Write full pythia information to output (vertex, parents, daughter etc).  

EOFILE

#fi

if [ $runLocally == true ]; then
cat slightTemplate.in | sed -e "s#RNDSEED#${seed}#g" | sed -e "s#NEVT#${nevts}#g" | sed -e "s#PRODUCTIONCHANNEL#${channel}#g" | sed -e "s#PRODMODE#${prodType}#g" | sed -e "s#B1Z#${Beam1Z}#g" | sed -e "s#B2Z#${Beam2Z}#g" | sed -e "s#B1A#${Beam1A}#g" | sed -e "s#B2A#${Beam2A}#g" | sed -e "s#B1G#${Beam1Gamma}#g" | sed -e "s#B2G#${Beam2Gamma}#g" | sed -e "s#PtCut#${PtCut}#g" | sed -e "s#PtMin#${PtMin}#g" | sed -e "s#PtMax#${PtMax}#g" | sed -e "s#EtaCut#${EtaCut}#g" | sed -e "s#EtaMin#${EtaMin}#g" | sed -e "s#EtaMax#${EtaMax}#g" | sed -e "s#BreakupMode#${BreakupMode}#g" > slight.in
fi

cat slightTemplate.in | sed -e "s#PRODUCTIONCHANNEL#${channel}#g" | sed -e "s#PRODMODE#${prodType}#g" | sed -e "s#B1Z#${Beam1Z}#g" | sed -e "s#B2Z#${Beam2Z}#g" | sed -e "s#B1A#${Beam1A}#g" | sed -e "s#B2A#${Beam2A}#g" | sed -e "s#PtCut#${PtCut}#g" | sed -e "s#PtMin#${PtMin}#g" | sed -e "s#PtMax#${PtMax}#g" | sed -e "s#EtaCut#${EtaCut}#g" | sed -e "s#EtaMin#${EtaMin}#g" | sed -e "s#EtaMax#${EtaMax}#g" | sed -e "s#BreakupMode#${BreakupMode}#g" > slightTemplateForNextProd.in


cd ../..

cp starlightTrunk/config/my.input starlightTrunk/build
cp $DPMJETDIR/dpmjet.dat starlightTrunk/build

#tar -czf slightGridpack_ProdType${prodType}_Channel${channel}.tgz starlightTrunk

if [ $runLocally == true ]; then
echo "*** STARTING STARLIGHT PRODUCTION ***"
cd starlightTrunk/build
if [ $prodType -ge 4 ]; then
./starlight < my.input &> log_${prodType}_${channel}_${seed}.txt
else
./starlight &> log_${prodType}_${channel}_${seed}.txt
fi
cp slight.out ${WORKDIR}/.
cd ${WORKDIR}
echo "***STARLIGHT COMPLETE***"

#now convert the starlight file to a LHE file
#curl https://raw.githubusercontent.com/kurtejung/genproductions/starlight_dev/bin/Starlight/convert_SL2LHE.C > convert_SL2LHE.C
root -l -b << EOF
.x convert_SL2LHE.C+(1,"slight.out","slight_${prodType}_${channel}_${seed}",${Beam1Gamma},${Beam2Gamma})
.q
EOF
echo "*** LHE CONVERSION COMPLETE ***"

echo "Output ready with log_${prodType}_${channel}_${seed}.txt and slight_${prodType}_${channel}_${seed}_GEN.root at `pwd` and $WORKDIR"
fi
echo "End of job on " `date`
exit 0
