export name=$folderName
export cardInput=$powInputName
export process=$process
export noPdfCheck=$noPdfCheck
export WORKDIR=$rootfolder
export patches_dir=$patches_dir 
# Release to be used to define the environment and the compiler needed
export RELEASE=$${CMSSW_VERSION}
export jhugenversion="v7.2.7" 

cd $$WORKDIR
pwd

mkdir -p $${name} ; 

cd $${name}
export PATH=`pwd`:$${PATH}

## Get the input card
#wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/$${cardinput} -O powheg.input  || cp -p $${cardinput} powheg.input || fail_exit "Failed to get powheg input card " $${card}

if [ -s ../$${cardInput} ]; then
  cp -p ../$${cardInput} powheg.input
fi

is5FlavorScheme=$isFiveFlavor
defaultPDF=$defaultPDF

if [[ $$is5FlavorScheme -eq 1 ]]; then
  echo "INFO: The process $$process uses the 5F PDF scheme"
else
  echo "INFO: The process $$process uses the 4F PDF scheme"
fi

forDYNNLOPS=$forDYNNLOPS

cd $$WORKDIR
cd $${name}
python ../make_rwl.py $${is5FlavorScheme} $${defaultPDF} $${forDYNNLOPS}

if [ -s ../JHUGen.input ]; then
  cp -p ../JHUGen.input JHUGen.input
fi

sed -i -e "s#--#-#g" powheg.input

myDir=`pwd`
card=$${myDir}/powheg.input
cardj=$${myDir}/JHUGen.input

## Try to get the JHUGen card
#wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/$${usejhugen} -O JHUGen.input || cp -p $${usejhugen} JHUGen.input

jhugen=0
if [[ -s ./JHUGen.input ]]; then
  jhugen=$$(expr $$jhugen + 1)
  echo "JHUGen activated!"
  #for decay weights in H->WW and H->ZZ
  wget --no-verbose https://github.com/cms-sw/genproductions/raw/7261679b0fd6464b80479c075b10f9ba1932ab9a/bin/JHUGen/Pdecay/PMWWdistribution.out 
  wget --no-verbose https://github.com/cms-sw/genproductions/raw/7261679b0fd6464b80479c075b10f9ba1932ab9a/bin/JHUGen/Pdecay/PMZZdistribution.out 

fi

### retrieve the powheg source tar ball
export POWHEGSRC=$powhegSrc 

echo 'D/L POWHEG source...'

if [ ! -f $${POWHEGSRC} ]; then
  wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/powheg/V2.0/src/$${POWHEGSRC} || fail_exit "Failed to get powheg tar ball "
fi
#cp -p ../$${POWHEGSRC} .

tar zxf $${POWHEGSRC}

# increase maxseeds to 10000
sed -i -e "s#par_maxseeds=200,#par_maxseeds=10000,#g" POWHEG-BOX/include/pwhg_par.h

if [ -e POWHEG-BOX/$${process}.tgz ]; then
  cd POWHEG-BOX/
  tar zxf $${process}.tgz
  cd -
fi

patch -l -p0 -i ${patches_dir}/pdfweights.patch
patch -l -p0 -i ${patches_dir}/pwhg_lhepdf.patch

$patch_1 

sed -i -e "s#500#1350#g"  POWHEG-BOX/include/pwhg_rwl.h

echo $${POWHEGSRC} > VERSION

cd POWHEG-BOX/$${process}

# This is just to please gcc 4.8.1
mkdir -p include

$patch_2 

# Use dynamic linking and lhapdf
sed -i -e "s#STATIC[ \t]*=[ \t]*-static#STATIC=-dynamic#g" Makefile
sed -i -e "s#PDF[ \t]*=[ \t]*native#PDF=lhapdf#g" Makefile
sed -i -e "s# -lLHAPDF# -lLHAPDF \$$(RPATHLIBS)#g" Makefile

# Use gfortran, not other compilers which are not free/licensed
sed -i -e "s#COMPILER[ \t]*=[ \t]*ifort#COMPILER=gfortran#g" Makefile

# Remove strange options in fortran (fixed line length, multiCPU compilation)
sed -i -e "s#132#none#g" Makefile
sed -i -e "s#make -j FC#make FC#g" Makefile

# hardcode svn info
sed -i -e 's#^pwhg_main:#$$(shell ../svnversion/svnversion.sh>/dev/null) \
\
pwhg_main:#g' Makefile

echo "pwhg_main.o: svn.version" >> Makefile
echo "lhefwrite.o: svn.version" >> Makefile

# Find proper histo booking routine (many of them exist)
BOOK_HISTO="pwhg_bookhist-multi.o"
if [ `echo $${POWHEGSRC} | cut -d "_" -f 1` = "powhegboxV1" ]; then
   BOOK_HISTO="pwhg_bookhist.o"
fi 

$patch_3 

if [ -e ./Virtual/Virt_full_cHHH_-1.0.grid ]; then
  cp ./Virtual/events.cdf $${WORKDIR}/$${name}/
  cp ./Virtual/creategrid.py* $${WORKDIR}/$${name}/
  cp ./Virtual/Virt_full_cHHH*.grid $${WORKDIR}/$${name}/
fi

# Remove ANY kind of analysis with parton shower
if [ `grep particle_identif pwhg_analysis-dummy.f` = ""]; then
   cp ../pwhg_analysis-dummy.f .
fi
sed -i -e "s#PWHGANAL[ \t]*=[ \t]*#\#PWHGANAL=#g" Makefile
sed -i -e "s#ANALYSIS[ \t]*=[ \t]*#\#ANALYSIS=#g" Makefile
sed -i -e "s#_\#ANALYSIS*#_ANALYSIS=#g" Makefile
sed -i -e "s#LHAPDF_CONFIG[ \t]*=[ \t]*#\#LHAPDF_CONFIG=#g" Makefile
sed -i -e "s#pwhg_bookhist.o# #g" Makefile
sed -i -e "s#pwhg_bookhist-new.o# #g" Makefile
sed -i -e "s#pwhg_bookhist-multi.o# #g" Makefile

$patch_4 

echo "ANALYSIS=none " >> tmpfile

# Add libraries now
NEWRPATH1=`ls /cvmfs/cms.cern.ch/$${SCRAM_ARCH}/external/gcc/*/* | grep "/lib64" | head -n 1`
NEWRPATH1=$${NEWRPATH1%?}
NEWRPATH2=`ls /cvmfs/cms.cern.ch/$${SCRAM_ARCH}/external/zlib-x86_64/*/* | grep "/lib" | head -n 1`
NEWRPATH2=$${NEWRPATH2%?}
echo "RPATHLIBS= -Wl,-rpath,$${NEWRPATH1} -L$${NEWRPATH1} -lgfortran -lstdc++ -Wl,-rpath,$${NEWRPATH2} -L$${NEWRPATH2} -lz" >> tmpfile

$patch_5 
echo "LHAPDF_CONFIG=$${LHAPDF_BASE}/bin/lhapdf-config" >> tmpfile
mv Makefile Makefile.interm
cat tmpfile Makefile.interm > Makefile
rm -f Makefile.interm tmpfile

# Add extra packages
if [ $$jhugen = 1 ]; then
  if [ ! -f JHUGenerator.$${jhugenversion}.tar.gz ]; then
    wget --no-verbose --no-check-certificate http://spin.pha.jhu.edu/Generator/JHUGenerator.$${jhugenversion}.tar.gz || fail_exit "Failed to get JHUGen tar ball "
  fi

  tar zxf JHUGenerator.$${jhugenversion}.tar.gz
  cd JHUGenerator
  sed -i -e "s#Comp = ifort#Comp = gfort#g" makefile
  sed -i -e "s#linkMELA = Yes#linkMELA = No#g" makefile
  make

  mkdir -p $${WORKDIR}/$${name}
  cp -p JHUGen $${WORKDIR}/$${name}/.
  cp -pr pdfs $${WORKDIR}/$${name}/.


  cd ..
fi

$patch_6 


echo 'Compiling pwhg_main...'
pwd

$patch_7 

export PYTHONPATH=./Virtual/:$$PYTHONPATH
make pwhg_main || fail_exit "Failed to compile pwhg_main"

mkdir -p $${WORKDIR}/$${name}
cp -p pwhg_main $${WORKDIR}/$${name}/.

if [ -d ./lib ]; then
  cp -a ./lib $${WORKDIR}/$${name}/.
fi 
if [ -d ./lib64 ]; then
  cp -a ./lib64 $${WORKDIR}/$${name}/.
fi
if [ -d ./obj-gfortran/proclib ]; then
  mkdir $${WORKDIR}/$${name}/obj-gfortran/
  cp -a ./obj-gfortran/proclib $${WORKDIR}/$${name}/obj-gfortran/.
  cp -a ./obj-gfortran/*.so $${WORKDIR}/$${name}/obj-gfortran/.
fi
if [ -d ./QCDLoop-1.9 ]; then                                 
  cp -a ./QCDLoop-1.9 $${WORKDIR}/$${name}/.                    
  cp -a ./QCDLoop-1.9/ff/ff*.dat $${WORKDIR}/$${name}/.      
fi

cd $${WORKDIR}/$${name}

$patch_8 

#mkdir -p workdir
#cd workdir
localDir=`pwd`

# Copy additional files
if [ -e $${WORKDIR}/vbfnlo.input ]; then
  cp -p $${WORKDIR}/vbfnlo.input .
fi 
if [ -e $${WORKDIR}/br.a3_2HDM ]; then
  cp -p $${WORKDIR}/br*2HDM .
fi
if [ -e $${WORKDIR}/powheg-fh.in ]; then
  cp -p $${WORKDIR}/powheg-fh.in .
fi
if [ -e $${WORKDIR}/cteq6m ]; then
    cp -p $${WORKDIR}/cteq6m .
fi 

#rm -f $${WORKDIR}/$${name}/powheg.input
#cat $${card} | sed -e "s#SEED#$${seed}#g" | sed -e "s#NEVENTS#$${nevt}#g" > powheg.input

#cat powheg.input
#if [[ -e ../pwhg_main-gnu ]]; then
#  mv ../pwhg_main-gnu ../pwhg_main
#  chmod a+x ../pwhg_main
#fi

#make sure env variable for pdfsets points to the right place
#export LHAPDF_DATA_PATH=`$${myDir}/lhapdf-config --datadir`
#../pwhg_main &> log_$${process}_$${seed}.txt

#cp -p ../runcms*.sh .

cd $${WORKDIR}

echo 'Compiling finished...'

if [ $$jhugen = 1 ]; then
  cp -p $${cardj} .
  
  if [ ! -e $${WORKDIR}/runcmsgrid_powhegjhugen.sh ]; then
   fail_exit "Did not find " $${WORKDIR}/runcmsgrid_powhegjhugen.sh 
  fi
  sed -e 's/PROCESS/'$${process}'/g' $${WORKDIR}/runcmsgrid_powhegjhugen.sh > runcmsgrid.sh
else
  if [ ! -e $${WORKDIR}/runcmsgrid_powheg.sh ]; then
   fail_exit "Did not find " $${WORKDIR}/runcmsgrid_powheg.sh 
  fi
  sed -e 's/PROCESS/'$${process}'/g' $${WORKDIR}/runcmsgrid_powheg.sh > runcmsgrid.sh
fi

sed -i s/SCRAM_ARCH_VERSION_REPLACE/$${SCRAM_ARCH}/g runcmsgrid.sh
sed -i s/CMSSW_VERSION_REPLACE/$${CMSSW_VERSION}/g runcmsgrid.sh

chmod 755 runcmsgrid.sh

