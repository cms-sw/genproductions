export name=$folderName
export cardInput=$powInputName
export process=$process
export noPdfCheck=$noPdfCheck
export WORKDIR=$rootfolder
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

if [ "$$process" = "WZ" ] || [ "$$process" = "ZZ" ]; then
   patch -l -p0 -i ${patches_dir}/lhapdf_zanderighi.patch
fi

if [ "$$process" = "b_bbar_4l" ]; then
    cd POWHEG-BOX
    patch -l -p0 -i ${patches_dir}/res_openloops_long_install_dir.patch
    cd ..
fi
if [ "$$process" = "ttb_NLO_dec" ]; then
    patch -l -p0 -i ${patches_dir}/pwhg_ttb_NLO_dec_gen_radiation_hook.patch
fi
if [ "$$process" = "ZZ" ]; then
    patch -l -p0 -i ${patches_dir}/zz_m4lcut.patch
fi
if [ "$$process" = "WWJ" ]; then
    patch -l -p0 -i ${patches_dir}/wwj-weights.patch
    cp ${patches_dir}/rwl_write_weights2_extra.f POWHEG-BOX/$$process/
fi
if [ "$$process" = "Zj" ] || [ "$$process" = "Wj" ]; then
    patch -l -p0 -i ${patches_dir}/pwhg_write_weights_nnlo.patch
fi


sed -i -e "s#500#1200#g"  POWHEG-BOX/include/pwhg_rwl.h

echo $${POWHEGSRC} > VERSION

cd POWHEG-BOX/$${process}

# This is just to please gcc 4.8.1
mkdir -p include

if [ "$$process" = "Zj" ] || [ "$$process" = "Wj" ]; then
    tar zxf ../DYNNLOPS.tgz
    wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/powheg/V2.0/src/nnlops_fast_patch3_$${process:0:1}.tgz
    tar zxf nnlops_fast_patch3_$${process:0:1}.tgz
    mv Makefile-NNLOPS Makefile
fi


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

if [ "$$process" = "gg_H" ] || [ "$$process" = "ggHH" ] || [ "$$process" = "ggHH_EWChL" ] || [ "$$process" = "WWJ" ]; then
   BOOK_HISTO=""
   echo "Process using pwhg_bookhist-multi-new"
fi

if [ "$$process" = "ggHH" ] || [ "$$process" = "ggHH_EWChL" ]; then
   sed -i -e "/PYTHIA8LOCATION/s|^|#|g" Makefile
   sed -i -e "/LIBPYTHIA8/s|^|#|g" Makefile
   sed -i -e "s|LIBHEPMC=|# LIBHEPMC=|g" Makefile
   sed -i -e "/main-PYTHIA8-lhef:/s|^|#|g" Makefile
   sed -i -e "s|LIBS+=-L:\\$$|LIBS+=-L\\$$|g" Makefile
fi

if [ "$$process" = "trijet" ]; then 
   BOOK_HISTO+=" observables.o"
   rm -rf ../progress/bbinit.f
fi  

if [ "$$process" = "VBF_HJJJ" ]; then 
  sed -i 's/..\/pwhg_book.h/pwhg_book.h/g' pwhg_analysis-dummy.f
fi  
if [ "$$process" = "VBF_H" ]; then 
  sed -i '/pwhginihist/d' pwhg_analysis-dummy.f 
  patch -l -p0 -i ${patches_dir}/vbf_h_init_couplings.patch
fi  
if [ "$$process" = "VBF_Z_Z" ]; then 
  patch -l -p0 -i ${patches_dir}/vbf_z_z_init_couplings.patch
fi  
if [ "$$process" = "Wgamma" ] || [ "$$process" = "W_ew-BMNNP" ]; then
    patch -l -p0 -i ${patches_dir}/pwhg_analysis_driver.patch 
fi
if [ "$$process" = "HW_ew" ]; then
    patch -l -p0 -i ${patches_dir}/hwew.patch 
fi
#if [ "$$process" = "ttb_NLO_dec" ]; then
#    patch -l -p0 -i ${patches_dir}/pwhg_analysis_driver_offshellmap.patch
#fi
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
sed -i -e "s#LHAPDF_CONFIG[ \t]*=[ \t]*#\#LHAPDF_CONFIG=#g" Makefile
sed -i -e "s#pwhg_bookhist.o# #g" Makefile
sed -i -e "s#pwhg_bookhist-new.o# #g" Makefile
sed -i -e "s#pwhg_bookhist-multi.o# #g" Makefile

# Use option O0 for bbH (O2 too long)
if [ "$$process" = "bbH" ]; then
   sed -i -e "s#O2#O0#g" Makefile
fi

# fix fortran options/linking to OpenLoops/missing libraries in VH_ew
if [ "$$process" = "HW_ew" ] || [ "$$process" = "HZ_ew" ] || [ "$$process" = "HZJ_ew" ] || [ "$$process" = "HWJ_ew" ] ; then
   sed -i -e "s#OL_process_src#OL_process_src f90_flags=-ffree-line-length-none#g" Makefile
   sed -i -e "s#\$$(PWD)/\$$(OBJ)#\$$(OBJ)#g" Makefile
   sed -i -e "s#\$$(OLPATH)/lib_src#lib_src#g" Makefile
   sed -i -e "s#cd \$$(OLPATH)#cp -r \$$(OLPATH)/* .#g" Makefile
   sed -i -e "s#abspath(os.path.join(config#relpath(os.path.join(config#g" ../OpenLoopsStuff/OpenLoops/SConstruct
   sed -i -e "s#rpath=\$$(PWD)/\$$(OBJDIR) -L\$$(PWD)/\$$(OBJDIR)#rpath=\$$(OBJDIR) -L\$$(OBJDIR)#g" Makefile
fi
if [ "$$process" = "HWJ_ew" ] || [ "$$process" = "HZJ_ew" ] ; then
   sed -i -e "s#boostrot.o#boostrot.o boostrot4.o#g" Makefile
fi

# 
if [ "$$process" = "ttJ" ]; then
  sed -i -e "s#_PATH) -L#_PATH) #g" Makefile
  sed -i -e "s# -lvirtual#/libvirtual.so.1.0.0#g" Makefile
fi

# Use option O0 for ttH (O2 too long)
if [ "$$process" = "ttH" ]; then
  sed -i 's/O2/O0/g' Makefile
  sed -i 's/4.5d0/4.75d0/g' init_couplings.f
fi

if [ "$$process" = "gg_H_MSSM" ]; then 
  sed -i 's/leq/le/g' nloreal.F
  cp -p ../gg_H_quark-mass-effects/SLHA.h .
  cp -p ../gg_H_quark-mass-effects/SLHADefs.h .
fi
  
echo "ANALYSIS=none " >> tmpfile

# Add libraries now
NEWRPATH1=`ls /cvmfs/cms.cern.ch/$${SCRAM_ARCH}/external/gcc/*/* | grep "/lib64" | head -n 1`
NEWRPATH1=$${NEWRPATH1%?}
NEWRPATH2=`ls /cvmfs/cms.cern.ch/$${SCRAM_ARCH}/external/zlib-x86_64/*/* | grep "/lib" | head -n 1`
NEWRPATH2=$${NEWRPATH2%?}
echo "RPATHLIBS= -Wl,-rpath,$${NEWRPATH1} -L$${NEWRPATH1} -lgfortran -lstdc++ -Wl,-rpath,$${NEWRPATH2} -L$${NEWRPATH2} -lz" >> tmpfile

if [ "$$process" = "Wgamma" ]; then
  echo "PWHGANAL=$$BOOK_HISTO pwhg_analysis-dummy.o uti.o " >> tmpfile
else
  echo "PWHGANAL=$$BOOK_HISTO pwhg_analysis-dummy.o " >> tmpfile
fi
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

if [ "$$process" = "WWJ" ]; then
  cp Makefile Makefile.orig
  cat Makefile.orig | sed -e "s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$$(scram tool info fastjet | grep BASE | cut -d "=" -f2)/bin/fastjet-config#g" | sed -e "s#cs_angles.o#cs_angles.o fastjetfortran.o observables.o pwhg_bookhist-multi-new.o#g" | sed -e "s#\#\ FASTJET_CONFIG#FASTJET_CONFIG#g" | sed -e "s#\#\ LIBSFASTJET#LIBSFASTJET#g" | sed -e "s#\#\ FJCXXFLAGS#FJCXXFLAGS#g" | sed -e "s#rwl_write_weights_extra.f#rwl_write_weights_extra.f\ rwl_write_weights2_extra.f#g" > Makefile
fi

if [ "$$process" = "gg_H_2HDM" ] || [ "$$process" = "gg_H_MSSM" ]; then
  echo "Adding CHAPLIN 1.2 library"
  if [ ! -f chaplin-1.2.tar ]; then
    wget --no-verbose http://chaplin.hepforge.org/code/chaplin-1.2.tar || fail_exit "Failed to get CHAPLIN tar ball "
  fi
  tar xvf chaplin-1.2.tar
  cd chaplin-1.2
  ./configure --prefix=`pwd`/..
  make install
  cd .. 
  echo "LIBS+=-L`pwd`/lib/ -L`pwd`/lib64/" >> Makefile   # be safe
  export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:$${LD_LIBRARY_PATH}

  if [ "$$process" = "gg_H_MSSM" ]; then
    echo "Adding FeynHiggs 2.10 library"
    if [ ! -f FeynHiggs-2.10.2.tar.gz ]; then
      wget --no-verbose http://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/newversion/FeynHiggs-2.10.2.tar.gz || fail_exit "Failed to get FeynHiggs tar ball "
    fi
    tar xvf FeynHiggs-2.10.2.tar.gz
    cd FeynHiggs-2.10.2
    ./configure --prefix=`pwd`/..
    make 
    make install
    cd ..
  fi
fi

if [ "$$process" = "directphoton" ]; then
  echo "Adding LoopTools 2.14 library"
  if [ ! -f LoopTools-2.14.tar.gz ]; then
    wget --no-verbose http://www.feynarts.de/looptools/LoopTools-2.14.tar.gz || fail_exit "Failed to get LoopTools tar ball "
  fi
  tar xvf LoopTools-2.14.tar.gz
  cd LoopTools-2.14
  ./configure --prefix=`pwd`/..
  make install
  cd ..
  sed -i -e 's/^LT\=$$.*/LT=$$\(PWD\)/' Makefile
  export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:$${LD_LIBRARY_PATH}
  mkdir obj-gfortran
fi

if [ "$$process" = "vbs-ssww-nloew" ]; then
  echo "Adding Recola2.2.0 library"
  if [ ! -f recola2-collier-2.2.0.tar.gz ]; then
    wget --no-verbose -O recola2-collier-2.2.0.tar.gz https://recola.hepforge.org/downloads/?f=recola2-collier-2.2.0.tar.gz || fail_exit "Failed to get Recola tar ball "
  fi
  tar -zxvf recola2-collier-2.2.0.tar.gz
  cd recola2-collier-2.2.0/build
  cmake .. -DCMAKE_Fortran_COMPILER=gfortran -Dmodel=SM
  make -j 1
  make install
  cd ../..
  mkdir obj-gfortran/proclib
  cd obj-gfortran/proclib
  cp ../../recola2-collier-2.2.0/recola2-2.2.0/librecola.so .
  cd ../..
  cp Makefile Makefile.orig
  cat Makefile.orig | sed -e "s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$$(scram tool info fastjet | grep BASE | cut -d "=" -f2)/bin/fastjet-config#g" | sed -e "s#/archive/mpellen/Programs/Recolas/Powheg/recola-1.3.6#$$\(PWD\)/recola2-collier-2.2.0/recola2-2.2.0#g" | sed -e "s# real16.o##g" | sed -e '154d;164d' > Makefile
  export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:$${LD_LIBRARY_PATH}
fi


echo 'Compiling pwhg_main...'
pwd
if [ "$$process" = "HJ" ]; then
  echo "fixing q2min determination for HJ"
  # avoid accessing member 1 for q2min determination. Use member 0, as member 1 may not be available
  sed -i "s/getq2min(1,tmp)/getq2min(0,tmp)/g" setlocalscales.f
fi  

if [ "$$process" = "ST_wtch_DR" ] || [ "$$process" = "ST_wtch_DS" ]; then   
  echo "D/L QCDLoop-1.9 library"                            
  if [ ! -f FeynHiggs-2.10.2.tar.gz ]; then                 
    wget --no-verbose http://qcdloop.fnal.gov/QCDLoop-1.96.tar.gz || fail_exit "Failed to get QCDLoop tar ball"
  fi                                                        
  tar xvf QCDLoop-1.96.tar.gz                               
  mv QCDLoop-1.96 QCDLoop-1.9
  sed -i -e 's#/Users/ellis/QCDLoop#./QCDLoop#' ff/ffinit_mine.f                
  cd QCDLoop-1.9                                            
  make                                                      
  cd ..                                                     
fi                                                          

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

if [ "$$process" = "HJ" ]; then
  echo "Compilinig HNNLO...."
  wget --no-verbose http://theory.fi.infn.it/grazzini/codes/hnnlo-v2.0.tgz
  tar -xzvf hnnlo-v2.0.tgz
  cd hnnlo-v2.0
  cp ../POWHEG-BOX/HJ/NNLOPS-mass-effects/HNNLO-makefile ./makefile 
  cp -r ../POWHEG-BOX/HJ/NNLOPS-mass-effects/HNNLO-patches ./
  cd src/Need/
  cat pdfset_lhapdf.f | sed -e "s#30#40#g" | sed -e "s#20#30#g" | sed -e "s#oldPDFname(1:i-1)//'.LHgrid'#oldPDFname(1:i-1)#g" | sed -e "s#oldPDFname(1:i-1)//'.LHpdf'#oldPDFname(1:i-1)#g" | sed -e "s#InitPDFset('PDFsets/'//PDFname)#InitPDFsetByName(PDFname)#g" > pdfset_lhapdf.f.new
  mv pdfset_lhapdf.f.new pdfset_lhapdf.f  
  cd -
  cat makefile | sed -e "s#LHAPDFLIB=.\+#LHAPDFLIB=$$(scram tool info lhapdf | grep LIBDIR | cut -d "=" -f2)#g" > makefile
  make || fail_exit "Failed to compile HNNLO"
  cp -p bin/hnnlo $${WORKDIR}/$${name}/
  cp bin/br.* $${WORKDIR}/$${name}/
  cd $${WORKDIR}/$${name}
  cp POWHEG-BOX/HJ/NNLOPS-mass-effects/mergedata.f .
  gfortran -o mergedata mergedata.f

  cd $${WORKDIR}/$${name}/POWHEG-BOX/HJ
 
  cp Makefile Makefile.orig
  cat Makefile.orig | sed -e "s#ANALYSIS=.\+#ANALYSIS=NNLOPS#g" |sed -e "s#\$$(shell \$$(LHAPDF_CONFIG) --libdir)#$$(scram tool info lhapdf | grep LIBDIR | cut -d "=" -f2)#g" | sed -e "s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$$(scram tool info fastjet | grep BASE | cut -d "=" -f2)/bin/fastjet-config#g" | sed -e "s#NNLOPSREWEIGHTER+=  fastjetfortran.o#NNLOPSREWEIGHTER+=  fastjetfortran.o pwhg_bookhist-multi.o#g" | sed -e "s#NNLOPSREWEIGHTERNRW+=  fastjetfortran.o#NNLOPSREWEIGHTERNRW+=  fastjetfortran.o pwhg_bookhist-multi.o#g" > Makefile
  make nnlopsreweighter-newrwgt || fail_exit "Failed to compile nnlopsreweighter"
  cp nnlopsreweighter-newrwgt ../../
  cd $${WORKDIR}/$${name}
  HMASS=`cat powheg.input | grep "^hmass" | cut -d " " -f2`;
  BEAM=`cat powheg.input | grep "^ebeam1" | cut -d " " -f2 | tr "d" "."`;
  COMENERGY=`echo "( $$BEAM*2 )" | bc`
  gawk "/sroot/{gsub(/8000/,$$COMENERGY)};/hmass/{gsub(/125.5/, $${HMASS})};/mur,muf/{gsub(/62.750/, $$(( $$HMASS/2 )))};{print}" POWHEG-BOX/HJ/PaperRun/HNNLO-LHC8-R04-APX2-11.input | sed -e "s#10103#SEED#g" | sed -e "s#HNNLO-LHC8-R04-APX2-11#HNNLO-LHC13-R04-APX2-11#g"> HNNLO-LHC13-R04-APX2-11.input
  gawk "/sroot/{gsub(/8000/,$$COMENERGY)};/hmass/{gsub(/125.5/, $${HMASS})};/mur,muf/{gsub(/62.750/, $$(( $$HMASS )))};{print}" POWHEG-BOX/HJ/PaperRun/HNNLO-LHC8-R04-APX2-11.input | sed -e "s#10103#SEED#g" | sed -e "s#HNNLO-LHC8-R04-APX2-11#HNNLO-LHC13-R04-APX2-22#g"> HNNLO-LHC13-R04-APX2-22.input
  gawk "/sroot/{gsub(/8000/,$$COMENERGY)};/hmass/{gsub(/125.5/, $${HMASS})};/mur,muf/{gsub(/62.750/, $$(( $$HMASS/4 )))};{print}" POWHEG-BOX/HJ/PaperRun/HNNLO-LHC8-R04-APX2-11.input | sed -e "s#10103#SEED#g" | sed -e "s#HNNLO-LHC8-R04-APX2-11#HNNLO-LHC13-R04-APX2-0505#g"> HNNLO-LHC13-R04-APX2-0505.input
  cat << EOF > nnlopsreweighter.input
# a line beginning with 'lhfile' followed by the name of the event file

lhfile pwgevents.lhe 
rwl_format_rwgt

# weights present in the lhfile: 'mtinf', 'mt', 'mtmb', 'mtmb-bminlo'


# a line with: 'nnlofiles'
# followed by a quoted label and the name of a HNNLO output file.
# In the following the 3 ouput refer to mt=infinity approx,
# finite mt, and finite mt and mb.

nnlofiles
'nn-mtmb-11' HNNLO-11.top
'nn-mtmb-22' HNNLO-22.top
'nn-mtmb-0505' HNNLO-0505.top

# The new desired weights, in the Les Houches format.
# The user can choose to group them in the way he prefers, and give them
# the id's he likes.
# The program determined how to compute each weights from the description
# line. It loops through the weights id's present in the pwgevents.lhe file
# and through the labels of the nnlofiles. If a label of a weight and
# a label of the nnlofiles are both present in the description field
# of a weight mentioned here, it computes that weight by reweighting
# the corresponding weights in the lhe file with the nnlo result present
# in the nnlofiles associated with the label. For example, in the
# nnlops-mt id in the following it reweights the nn-mtinf weight present
# in the .lhe file with the nnlo result present in the
# HNNLO-LHC8-R04-APX0-11.top file.

<initrwgt>
<weightgroup name='nnl'> 
<weight id='nnlops-11-1'> combines 'nn-mtmb-11' with '1001' </weight> 
<weight id='nnlops-11-2'> combines 'nn-mtmb-11' with '1002' </weight> 
<weight id='nnlops-11-3'> combines 'nn-mtmb-11' with '1003' </weight> 
<weight id='nnlops-11-4'> combines 'nn-mtmb-11' with '1004' </weight> 
<weight id='nnlops-11-5'> combines 'nn-mtmb-11' with '1005' </weight> 
<weight id='nnlops-11-6'> combines 'nn-mtmb-11' with '1006' </weight> 
<weight id='nnlops-11-7'> combines 'nn-mtmb-11' with '1007' </weight> 
<weight id='nnlops-11-8'> combines 'nn-mtmb-11' with '1008' </weight> 
<weight id='nnlops-11-9'> combines 'nn-mtmb-11' with '1009' </weight> 
<weight id='nnlops-22-1'> combines 'nn-mtmb-22' with '1001' </weight>          
<weight id='nnlops-22-2'> combines 'nn-mtmb-22' with '1002' </weight>         
<weight id='nnlops-22-3'> combines 'nn-mtmb-22' with '1003' </weight>         
<weight id='nnlops-22-4'> combines 'nn-mtmb-22' with '1004' </weight>         
<weight id='nnlops-22-5'> combines 'nn-mtmb-22' with '1005' </weight>         
<weight id='nnlops-22-6'> combines 'nn-mtmb-22' with '1006' </weight>         
<weight id='nnlops-22-7'> combines 'nn-mtmb-22' with '1007' </weight>         
<weight id='nnlops-22-8'> combines 'nn-mtmb-22' with '1008' </weight>         
<weight id='nnlops-22-9'> combines 'nn-mtmb-22' with '1009' </weight>
<weight id='nnlops-0505-1'> combines 'nn-mtmb-0505' with '1001' </weight>
<weight id='nnlops-0505-2'> combines 'nn-mtmb-0505' with '1002' </weight>
<weight id='nnlops-0505-3'> combines 'nn-mtmb-0505' with '1003' </weight>
<weight id='nnlops-0505-4'> combines 'nn-mtmb-0505' with '1004' </weight>
<weight id='nnlops-0505-5'> combines 'nn-mtmb-0505' with '1005' </weight>
<weight id='nnlops-0505-6'> combines 'nn-mtmb-0505' with '1006' </weight>
<weight id='nnlops-0505-7'> combines 'nn-mtmb-0505' with '1007' </weight>
<weight id='nnlops-0505-8'> combines 'nn-mtmb-0505' with '1008' </weight>
<weight id='nnlops-0505-9'> combines 'nn-mtmb-0505' with '1009' </weight>
</weightgroup>
</initrwgt>
EOF

fi  

if [ "$$process" = "Zj" ] || [ "$$process" = "Wj" ]; then
  echo "Compiling DYNNLO...."
  wget --no-verbose --no-check-certificate http://theory.fi.infn.it/grazzini/codes/dynnlo-v1.5.tgz
  tar -xzvf dynnlo-v1.5.tgz
  cd dynnlo-v1.5
  cp ../POWHEG-BOX/$${process}/DYNNLOPS/$${process:0:1}NNLOPS/dynnlo-patches/dynnlo.makefile ./makefile
  cp -r -L ../POWHEG-BOX/$${process}/DYNNLOPS/$${process:0:1}NNLOPS/dynnlo-patches ./
  cd src/Need/
  cat pdfset_lhapdf.f | sed -e "s#30#40#g" | sed -e "s#20#30#g" | sed -e "s#oldPDFname(1:i-1)//'.LHgrid'#oldPDFname(1:i-1)#g" | sed -e "s#oldPDFname(1:i-1)//'.LHpdf'#oldPDFname(1:i-1)#g" | sed -e "s#InitPDFset('PDFsets/'//PDFname)#InitPDFsetByName(PDFname)#g" > pdfset_lhapdf.f.new
  mv pdfset_lhapdf.f.new pdfset_lhapdf.f
  cd -
  cat makefile | sed -e "s#LHAPDFLIB=.\+#LHAPDFLIB=$$(scram tool info lhapdf | grep LIBDIR | cut -d "=" -f2)#g" > makefile
  make || fail_exit "Failed to compile DYNNLO"

  cp -p bin/dynnlo $${WORKDIR}/$${name}/

  cd $${WORKDIR}/$${name}/POWHEG-BOX/$${process}/DYNNLOPS/aux
  gfortran -mcmodel=medium -o merge3ddata merge3ddata.f  || fail_exit "Failed to compile merge3ddata"
  cp merge3ddata $${WORKDIR}/$${name}/
  gfortran -mcmodel=medium -o mergedata mergedata.f  || fail_exit "Failed to compile mergedata"
  cp mergedata $${WORKDIR}/$${name}/

  cd $${WORKDIR}/$${name}/POWHEG-BOX/$${process}
  make lhef_analysis_3d || fail_exit "Failed to compile lhef_analysis_3d"
  cp lhef_analysis_3d $${WORKDIR}/$${name}/

  cd $${WORKDIR}/$${name}
  VMASS=`cat powheg.input | grep "^Wmass\|^Zmass" | awk '{print $$2}' | cut -d "d" -f1`
  VMASSEXP=`cat powheg.input | grep "^Wmass\|^Zmass" | awk '{print $$2}' | cut -d "d" -f2`
  VMASS=`echo "( $$VMASS*10^$$VMASSEXP )" | bc`
  VMASSMIN=`cat powheg.input | grep "^min_W_mass\|^min_Z_mass" | awk '{print $$2}'`
  VMASSMAX=`cat powheg.input | grep "^max_W_mass\|^max_Z_mass" | awk '{print $$2}'`
  echo $$VMASS
  DYNNLOPROC=3
  if [ "$$process" = "Wj" ]; then
    DYNNLOPROC=1
    VID=`cat powheg.input | grep "^idvecbos" | awk '{print $$2}'`;
    if [ "$$VID" = "-24" ]; then
      DYNNLOPROC=2
    fi
  fi
  BEAM=`cat powheg.input | grep "^ebeam1" | cut -d " " -f2 | tr "d" "."`;
  COMENERGY=`echo "( $$BEAM*2 )" | bc`

  cp POWHEG-BOX/$${process}/DYNNLOPS/$${process:0:1}NNLOPS/dynnlo-patches/dynnlo.infile dynnlo.infile.orig
  gawk "/sroot/{gsub(/.*!/,$$COMENERGY \" !\")};\
        /nproc/{gsub(/.*!/,$$DYNNLOPROC \" !\")};\
        /mur/{gsub(/.*!/, $$VMASS \" \" $$VMASS \" !\")};\
        /mwmin/{gsub(/.*!/, $$VMASSMIN \" \" $$VMASSMAX \" !\")};\
        /rseed/{gsub(/.*!/,\"SEED !\")};\
        /runstring/{gsub(/.*!/,\"'SEED' !\")};\
        {print}" dynnlo.infile.orig | tee DYNNLO.input
fi

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

