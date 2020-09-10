import string 

def fillTemplatedFile(template_file_name, out_file_name, template_dict, openmode = "a"):
    with open(template_file_name, "r") as templateFile:
        source = string.Template(templateFile.read())
        result = source.substitute(template_dict)
    with open(out_file_name, openmode) as outFile:
        outFile.write(result)

def runGetSource_patch_1(process) :
  return {
    "WZ" : "patch -l -p0 -i ${patches_dir}/lhapdf_zanderighi.patch",
    "ZZ" : "patch -l -p0 -i ${patches_dir}/lhapdf_zanderighi.patch\n \
patch -l -p0 -i ${patches_dir}/zz_m4lcut.patch",
    "b_bbar_4l" : "cd POWHEG-BOX\n \
patch -l -p0 -i ${patches_dir}/res_openloops_long_install_dir.patch\n \
cd ..",
    "ttb_NLO_dec" : "patch -l -p0 -i ${patches_dir}/pwhg_ttb_NLO_dec_gen_radiation_hook.patch",
    "WWJ" : "patch -l -p0 -i ${patches_dir}/wwj-weights.patch\n \
cp ${patches_dir}/rwl_write_weights2_extra.f POWHEG-BOX/$process/",
    "Zj" : "patch -l -p0 -i ${patches_dir}/pwhg_write_weights_nnlo.patch",
    "Wj" : "patch -l -p0 -i ${patches_dir}/pwhg_write_weights_nnlo.patch",
    "VBF_H_smeft" : "cd POWHEG-BOX/VBF_H_smeft\n \
head -n 966 pwhg_analysis.f | tail -n 12 > pwhg_analysis_new.f\n \
mv pwhg_analysis_new.f pwhg_analysis.f\n \
cd ../..", 
    }.get(process,"")

def runGetSource_patch_2(process) :
  return {
    "Zj" : "tar zxf ../DYNNLOPS.tgz\n \
wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/powheg/V2.0/src/nnlops_fast_patch3_${process:0:1}.tgz\n \
tar zxf nnlops_fast_patch3_${process:0:1}.tgz\n \
mv Makefile-NNLOPS Makefile",
    "Wj" : "tar zxf ../DYNNLOPS.tgz\n \
wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/powheg/V2.0/src/nnlops_fast_patch3_${process:0:1}.tgz\n \
tar zxf nnlops_fast_patch3_${process:0:1}.tgz\n \
mv Makefile-NNLOPS Makefile",
    }.get(process,"")

def runGetSource_patch_3(process) :
  return {
    "gg_H" : "BOOK_HISTO=\"\"\n \
echo \"Process using pwhg_bookhist-multi-new\"",
    "ggHH" : "BOOK_HISTO=\"\"\n \
echo \"Process using pwhg_bookhist-multi-new\"\n\n \
sed -i -e \"/PYTHIA8LOCATION/s|^|#|g\" Makefile\n \
sed -i -e \"/LIBPYTHIA8/s|^|#|g\" Makefile\n \
sed -i -e \"s|LIBHEPMC=|# LIBHEPMC=|g\" Makefile\n \
sed -i -e \"/main-PYTHIA8-lhef:/s|^|#|g\" Makefile\n \
sed -i -e \"s|LIBS+=-L:\\$|LIBS+=-L\\$|g\" Makefile",
    "ggHH_EWChL" : "BOOK_HISTO=\"\"\n \
echo \"Process using pwhg_bookhist-multi-new\"\n\n \
sed -i -e \"/PYTHIA8LOCATION/s|^|#|g\" Makefile\n \
sed -i -e \"/LIBPYTHIA8/s|^|#|g\" Makefile\n \
sed -i -e \"s|LIBHEPMC=|# LIBHEPMC=|g\" Makefile\n \
sed -i -e \"/main-PYTHIA8-lhef:/s|^|#|g\" Makefile\n \
sed -i -e \"s|LIBS+=-L:\\$|LIBS+=-L\\$|g\" Makefile",
    "WWJ" : "BOOK_HISTO=\"\"\n \
echo \"Process using pwhg_bookhist-multi-new\"",
    "trijet" : "   BOOK_HISTO+=\" observables.o\"\n \
rm -rf ../progress/bbinit.f",
    "VBF_HJJJ" : "sed -i 's/..\/pwhg_book.h/pwhg_book.h/g' pwhg_analysis-dummy.f",
    "VBF_H" : "sed -i '/pwhginihist/d' pwhg_analysis-dummy.f\n \
patch -l -p0 -i ${patches_dir}/vbf_h_init_couplings.patch",
    "VBF_H_smeft" : "sed -i -e \"s#reshufflemoms.o#reshufflemoms.o pwhg_analysis.o#g\" Makefile",
    "VBF_Z_Z" : "patch -l -p0 -i ${patches_dir}/vbf_z_z_init_couplings.patch",
    "Wbb_nodec" : "sed -i -e \"s#reshufflemoms.o#reshufflemoms.o lhefread.o rwl_weightlists.o rwl_setup_param_weights.o pwhg_io_interface.o#g\" Makefile\n \
    rm -f pwhg_analysis_driver.f",
    "Wgamma" : "patch -l -p0 -i ${patches_dir}/pwhg_analysis_driver.patch",
    "W_ew-BMNNP" : "patch -l -p0 -i ${patches_dir}/pwhg_analysis_driver.patch",
    "HW_ew" : "patch -l -p0 -i ${patches_dir}/hwew.patch",
    "ttb_NLO_dec" : "#patch -l -p0 -i ${patches_dir}/pwhg_analysis_driver_offshellmap.patch",
    }.get(process,"")

def runGetSource_patch_4(process) :
  return {
    "bbH" : "# Use option O0 for bbH (O2 too long)\n \
sed -i -e \"s#O2#O0#g\" Makefile",
    "HW_ew" : "# fix fortran options/linking to OpenLoops/missing libraries in VH_ew\n \
sed -i -e \"s#OL_process_src#OL_process_src f90_flags=-ffree-line-length-none#g\" Makefile\n \
sed -i -e \"s#\$(PWD)/\$(OBJ)#\$(OBJ)#g\" Makefile\n \
sed -i -e \"s#\$(OLPATH)/lib_src#lib_src#g\" Makefile\n \
sed -i -e \"s#cd \$(OLPATH)#cp -r \$(OLPATH)/* .#g\" Makefile\n \
sed -i -e \"s#abspath(os.path.join(config#relpath(os.path.join(config#g\" ../OpenLoopsStuff/OpenLoops/SConstruct\n \
sed -i -e \"s#rpath=\$(PWD)/\$(OBJDIR) -L\$(PWD)/\$(OBJDIR)#rpath=\$(OBJDIR) -L\$(OBJDIR)#g\" Makefile",
    "HZ_ew" : "# fix fortran options/linking to OpenLoops/missing libraries in VH_ew\n \
sed -i -e \"s#OL_process_src#OL_process_src f90_flags=-ffree-line-length-none#g\" Makefile\n \
sed -i -e \"s#\$(PWD)/\$(OBJ)#\$(OBJ)#g\" Makefile\n \
sed -i -e \"s#\$(OLPATH)/lib_src#lib_src#g\" Makefile\n \
sed -i -e \"s#cd \$(OLPATH)#cp -r \$(OLPATH)/* .#g\" Makefile\n \
sed -i -e \"s#abspath(os.path.join(config#relpath(os.path.join(config#g\" ../OpenLoopsStuff/OpenLoops/SConstruct\n \
sed -i -e \"s#rpath=\$(PWD)/\$(OBJDIR) -L\$(PWD)/\$(OBJDIR)#rpath=\$(OBJDIR) -L\$(OBJDIR)#g\" Makefile",
    "HZJ_ew" : "# fix fortran options/linking to OpenLoops/missing libraries in VH_ew\n \
sed -i -e \"s#OL_process_src#OL_process_src f90_flags=-ffree-line-length-none#g\" Makefile\n \
sed -i -e \"s#\$(PWD)/\$(OBJ)#\$(OBJ)#g\" Makefile\n \
sed -i -e \"s#\$(OLPATH)/lib_src#lib_src#g\" Makefile\n \
sed -i -e \"s#cd \$(OLPATH)#cp -r \$(OLPATH)/* .#g\" Makefile\n \
sed -i -e \"s#abspath(os.path.join(config#relpath(os.path.join(config#g\" ../OpenLoopsStuff/OpenLoops/SConstruct\n \
sed -i -e \"s#rpath=\$(PWD)/\$(OBJDIR) -L\$(PWD)/\$(OBJDIR)#rpath=\$(OBJDIR) -L\$(OBJDIR)#g\" Makefile\n \
sed -i -e \"s#boostrot.o#boostrot.o boostrot4.o#g\" Makefile",
    "HWJ_ew" : "# fix fortran options/linking to OpenLoops/missing libraries in VH_ew\n \
sed -i -e \"s#OL_process_src#OL_process_src f90_flags=-ffree-line-length-none#g\" Makefile\n \
sed -i -e \"s#\$(PWD)/\$(OBJ)#\$(OBJ)#g\" Makefile\n \
sed -i -e \"s#\$(OLPATH)/lib_src#lib_src#g\" Makefile\n \
sed -i -e \"s#cd \$(OLPATH)#cp -r \$(OLPATH)/* .#g\" Makefile\n \
sed -i -e \"s#abspath(os.path.join(config#relpath(os.path.join(config#g\" ../OpenLoopsStuff/OpenLoops/SConstruct\n \
sed -i -e \"s#rpath=\$(PWD)/\$(OBJDIR) -L\$(PWD)/\$(OBJDIR)#rpath=\$(OBJDIR) -L\$(OBJDIR)#g\" Makefile\n \
sed -i -e \"s#boostrot.o#boostrot.o boostrot4.o#g\" Makefile",
    "ttJ" : "sed -i -e \"s#_PATH) -L#_PATH) #g\" Makefile\n \
sed -i -e \"s# -lvirtual#/libvirtual.so.1.0.0#g\" Makefile",
    "ttH" : "# Use option O0 for ttH (O2 too long)\n \
sed -i 's/O2/O0/g' Makefile\n \
sed -i 's/4.5d0/4.75d0/g' init_couplings.f",
    "gg_H_MSSM" : "sed -i 's/leq/le/g' nloreal.F\n \
cp -p ../gg_H_quark-mass-effects/SLHA.h .\n \
cp -p ../gg_H_quark-mass-effects/SLHADefs.h .",
    }.get(process,"")

def runGetSource_patch_5(process) :
  return {
    "Wgamma" : "echo \"PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o uti.o \" >> tmpfile",
     }.get(process,"echo \"PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o \" >> tmpfile")

def runGetSource_patch_6(process) :
  return {
    "WWJ" : "cp Makefile Makefile.orig\n \
cat Makefile.orig | sed -e \"s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$(scram tool info fastjet | grep BASE | cut -d \"=\" -f2)/bin/fastjet-config#g\" | sed -e \"s#cs_angles.o#cs_angles.o fastjetfortran.o observables.o pwhg_bookhist-multi-new.o#g\" | sed -e \"s#\#\ FASTJET_CONFIG#FASTJET_CONFIG#g\" | sed -e \"s#\#\ LIBSFASTJET#LIBSFASTJET#g\" | sed -e \"s#\#\ FJCXXFLAGS#FJCXXFLAGS#g\" | sed -e \"s#rwl_write_weights_extra.f#rwl_write_weights_extra.f\ rwl_write_weights2_extra.f#g\" > Makefile",
    "ttbarj" : "cp Makefile Makefile.orig\n \
cat Makefile.orig | sed -e \"s#OLPATH=.\+#OLPATH=$(scram tool info OpenLoops | grep BASE | cut -d \"=\" -f2)#g\" > Makefile\n \
sed -i -e \"s#Pythia8Plugins#Pythia8Plugins \$(shell \$(LHAPDF_CONFIG) --cxxflags )#g\" Makefile",
    "gg_H_2HDM" : "echo \"Adding CHAPLIN 1.2 library\"\n \
if [ ! -f chaplin-1.2.tar ]; then\n \
  wget --no-verbose http://chaplin.hepforge.org/code/chaplin-1.2.tar || fail_exit \"Failed to get CHAPLIN tar ball \"\n \
fi\n \
tar xvf chaplin-1.2.tar\n \
cd chaplin-1.2\n \
./configure --prefix=`pwd`/..\n \
make install\n \
cd ..\n \
echo \"LIBS+=-L`pwd`/lib/ -L`pwd`/lib64/\" >> Makefile   # be safe \n \
export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH}",

    "gg_H_MSSM" : "echo \"Adding CHAPLIN 1.2 library\"\n \
if [ ! -f chaplin-1.2.tar ]; then\n \
  wget --no-verbose http://chaplin.hepforge.org/code/chaplin-1.2.tar || fail_exit \"Failed to get CHAPLIN tar ball \"\n \
fi\n \
tar xvf chaplin-1.2.tar\n \
cd chaplin-1.2\n \
./configure --prefix=`pwd`/..\n \
make install\n \
cd ..\n \
echo \"LIBS+=-L`pwd`/lib/ -L`pwd`/lib64/\" >> Makefile   # be safe \n \
export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH}\n\n \
echo \"Adding FeynHiggs 2.10 library\"\n \
if [ ! -f FeynHiggs-2.10.2.tar.gz ]; then\n \
  wget --no-verbose http://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/newversion/FeynHiggs-2.10.2.tar.gz || fail_exit \"Failed to get FeynHiggs tar ball \"\n \
fi\n \
tar xvf FeynHiggs-2.10.2.tar.gz\n \
cd FeynHiggs-2.10.2\n \
./configure --prefix=`pwd`/..\n \
make\n \
make install\n \
cd ..",

    "directphoton" : "echo \"Adding LoopTools 2.14 library\"\n \
if [ ! -f LoopTools-2.14.tar.gz ]; then\n \
  wget --no-verbose http://www.feynarts.de/looptools/LoopTools-2.14.tar.gz || fail_exit \"Failed to get LoopTools tar ball \"\n \
fi\n \
tar xvf LoopTools-2.14.tar.gz\n \
cd LoopTools-2.14\n \
./configure --prefix=`pwd`/..\n \
make install\n \
cd ..\n \
sed -i -e \"s#LT=PathToLoopTools#LT=.#\" Makefile\n \
export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH}\n \
mkdir obj-gfortran",

    "vbs-ssww-nloew" : "echo \"Adding Recola2.2.0 library\"\n \
if [ ! -f recola2-collier-2.2.0.tar.gz ]; then\n \
  wget --no-verbose -O recola2-collier-2.2.0.tar.gz https://recola.hepforge.org/downloads/?f=recola2-collier-2.2.0.tar.gz || fail_exit \"Failed to get Recola tar ball \"\n \
fi\n \
tar -zxvf recola2-collier-2.2.0.tar.gz\n \
cd recola2-collier-2.2.0/build\n \
cmake .. -DCMAKE_Fortran_COMPILER=gfortran -Dmodel=SM\n \
make -j 1\n \
make install\n \
cd ../..\n \
mkdir obj-gfortran/proclib\n \
cd obj-gfortran/proclib\n \
cp ../../recola2-collier-2.2.0/recola2-2.2.0/librecola.so .\n \
cd ../..\n \
cp Makefile Makefile.orig\n \
cat Makefile.orig | sed -e \"s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$(scram tool info fastjet | grep BASE | cut -d \"=\" -f2)/bin/fastjet-config#g\" | sed -e \"s#RECOLALOCATION=.\+#RECOLALOCATION=$\(PWD\)/recola2-collier-2.2.0/recola2-2.2.0#g\" | sed -e \"s# real16.o##g\" | sed -e \"s#test#none#g\" | sed -e \"s#none_Suda#test_Suda#g\" > Makefile\n \
export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH}",
    }.get(process,"")

def runGetSource_patch_7(process) :
  return {
    "HJ" : "echo \"fixing q2min determination for HJ\"\n \
# avoid accessing member 1 for q2min determination. Use member 0, as member 1 may not be available\n \
sed -i \"s/getq2min(1,tmp)/getq2min(0,tmp)/g\" setlocalscales.f",

    "ST_wtch_DR" : "echo \"D/L QCDLoop-1.9 library\"\n \
if [ ! -f FeynHiggs-2.10.2.tar.gz ]; then\n \
  wget --no-verbose http://qcdloop.fnal.gov/QCDLoop-1.96.tar.gz || fail_exit \"Failed to get QCDLoop tar ball\"\n \
fi\n \
tar xvf QCDLoop-1.96.tar.gz\n \
mv QCDLoop-1.96 QCDLoop-1.9\n \
sed -i -e 's#/Users/ellis/QCDLoop#./QCDLoop#' ff/ffinit_mine.f\n \
cd QCDLoop-1.9\n \
make\n \
cd ..",

    "ST_wtch_DS" : "echo \"D/L QCDLoop-1.9 library\"\n \
if [ ! -f FeynHiggs-2.10.2.tar.gz ]; then\n \
  wget --no-verbose http://qcdloop.fnal.gov/QCDLoop-1.96.tar.gz || fail_exit \"Failed to get QCDLoop tar ball\"\n \
fi\n \
tar xvf QCDLoop-1.96.tar.gz\n \
mv QCDLoop-1.96 QCDLoop-1.9\n \
sed -i -e 's#/Users/ellis/QCDLoop#./QCDLoop#' ff/ffinit_mine.f\n \
cd QCDLoop-1.9\n \
make\n \
cd ..",
    }.get(process,"")

def runGetSource_patch_8(process) :
  return {
    "HJ" : "echo \"Compiling HNNLO....\"\n \
wget --no-verbose http://theory.fi.infn.it/grazzini/codes/hnnlo-v2.0.tgz\n \
tar -xzvf hnnlo-v2.0.tgz\n \
cd hnnlo-v2.0\n \
cp ../POWHEG-BOX/HJ/NNLOPS-mass-effects/HNNLO-makefile ./makefile\n \
cp -r ../POWHEG-BOX/HJ/NNLOPS-mass-effects/HNNLO-patches ./\n \
cd src/Need/\n \
cat pdfset_lhapdf.f | sed -e \"s#30#40#g\" | sed -e \"s#20#30#g\" | sed -e \"s#oldPDFname(1:i-1)//'.LHgrid'#oldPDFname(1:i-1)#g\" | sed -e \"s#oldPDFname(1:i-1)//'.LHpdf'#oldPDFname(1:i-1)#g\" | sed -e \"s#InitPDFset('PDFsets/'//PDFname)#InitPDFsetByName(PDFname)#g\" > pdfset_lhapdf.f.new\n \
mv pdfset_lhapdf.f.new pdfset_lhapdf.f\n \
cd -\n \
cat makefile | sed -e \"s#LHAPDFLIB=.\+#LHAPDFLIB=$(scram tool info lhapdf | grep LIBDIR | cut -d \"=\" -f2)#g\" > makefile\n \
make || fail_exit \"Failed to compile HNNLO\"\n \
cp -p bin/hnnlo ${WORKDIR}/${name}/\n \
cp bin/br.* ${WORKDIR}/${name}/\n \
cd ${WORKDIR}/${name}\n \
cp POWHEG-BOX/HJ/NNLOPS-mass-effects/mergedata.f .\n \
gfortran -o mergedata mergedata.f\n\n \
\
cd ${WORKDIR}/${name}/POWHEG-BOX/HJ\n\n \
\
cp Makefile Makefile.orig\n \
cat Makefile.orig | sed -e \"s#ANALYSIS=.\+#ANALYSIS=NNLOPS#g\" |sed -e \"s#\$(shell \$(LHAPDF_CONFIG) --libdir)#$(scram tool info lhapdf | grep LIBDIR | cut -d \"=\" -f2)#g\" | sed -e \"s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$(scram tool info fastjet | grep BASE | cut -d \"=\" -f2)/bin/fastjet-config#g\" | sed -e \"s#NNLOPSREWEIGHTER+=  fastjetfortran.o#NNLOPSREWEIGHTER+=  fastjetfortran.o pwhg_bookhist-multi.o#g\" | sed -e \"s#NNLOPSREWEIGHTERNRW+=  fastjetfortran.o#NNLOPSREWEIGHTERNRW+=  fastjetfortran.o pwhg_bookhist-multi.o#g\" > Makefile\n \
make nnlopsreweighter-newrwgt || fail_exit \"Failed to compile nnlopsreweighter\"\n \
cp nnlopsreweighter-newrwgt ../../\n \
cd ${WORKDIR}/${name}\n \
HMASS=`cat powheg.input | grep \"^hmass\" | cut -d \" \" -f2`;\n \
BEAM=`cat powheg.input | grep \"^ebeam1\" | cut -d \" \" -f2 | tr \"d\" \".\"`;\n \
COMENERGY=`echo \"( $BEAM*2 )\" | bc`\n \
gawk \"/sroot/{gsub(/8000/,$COMENERGY)};/hmass/{gsub(/125.5/, ${HMASS})};/mur,muf/{gsub(/62.750/, $(( $HMASS/2 )))};{print}\" POWHEG-BOX/HJ/PaperRun/HNNLO-LHC8-R04-APX2-11.input | sed -e \"s#10103#SEED#g\" | sed -e \"s#HNNLO-LHC8-R04-APX2-11#HNNLO-LHC13-R04-APX2-11#g\"> HNNLO-LHC13-R04-APX2-11.input\n \
gawk \"/sroot/{gsub(/8000/,$COMENERGY)};/hmass/{gsub(/125.5/, ${HMASS})};/mur,muf/{gsub(/62.750/, $(( $HMASS )))};{print}\" POWHEG-BOX/HJ/PaperRun/HNNLO-LHC8-R04-APX2-11.input | sed -e \"s#10103#SEED#g\" | sed -e \"s#HNNLO-LHC8-R04-APX2-11#HNNLO-LHC13-R04-APX2-22#g\"> HNNLO-LHC13-R04-APX2-22.input\n \
gawk \"/sroot/{gsub(/8000/,$COMENERGY)};/hmass/{gsub(/125.5/, ${HMASS})};/mur,muf/{gsub(/62.750/, $(( $HMASS/4 )))};{print}\" POWHEG-BOX/HJ/PaperRun/HNNLO-LHC8-R04-APX2-11.input | sed -e \"s#10103#SEED#g\" | sed -e \"s#HNNLO-LHC8-R04-APX2-11#HNNLO-LHC13-R04-APX2-0505#g\"> HNNLO-LHC13-R04-APX2-0505.input\n \
cp ${WORKDIR}/Utilities/nnlopsreweighter.input .",

    "Zj" : "echo \"Compiling DYNNLO....\"\n \
wget --no-verbose --no-check-certificate http://theory.fi.infn.it/grazzini/codes/dynnlo-v1.5.tgz\n \
tar -xzvf dynnlo-v1.5.tgz\n \
cd dynnlo-v1.5\n \
cp ../POWHEG-BOX/${process}/DYNNLOPS/${process:0:1}NNLOPS/dynnlo-patches/dynnlo.makefile ./makefile\n \
cp -r -L ../POWHEG-BOX/${process}/DYNNLOPS/${process:0:1}NNLOPS/dynnlo-patches ./\n \
cd src/Need/\n \
cat pdfset_lhapdf.f | sed -e \"s#30#40#g\" | sed -e \"s#20#30#g\" | sed -e \"s#oldPDFname(1:i-1)//'.LHgrid'#oldPDFname(1:i-1)#g\" | sed -e \"s#oldPDFname(1:i-1)//'.LHpdf'#oldPDFname(1:i-1)#g\" | sed -e \"s#InitPDFset('PDFsets/'//PDFname)#InitPDFsetByName(PDFname)#g\" > pdfset_lhapdf.f.new\n \
mv pdfset_lhapdf.f.new pdfset_lhapdf.f\n \
cd -\n \
cat makefile | sed -e \"s#LHAPDFLIB=.\+#LHAPDFLIB=$(scram tool info lhapdf | grep LIBDIR | cut -d \"=\" -f2)#g\" > makefile\n \
make || fail_exit \"Failed to compile DYNNLO\"\n\n \
\
cp -p bin/dynnlo ${WORKDIR}/${name}/\n\n \
\
cd ${WORKDIR}/${name}/POWHEG-BOX/${process}/DYNNLOPS/aux\n \
gfortran -mcmodel=medium -o merge3ddata merge3ddata.f  || fail_exit \"Failed to compile merge3ddata\"\n \
cp merge3ddata ${WORKDIR}/${name}/\n \
gfortran -mcmodel=medium -o mergedata mergedata.f  || fail_exit \"Failed to compile mergedata\"\n \
cp mergedata ${WORKDIR}/${name}/\n\n \
\
cd ${WORKDIR}/${name}/POWHEG-BOX/${process}\n \
make lhef_analysis_3d || fail_exit \"Failed to compile lhef_analysis_3d\"\n \
cp lhef_analysis_3d ${WORKDIR}/${name}/\n\n \
\
cd ${WORKDIR}/${name}\n \
VMASS=`cat powheg.input | grep \"^Wmass\|^Zmass\" | awk '{print $2}' | cut -d \"d\" -f1`\n \
VMASSEXP=`cat powheg.input | grep \"^Wmass\|^Zmass\" | awk '{print $2}' | cut -d \"d\" -f2`\n \
VMASS=`echo \"( $VMASS*10^$VMASSEXP )\" | bc`\n \
VMASSMIN=`cat powheg.input | grep \"^min_W_mass\|^min_Z_mass\" | awk '{print $2}'`\n \
VMASSMAX=`cat powheg.input | grep \"^max_W_mass\|^max_Z_mass\" | awk '{print $2}'`\n \
echo $VMASS\n \
DYNNLOPROC=3\n \
BEAM=`cat powheg.input | grep \"^ebeam1\" | cut -d \" \" -f2 | tr \"d\" \".\"`;\n \
COMENERGY=`echo \"( $BEAM*2 )\" | bc`\n\n \
\
cp POWHEG-BOX/${process}/DYNNLOPS/${process:0:1}NNLOPS/dynnlo-patches/dynnlo.infile dynnlo.infile.orig\n \
gawk \"/sroot/{gsub(/.*!/,$COMENERGY \\\" !\\\")};\\ \n \
      /nproc/{gsub(/.*!/,$DYNNLOPROC \\\" !\\\")};\\ \n \
      /mur/{gsub(/.*!/, $VMASS \\\" \\\" $VMASS \\\" !\\\")};\\ \n \
      /mwmin/{gsub(/.*!/, $VMASSMIN \\\" \\\" $VMASSMAX \\\" !\\\")};\\ \n \
      /rseed/{gsub(/.*!/,\\\"SEED !\\\")};\\ \n \
      /runstring/{gsub(/.*!/,\\\"'SEED' !\\\")};\\ \n \
      {print}\" dynnlo.infile.orig | tee DYNNLO.input",

    "Wj" : "echo \"Compiling DYNNLO....\"\n \
wget --no-verbose --no-check-certificate http://theory.fi.infn.it/grazzini/codes/dynnlo-v1.5.tgz\n \
tar -xzvf dynnlo-v1.5.tgz\n \
cd dynnlo-v1.5\n \
cp ../POWHEG-BOX/${process}/DYNNLOPS/${process:0:1}NNLOPS/dynnlo-patches/dynnlo.makefile ./makefile\n \
cp -r -L ../POWHEG-BOX/${process}/DYNNLOPS/${process:0:1}NNLOPS/dynnlo-patches ./\n \
cd src/Need/\n \
cat pdfset_lhapdf.f | sed -e \"s#30#40#g\" | sed -e \"s#20#30#g\" | sed -e \"s#oldPDFname(1:i-1)//'.LHgrid'#oldPDFname(1:i-1)#g\" | sed -e \"s#oldPDFname(1:i-1)//'.LHpdf'#oldPDFname(1:i-1)#g\" | sed -e \"s#InitPDFset('PDFsets/'//PDFname)#InitPDFsetByName(PDFname)#g\" > pdfset_lhapdf.f.new\n \
mv pdfset_lhapdf.f.new pdfset_lhapdf.f\n \
cd -\n \
cat makefile | sed -e \"s#LHAPDFLIB=.\+#LHAPDFLIB=$(scram tool info lhapdf | grep LIBDIR | cut -d \"=\" -f2)#g\" > makefile\n \
make || fail_exit \"Failed to compile DYNNLO\"\n\n \
\
cp -p bin/dynnlo ${WORKDIR}/${name}/\n\n \
\
cd ${WORKDIR}/${name}/POWHEG-BOX/${process}/DYNNLOPS/aux\n \
gfortran -mcmodel=medium -o merge3ddata merge3ddata.f  || fail_exit \"Failed to compile merge3ddata\"\n \
cp merge3ddata ${WORKDIR}/${name}/\n \
gfortran -mcmodel=medium -o mergedata mergedata.f  || fail_exit \"Failed to compile mergedata\"\n \
cp mergedata ${WORKDIR}/${name}/\n\n \
\
cd ${WORKDIR}/${name}/POWHEG-BOX/${process}\n \
make lhef_analysis_3d || fail_exit \"Failed to compile lhef_analysis_3d\"\n \
cp lhef_analysis_3d ${WORKDIR}/${name}/\n\n \
\
cd ${WORKDIR}/${name}\n \
VMASS=`cat powheg.input | grep \"^Wmass\|^Zmass\" | awk '{print $2}' | cut -d \"d\" -f1`\n \
VMASSEXP=`cat powheg.input | grep \"^Wmass\|^Zmass\" | awk '{print $2}' | cut -d \"d\" -f2`\n \
VMASS=`echo \"( $VMASS*10^$VMASSEXP )\" | bc`\n \
VMASSMIN=`cat powheg.input | grep \"^min_W_mass\|^min_Z_mass\" | awk '{print $2}'`\n \
VMASSMAX=`cat powheg.input | grep \"^max_W_mass\|^max_Z_mass\" | awk '{print $2}'`\n \
echo $VMASS\n \
DYNNLOPROC=1\n \
VID=`cat powheg.input | grep \"^idvecbos\" | awk '{print $2}'`;\n \
if [ \"$VID\" = \"-24\" ]; then\n \
  DYNNLOPROC=2\n \
fi\n \
BEAM=`cat powheg.input | grep \"^ebeam1\" | cut -d \" \" -f2 | tr \"d\" \".\"`;\n \
COMENERGY=`echo \"( $BEAM*2 )\" | bc`\n\n \
\
cp POWHEG-BOX/${process}/DYNNLOPS/${process:0:1}NNLOPS/dynnlo-patches/dynnlo.infile dynnlo.infile.orig\n \
gawk \"/sroot/{gsub(/.*!/,$COMENERGY \\\" !\\\")};\\ \n \
      /nproc/{gsub(/.*!/,$DYNNLOPROC \\\" !\\\")};\\ \n \
      /mur/{gsub(/.*!/, $VMASS \\\" \\\" $VMASS \\\" !\\\")};\\ \n \
      /mwmin/{gsub(/.*!/, $VMASSMIN \\\" \\\" $VMASSMAX \\\" !\\\")};\\ \n \
      /rseed/{gsub(/.*!/,\\\"SEED !\\\")};\\ \n \
      /runstring/{gsub(/.*!/,\\\"'SEED' !\\\")};\\ \n \
      {print}\" dynnlo.infile.orig | tee DYNNLO.input",
    }.get(process,"")
