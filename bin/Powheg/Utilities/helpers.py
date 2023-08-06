import string

def fillTemplatedFile(template_file_name, out_file_name, template_dict, openmode = "a"):
    with open(template_file_name, "r") as templateFile:
        source = string.Template(templateFile.read())
        result = source.substitute(template_dict)
    with open(out_file_name, openmode) as outFile:
        outFile.write(result)

def runGetSource_patch_0(process) :
  return {
   "X0jj" :"echo ' MADGRAPH+POWHEG INSTALL '\n \
cd ${WORKDIR}/${name}\n \
export MG_NAME=MG5_aMC_v2_6_7\n \
echo 'Untar MG5_aMC_v2.6.7'\n \
wget https://launchpad.net/mg5amcnlo/2.0/2.6.x/+download/MG5_aMC_v2.6.7.tar.gz\n \
tar xzvf MG5_aMC_v2.6.7.tar.gz\n \
cd $MG_NAME\n \
echo 'Untar Powheg plugin'\n \
cd PLUGIN\n \
wget https://cms-project-generators.web.cern.ch/cms-project-generators/PWG_MG5_plugin_v0.tgz\n \
tar xzvf PWG_MG5_plugin_v0.tgz\n \
cd ..\n \
echo 'Run mg5_aMC'\n \
./bin/mg5_aMC --mode=MG5aMC_PWG --file=../../examples/V2/X0jj_13TeV/mg5.cmd\n \
echo 'Make POWHEG-BOX link'\n \
cd ${process}\n \
sed -i '/kAgg/c\   15 0.666667e+00 # kAgg' Cards/param_card.dat\n \
ln -s ../../POWHEG-BOX POWHEG-BOX\n \
echo 'Checkout source'\n \
POWHEG-BOX/X0jj/clean_BEFORE_svn-save.sh\n \
cp -rp POWHEG-BOX/X0jj/* .\n \
echo 'Patch Makefile'\n \
sed -i -e 's#../svnversion#POWHEG-BOX/svnversion#g' Makefile\n \
sed -i -e 's#BOX=/[\*]*/POWHEG-BOX-V2#BOX=POWHEG-BOX#g' Makefile\n \
echo 'Copy files for MadLoopParams'\n \
cp SubProcesses/MadLoopParams.dat ${WORKDIR}/${name}\n \
for f in `ls  SubProcesses/MadLoop5_resources/*` ; do\n \
    ln -sf $MG_NAME/${process}/$f ${WORKDIR}/${name}\n \
done\n \
echo 'MADGRAPH+POWHEG END-INSTALL'",
    }.get(process,"")

def runGetSource_patch_1(process) :
  return {
    "WZ" : "patch -l -p0 -i ${patches_dir}/lhapdf_zanderighi.patch",
    "ZZ" : "patch -l -p0 -i ${patches_dir}/lhapdf_zanderighi.patch\n \
patch -l -p0 -i ${patches_dir}/zz_m4lcut.patch",
    "b_bbar_4l" : "cd POWHEG-BOX\n \
patch -l -p0 -i ${patches_dir}/res_openloops_long_install_dir.patch\n \
cd ..",
    "ttb_NLO_dec" : "patch -l -p0 -i ${patches_dir}/pwhg_ttb_NLO_dec_gen_radiation_hook.patch",
    "Zj" : "if [ ${forMiNNLO} -eq 1 ]; then\n \
cd POWHEG-BOX\n \
patch -l -p0 -i ${patches_dir}/pwhg_rm_bad_st1.patch\n \
patch -l -p0 -i ${patches_dir}/pwhg_rwl_add_random.patch\n \
patch -l -p0 -i ${patches_dir}/minnlo_pdf_weights.patch\n \
patch -l -p0 -i ${patches_dir}/minnlo_pdf_representations_init.patch\n \
patch -l -p2 -i ${patches_dir}/minnlo_pdf_ymax.patch\n \
cd ..\n \
fi",
    "Wj" : "if [ ${forMiNNLO} -eq 1 ]; then\n \
cd POWHEG-BOX\n \
patch -l -p0 -i ${patches_dir}/pwhg_rm_bad_st1.patch\n \
patch -l -p0 -i ${patches_dir}/pwhg_rwl_add_random.patch\n \
patch -l -p0 -i ${patches_dir}/minnlo_pdf_weights.patch\n \
patch -l -p0 -i ${patches_dir}/minnlo_pdf_representations_init.patch\n \
patch -l -p2 -i ${patches_dir}/minnlo_pdf_ymax.patch\n \
cd ..\n \
fi",
    "HJ" : "if [ ${forMiNNLO} -eq 1 ]; then\n \
cd POWHEG-BOX\n \
patch -l -p0 -i ${patches_dir}/pwhg_rm_bad_st1.patch\n \
patch -l -p0 -i ${patches_dir}/pwhg_rwl_add_random.patch\n \
patch -l -p0 -i ${patches_dir}/minnlo_pdf_weights.patch\n \
patch -l -p0 -i ${patches_dir}/minnlo_pdf_representations_init.patch\n \
patch -l -p2 -i ${patches_dir}/minnlo_pdf_ymax.patch\n \
cd ..\n \
fi",
    "VBF_H_smeft" : "cd POWHEG-BOX/VBF_H_smeft\n \
head -n 966 pwhg_analysis.f | tail -n 12 > pwhg_analysis_new.f\n \
mv pwhg_analysis_new.f pwhg_analysis.f\n \
cd ../..",
    }.get(process,"")

def runGetSource_patch_2(process) :
  return {
    "Zj" : "if [ ${forMiNNLO} -eq 1 ]; then\n \
patch -l -p0 -i ${WORKDIR}/patches/zj_minnlo_scheme_weights.patch\n \
cd ZjMiNNLO\n \
patch -l -p0 -i ${WORKDIR}/patches/vj_minnlo_rwl_pdf_optimization.patch\n \
patch -l -p0 -i ${WORKDIR}/patches/vj_minnlo_compiler_flags.patch\n \
fi",
    "Wj" : "if [ ${forMiNNLO} -eq 1 ]; then\n \
patch -l -p0 -i ${WORKDIR}/patches/wj_minnlo_scheme_weights.patch\n \
cd WjMiNNLO\n \
patch -l -p0 -i ${WORKDIR}/patches/vj_minnlo_rwl_pdf_optimization.patch\n \
patch -l -p0 -i ${WORKDIR}/patches/vj_minnlo_compiler_flags.patch\n \
fi",
    "HJ" : "if [ ${forMiNNLO} -eq 1 ]; then\n \
cd HJMiNNLO\n \
patch -l -p0 -i ${WORKDIR}/patches/vj_minnlo_rwl_pdf_optimization.patch\n \
patch -l -p0 -i ${WORKDIR}/patches/vj_minnlo_compiler_flags.patch\n \
fi",
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
sed -i -e \"s#rpath=\$(PWD)/\$(OBJDIR) -L\$(PWD)/\$(OBJDIR)#rpath=\$(OBJDIR) -L\$(OBJDIR)#g\" Makefile\n \
sed -i -e \"s#PDFPACK=lhapdfif.o#PDFPACK=lhapdf6if.o lhapdf6ifcc.o#g\" Makefile\n \
cat ${patches_dir}/missing_lhapdf6.txt >> Makefile",   
    "HZ_ew" : "# fix fortran options/linking to OpenLoops/missing libraries in VH_ew\n \
sed -i -e \"s#OL_process_src#OL_process_src f90_flags=-ffree-line-length-none#g\" Makefile\n \
sed -i -e \"s#\$(PWD)/\$(OBJ)#\$(OBJ)#g\" Makefile\n \
sed -i -e \"s#\$(OLPATH)/lib_src#lib_src#g\" Makefile\n \
sed -i -e \"s#cd \$(OLPATH)#cp -r \$(OLPATH)/* .#g\" Makefile\n \
sed -i -e \"s#abspath(os.path.join(config#relpath(os.path.join(config#g\" ../OpenLoopsStuff/OpenLoops/SConstruct\n \
sed -i -e \"s#rpath=\$(PWD)/\$(OBJDIR) -L\$(PWD)/\$(OBJDIR)#rpath=\$(OBJDIR) -L\$(OBJDIR)#g\" Makefile\n \
sed -i -e \"s#opencount.o#opencount.o sigequiv_hook.o#g\" Makefile\n \
sed -i -e \"s#PDFPACK=lhapdfif.o#PDFPACK=lhapdf6if.o lhapdf6ifcc.o#g\" Makefile\n \
cat ${patches_dir}/missing_lhapdf6.txt >> Makefile",
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
sed -i -e \"s#opencount.o#opencount.o sigequiv_hook.o#g\" Makefile\n \
sed -i -e \"s#PDFPACK=lhapdfif.o#PDFPACK=lhapdf6if.o lhapdf6ifcc.o#g\" Makefile\n \
sed -i -e \"s#boostrot.o#boostrot.o boostrot4.o#g\" Makefile\n \
cat ${patches_dir}/missing_lhapdf6.txt >> Makefile",
    "ttJ" : "sed -i -e \"s#_PATH) -L#_PATH) #g\" Makefile\n \
sed -i -e \"s# -lvirtual#/libvirtual.so.1.0.0#g\" Makefile",
    "ttH" : "# Use option O0 for ttH (O2 too long)\n \
sed -i 's/O2/O0/g' Makefile\n \
sed -i 's/4.5d0/4.75d0/g' init_couplings.f",
    "gg_H_MSSM" : "sed -i 's/leq/le/g' nloreal.F\n \
cp -p ../gg_H_quark-mass-effects/SLHA.h .\n \
cp -p ../gg_H_quark-mass-effects/SLHADefs.h .",
    "HJ/HJMiNNLO" : "sed -i -e \"s#PDF=hoppet#PDF=lhapdf#\" Makefile \n \
     sed -i -e \"s#internal_parameters.o coefficient_functions_nnlops.o#internal_parameters.o coefficient_functions_nnlops.o sudakov_radiators.o#\" Makefile",    
  }.get(process,"")

def runGetSource_patch_5(process) :
  return {
    "Wgamma" : "echo \"PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o uti.o \" >> tmpfile",
     }.get(process,"echo \"PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o \" >> tmpfile")

def runGetSource_patch_6(process) :
  return {
    "WWJ" : "cp Makefile Makefile.orig\n \
cat Makefile.orig | sed -e \"s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$(scram tool info fastjet | grep BASE | cut -d \"=\" -f2)/bin/fastjet-config#g\" | sed -e \"s#\#\ FASTJET_CONFIG#FASTJET_CONFIG#g\" | sed -e \"s#\#\ LIBSFASTJET#LIBSFASTJET#g\" | sed -e \"s#\#\ FJCXXFLAGS#FJCXXFLAGS#g\" > Makefile\n \
cd ${WORKDIR}/${name}/POWHEG-BOX/MATRIXStuff\n \
./matrix --minnlo_interface\n \
cd -\n \
cd ${WORKDIR}/${name}/POWHEG-BOX/WWJ\n \
wget --no-verbose --no-check-certificate https://wwwth.mpp.mpg.de/members/wieseman/download/codes/WW_MiNNLO/VVamp_interpolation_grids/WW_MiNNLO_2loop_grids_reduced1.tar.gz\n \
tar xzf WW_MiNNLO_2loop_grids_reduced1.tar.gz\n \
cd -\n \
source /cvmfs/cms.cern.ch/${SCRAM_ARCH}/external/cmake/3.10.0/etc/profile.d/init.sh",
    "ZZJ" : "cp Makefile Makefile.orig\n \
cat Makefile.orig | sed -e \"s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$(scram tool info fastjet | grep BASE | cut -d \"=\" -f2)/bin/fastjet-config#g\" | sed -e \"s#\#\ FASTJET_CONFIG#FASTJET_CONFIG#g\" > Makefile\n \
cd ${WORKDIR}/${name}/POWHEG-BOX/MATRIXStuff\n \
./matrix --minnlo_interface\n \
cd -\n \
source /cvmfs/cms.cern.ch/${SCRAM_ARCH}/external/cmake/3.10.0/etc/profile.d/init.sh",
    "ZgamJ" : "cp Makefile Makefile.orig\n \
cat Makefile.orig | sed -e \"s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$(scram tool info fastjet | grep BASE | cut -d \"=\" -f2)/bin/fastjet-config#g\" | sed -e \"s#\#\ FASTJET_CONFIG#FASTJET_CONFIG#g\" > Makefile\n \
tail -n 13 ../MiNNLOStuff/setlocalscales.f >> setlocalscales.f\n \
cd ${WORKDIR}/${name}/POWHEG-BOX/MATRIXStuff\n \
./matrix --minnlo_interface\n \
cd -\n \
source /cvmfs/cms.cern.ch/${SCRAM_ARCH}/external/cmake/3.10.0/etc/profile.d/init.sh",
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

    "HJJ_ew" : "echo \"Adding Recola2.2.0 library\"\n \
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
cat Makefile.orig | sed -e \"s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$(scram tool info fastjet | grep BASE | cut -d \"=\" -f2)/bin/fastjet-config#g\" | sed -e \"s#RECOLALOCATION=.\+#RECOLALOCATION=$\(PWD\)/recola2-collier-2.2.0/recola2-2.2.0#g\" > Makefile\n \
export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH}",

    "VV_dec_ew" : "echo \"Adding Recola2.2.0 library\"\n \
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
cat Makefile.orig | sed -e \"s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$(scram tool info fastjet | grep BASE | cut -d \"=\" -f2)/bin/fastjet-config#g\" | sed -e \"s#RECOLALOCATION=.\+#RECOLALOCATION=$\(PWD\)/recola2-collier-2.2.0/recola2-2.2.0#g\" > Makefile\n \
export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH}",

    "Wtt_dec" : " cd ../../\n \
echo \"Adding NLOX libraries to: $(pwd)\"\n \
if [ ! -f NLOX_util_1.2.0.tar.gz ]; then\n \
  wget --no-verbose --user NLOX --password LoopsAreCool http://www.hep.fsu.edu/~nlox/downloads/v1.2.0/NLOX_util_1.2.0.tar.gz || fail_exit \"Failed to get NLOX_util_1.2.0 tar ball\"\n \
fi\n \
if [ ! -f NLOX_1.2.0.tar.gz ]; then\n \
  wget --user NLOX --password LoopsAreCool http://www.hep.fsu.edu/~nlox/downloads/v1.2.0/NLOX_1.2.0.tar.gz || fail_exit \"Failed to get NLOX_1.2.0 tar ball\"\n \
fi\n \
if [ ! -d NLOX_util_1.2.0 ]; then\n \
    tar -xzvf NLOX_util_1.2.0.tar.gz\n \
fi\n \
if [ ! -d NLOX_1.2.0 ]; then\n \
    tar -xzvf NLOX_1.2.0.tar.gz\n \
fi\n \
cd NLOX_util_1.2.0\n \
export abs_NLOX_util_path=$(pwd)\n \
echo \"Setting abs_NLOX_util_path=\"${abs_NLOX_util_path}\n \
./install_nlox_util.sh --prefix=${abs_NLOX_util_path}\n \
cd OneLOop-3.6\n \
export abs_OneLOop_path=$(pwd)\n \
echo \"Exporting OneLOop-3.6 path: \" ${abs_OneLOop_path} \n \
cd ../QCDLoop-1.95\n \
export abs_QCDLoop_path=`pwd`\n \
echo \"Exporting QCDLoop-1.95 path:\" ${abs_QCDLoop_path}\n \
cd ../../NLOX_1.2.0\n \
export abs_NLOX_path=`pwd`\n \
echo \"Setting abs_NLOX_path=\" ${abs_NLOX_path}\n \
./install_nlox.sh --with-nloxutil=${abs_NLOX_util_path}\n \
if [ ! -f pp_Wpttbar.tar.gz ]; then\n \
  wget --user NLOX --password LoopsAreCool http://www.hep.fsu.edu/~nlox/downloads/processes/v1.2.0/pp_Wpttbar.tar.gz || fail_exit \"Failed to get pp_Wpttbar tar ball\"\n \
fi\n \
if [ ! -f pp_Wmttbar.tar.gz ]; then\n \
  wget --user NLOX --password LoopsAreCool http://www.hep.fsu.edu/~nlox/downloads/processes/v1.2.0/pp_Wmttbar.tar.gz || fail_exit \"Failed to get pp_Wmttbar tar ball\"\n \
fi\n \
tar -xzvf pp_Wpttbar.tar.gz\n \
tar -xzvf pp_Wmttbar.tar.gz\n \
cd pp_Wpttbar\n \
echo \"Editing and compiling Makefiles in \" $(pwd)\n \
sed -i -e \"s|# NLOX_DIR=<nlox_builddir>|NLOX_DIR=${abs_NLOX_path}|\" Makefile_process\n \
sed -i -e \"s|# NLOX_UTIL_DIR=<nlox_util_builddir>|NLOX_UTIL_DIR=${abs_NLOX_util_path}|\" Makefile_process\n \
make -j 10 library flibrary -f Makefile_process\n \
cd ../pp_Wmttbar\n \
echo \"Editing and compiling Makefiles in $(pwd)/pp_Wmttbar\"\n \
sed -i -e \"s|# NLOX_DIR=<nlox_builddir>|NLOX_DIR=${abs_NLOX_path}|\" Makefile_process\n \
sed -i -e \"s|# NLOX_UTIL_DIR=<nlox_util_builddir>|NLOX_UTIL_DIR=${abs_NLOX_util_path}|\" Makefile_process\n \
make -j 10 library flibrary -f Makefile_process\n \
cd ../..\n \
export process=\"Wtt_dec\"\n \
cd POWHEG-BOX/${process}/pp_ttWp_EW\n \
echo \"Editing and compiling Makefiles in `pwd`\"\n \
LHAPDF_BASE=`scram tool info lhapdf | grep LHAPDF_BASE |cut -d \"=\" -f2`\n \
echo \"LHAPDF_BASE: ${LHAPDF_BASE}\"\n \
sed -i -e \"s|ONELOOPDIR=\$(NLOX_UTIL_DIR)\/lib\/NLOX_util\/|ONELOOPDIR=${abs_OneLOop_path}|\" Makefile\n \
sed -i -e \"s|QCDLOOPDIR=\$(ONELOOPDIR)|QCDLOOPDIR=${abs_QCDLoop_path}|\" Makefile\n \
sed -i -e \"s|COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/lib64|COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/ff/|\" Makefile\n \
sed -i \"0,/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/! {0,/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/ s/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/COMMON_LIB_DIRS += -L\$(QCDLOOPDIR)\/ql\//}\" Makefile\n \
sed -i -e \"s|#LHAPDF_CONFIG=\$(HOME)\/local|LHAPDF_CONFIG=${LHAPDF_BASE}|\" Makefile\n \
sed -i -e \"s|#NLOX_DIR=\$(HOME)\/NLOX|NLOX_DIR=${abs_NLOX_path}|\" Makefile\n \
sed -i -e \"s|#NLOX_UTIL_DIR=\$(HOME)\/NLOX\/UTIL\/|NLOX_UTIL_DIR=${abs_NLOX_util_path}|\" Makefile\n \
make clean \n \
make\n \
cd ../pp_ttWp_QCD\n \
echo \"Editing and compiling Makefiles in `pwd`\"\n \
sed -i -e \"s|ONELOOPDIR=\$(NLOX_UTIL_DIR)\/lib\/NLOX_util\/|ONELOOPDIR=${abs_OneLOop_path}|\" Makefile\n \
sed -i -e \"s|QCDLOOPDIR=\$(ONELOOPDIR)|QCDLOOPDIR=${abs_QCDLoop_path}|\" Makefile\n \
sed -i -e \"s|COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/lib64|COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/ff/|\" Makefile\n \
sed -i \"0,/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/! {0,/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/ s/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/COMMON_LIB_DIRS += -L\$(QCDLOOPDIR)\/ql\//}\" Makefile\n \
sed -i -e \"s|#LHAPDF_CONFIG=\$(HOME)\/local|LHAPDF_CONFIG=${LHAPDF_BASE}|\" Makefile\n \
sed -i -e \"s|#NLOX_DIR=\$(HOME)\/NLOX|NLOX_DIR=${abs_NLOX_path}|\" Makefile\n \
sed -i -e \"s|#NLOX_UTIL_DIR=\$(HOME)\/NLOX\/UTIL\/|NLOX_UTIL_DIR=${abs_NLOX_util_path}|\" Makefile\n \
make clean \n \
make\n \
cd ../pp_ttWm_EW\n \
echo \"Editing and compiling Makefiles in `pwd`\"\n \
sed -i -e \"s|ONELOOPDIR=\$(NLOX_UTIL_DIR)\/lib\/NLOX_util\/|ONELOOPDIR=${abs_OneLOop_path}|\" Makefile\n \
sed -i -e \"s|QCDLOOPDIR=\$(ONELOOPDIR)|QCDLOOPDIR=${abs_QCDLoop_path}|\" Makefile\n \
sed -i -e \"s|COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/lib64|COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/ff/|\" Makefile\n \
sed -i \"0,/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/! {0,/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/ s/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/COMMON_LIB_DIRS += -L\$(QCDLOOPDIR)\/ql\//}\" Makefile\n \
sed -i -e \"s|#LHAPDF_CONFIG=\$(HOME)\/local|LHAPDF_CONFIG=${LHAPDF_BASE}|\" Makefile\n \
sed -i -e \"s|#NLOX_DIR=\$(HOME)\/NLOX|NLOX_DIR=${abs_NLOX_path}|\" Makefile\n \
sed -i -e \"s|#NLOX_UTIL_DIR=\$(HOME)\/NLOX\/UTIL\/|NLOX_UTIL_DIR=${abs_NLOX_util_path}|\" Makefile\n \
make clean \n \
make\n \
cd ../pp_ttWm_QCD\n \
echo \"Editing and compiling Makefiles in $(pwd)\"\n \
sed -i -e \"s|ONELOOPDIR=\$(NLOX_UTIL_DIR)\/lib\/NLOX_util\/|ONELOOPDIR=${abs_OneLOop_path}|\" Makefile\n \
sed -i -e \"s|QCDLOOPDIR=\$(ONELOOPDIR)|QCDLOOPDIR=${abs_QCDLoop_path}|\" Makefile\n \
sed -i -e \"s|COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/lib64|COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/ff/|\" Makefile\n \
sed -i \"0,/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/! {0,/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/ s/COMMON_LIB_DIRS+=-L\$(QCDLOOPDIR)/COMMON_LIB_DIRS += -L\$(QCDLOOPDIR)\/ql\//}\" Makefile\n \
sed -i -e \"s|#LHAPDF_CONFIG=\$(HOME)\/local|LHAPDF_CONFIG=${LHAPDF_BASE}|\" Makefile\n \
sed -i -e \"s|#NLOX_DIR=\$(HOME)\/NLOX|NLOX_DIR=${abs_NLOX_path}|\" Makefile\n \
sed -i -e \"s|#NLOX_UTIL_DIR=\$(HOME)\/NLOX\/UTIL\/|NLOX_UTIL_DIR=${abs_NLOX_util_path}|\" Makefile\n \
make clean \n \
make\n \
cd ../\n \
echo \"Leaving patch6 for ${process} in directory $(pwd)\"\n \
    ",
    "Z_ew-BMNNPV" : "patch -l -p0 -i ${patches_dir}/z_ew.patch\n \
## put the correct library names for PHOTOS++ into the Makefile\n \
echo 'Linking PHOTOS++ libraries in the Makefile'\n \
sed -i 's+^PHOTOSCC_LOCATION=.*+PHOTOSCC_LOCATION=/cvmfs/cms.cern.ch/slc6_amd64_gcc700/external/photospp/3.61-omkpbe2+g' Makefile\n \
sed -i '/lPhotosFortran/s/^/#/g' Makefile\n \
sed -i '/lPhotospp/s/^# //g' Makefile\n \
## make the main-PHOTOS-lhef\n \
echo 'Making main-PHOTOS-lhef'\n \
make main-PHOTOS-lhef",
    "W_ew-BMNNP" : "patch -l -p0 -i ${patches_dir}/w_ew.patch\n \
## put the correct library names for PHOTOS++ into the Makefile\n \
echo 'Linking PHOTOS++ libraries in the Makefile'\n \
sed -i 's+^PHOTOSCC_LOCATION=.*+PHOTOSCC_LOCATION=/cvmfs/cms.cern.ch/slc6_amd64_gcc700/external/photospp/3.61-omkpbe2+g' Makefile\n \
sed -i '/lPhotosFortran/s/^/#/g' Makefile\n \
sed -i '/lPhotospp/s/^# //g' Makefile\n \
## make the main-PHOTOS-lhef\n \
echo 'Making main-PHOTOS-lhef'\n \
make main-PHOTOS-lhef",
    "ttJ_MiNNLO" :"echo \"Adding CHAPLIN 1.2 library\"\n \
if [ ! -f chaplin-1.2.tar ]; then\n \
  wget --no-verbose http://chaplin.hepforge.org/code/chaplin-1.2.tar || fail_exit \"Failed to get CHAPLIN tar ball \"\n \
fi\n \
tar xvf chaplin-1.2.tar\n \
cd chaplin-1.2\n \
./configure --prefix=`pwd`/..\n \
make install\n \
cd ..\n \
echo \"LIBS+=-L`pwd`/lib/ -L`pwd`/lib64/\" >> Makefile   # be safe \n \
export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH} \n \
echo \"Editing and compiling Makefiles in `pwd`\" \n \
PROCESSDIR=\"${WORKDIR}/${name}/POWHEG-BOX/${process}\" \n \
cp H2Stuff/virtgg.f  H2Stuff/virtgg.f.orig\n \
cp H2Stuff/virtqq.f  H2Stuff/virtqq.f.orig\n \
cat H2Stuff/virtgg.f.orig | sed -e \"s#PROCESSDIR#${PROCESSDIR}#g\" > H2Stuff/virtgg.f \n \
cat H2Stuff/virtqq.f.orig | sed -e \"s#PROCESSDIR#${PROCESSDIR}#g\" > H2Stuff/virtqq.f \n \
LHAPDF_BASE=/cvmfs/cms.cern.ch/slc7_amd64_gcc900/external/lhapdf/6.3.0/\n \
echo \"LHAPDF_BASE: ${LHAPDF_BASE}\"\n \
cp Makefile Makefile.orig\n \
cat Makefile.orig | sed -e \"s#LHAPDF_CONFIG=.\+#LHAPDF_CONFIG=${LHAPDF_BASE}bin/lhapdf-config#g\" > Makefile\n \
patch -l -p0 -i ${WORKDIR}/patches/ttJ_minnlo_compiler.patch", 
}.get(process,"")

def runGetSource_patch_7(process) :
  return {
    "HJ" : "echo \"fixing q2min determination for HJ\"\n \
# avoid accessing member 1 for q2min determination. Use member 0, as member 1 may not be available\n \
sed -i \"s/getq2min(1,tmp)/getq2min(0,tmp)/g\" setlocalscales.f",

    "ST_wtch_DR" : "echo \"D/L QCDLoop-1.9 library\"\n \
if [ ! -f FeynHiggs-2.10.2.tar.gz ]; then\n \
  wget --no-verbose http://qcdloop.fnal.gov/QCDLoop-1.98.tar.gz || fail_exit \"Failed to get QCDLoop tar ball\"\n \
fi\n \
tar xvf QCDLoop-1.98.tar.gz\n \
mv QCDLoop-1.98 QCDLoop-1.9\n \
sed -i -e 's#/Users/ellis/QCDLoop#./QCDLoop#' ff/ffinit_mine.f\n \
cd QCDLoop-1.9\n \
sed -i -e 's#FFLAGS = #FFLAGS = -std=legacy #g' makefile\n \
sed -i -e 's#FFLAGS        = #FFLAGS        = -std=legacy #g' ff/makefile\n \
sed -i -e 's#FFLAGS  = #FFLAGS  = -std=legacy #g' ql/makefile\n \
make\n \
cd ..\n \
sed -i -e 's#QCDLoop-1.98#QCDLoop-1.9#g' Makefile",

    "ST_wtch_DS" : "echo \"D/L QCDLoop-1.9 library\"\n \
if [ ! -f FeynHiggs-2.10.2.tar.gz ]; then\n \
  wget --no-verbose http://qcdloop.fnal.gov/QCDLoop-1.98.tar.gz || fail_exit \"Failed to get QCDLoop tar ball\"\n \
fi\n \
tar xvf QCDLoop-1.98.tar.gz\n \
mv QCDLoop-1.98 QCDLoop-1.9\n \
sed -i -e 's#/Users/ellis/QCDLoop#./QCDLoop#' ff/ffinit_mine.f\n \
cd QCDLoop-1.9\n \
sed -i -e 's#FFLAGS = #FFLAGS = -std=legacy #g' makefile\n \
sed -i -e 's#FFLAGS        = #FFLAGS        = -std=legacy #g' ff/makefile\n \
sed -i -e 's#FFLAGS  = #FFLAGS  = -std=legacy #g' ql/makefile\n \
make\n \
cd ..\n \
sed -i -e 's#QCDLoop-1.98#QCDLoop-1.9#g' Makefile",
    }.get(process,"")

def runGetSource_patch_8(process) :
  return {
    "HJ" : "if [ ${forMiNNLO} -eq 0 ]; then\n \
echo \"Compiling HNNLO....\"\n \
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
cp ${WORKDIR}/Utilities/nnlopsreweighter.input .\n \
fi",
    "ttJ_MiNNLO" :"echo \"Renaming pwhg_main-gnu to pwhg_main .\"\n \
cd ${WORKDIR}/${name}/POWHEG-BOX/${process} \n \
mv pwhg_main-gnu pwhg_main \n \
cp -p pwhg_main ${WORKDIR}/${name}/. \n \
cd -",
    }.get(process,"")
