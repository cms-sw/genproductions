#!/usr/bin/python

'''
Script for POWHEG generator production
By Yuan CHAO  03/30/2015
Based on Pietro's script
'''

import commands
import fileinput
# import argparse
import sys
import os
from optparse import OptionParser

TESTING = 0
QUEUE = ''

rootfolder = os.getcwd()


def runCommand(command, printIt = False, doIt = 1, TESTING = 0) :
    if TESTING : 
        printIt = 1
        doIt = 0
    if printIt : print ('> ' + command)
    if doIt : 
        commandOutput = commands.getstatusoutput(command)
        if printIt : print commandOutput[1]
        return commandOutput[0]
    else :    print ('    jobs not submitted')
    return 1
    
# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def prepareJob(tag, i, folderName) :
    filename = folderName+'/run_' + tag + '.sh'
    f = open(filename, 'w')

    f.write('#!/bin/bash \n')
    f.write('fail_exit() { echo "$@" 1>&2; exit 1; } \n\n')

    f.write('echo "Start of job on " `date`\n\n')

    f.write('cd '+os.getcwd()+'\n\n')
#    f.write('pwd \n\n')

    f.write('source /cvmfs/cms.cern.ch/cmsset_default.sh\n\n')
    f.write('eval `scramv1 runtime -sh`\n\n')

    f.write('### Prepare environments for FastJet ### \n\n')

    f.write('export FASTJET_BASE=`scram tool info fastjet | grep FASTJET_BASE | sed -e s%FASTJET_BASE=%%`\n')
    f.write('export PATH=$FASTJET_BASE/bin/:$PATH \n')

    f.write('### Prepare environments for LHAPDF ### \n\n')
    
    f.write('LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml    \n')
    f.write('if [ -e $LHAPDF6TOOLFILE ]; then    \n')
    f.write('   export LHAPDF_BASE=`cat $LHAPDF6TOOLFILE | grep "<environment name=\\"LHAPDF6_BASE\\"" | cut -d \\" -f 4`    \n')
    f.write('else    \n')
    f.write('   export LHAPDF_BASE=`scram tool info lhapdf | grep LHAPDF_BASE | sed -e s%LHAPDF_BASE=%%`    \n')
    f.write('fi    \n')

    f.write('echo "LHAPDF_BASE is set to:" $LHAPDF_BASE \n')
    f.write('export PATH=$LHAPDF_BASE/bin/:$PATH \n')
#    f.write('export LHAPATH=`scram tool info lhapdf | grep LHAPATH | sed -e s%LHAPATH=%%`\n')
    f.write('export LHAPDF_DATA_PATH=`$LHAPDF_BASE/bin/lhapdf-config --datadir` \n')
#    f.write('export LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml \n\n')
#    f.write('cd ' + rootfolder + '/' + folderName + '\n')
    f.write ('cd -' + '\n')
    f.write ('echo "I am here:"' + '\n')
    f.write ('pwd' + '\n')
    f.write ('cp -p ' + rootfolder + '/' + folderName + '/powheg.input ./' + '\n')
    f.write ('cp -p ' + rootfolder + '/' + folderName + '/JHUGen.input ./' + '\n')
    f.write ('cp -p ' + rootfolder + '/' + folderName + '/*.dat  ./' + '\n') 
    f.write ('cp -p ' + rootfolder + '/' + folderName + '/pwhg_main  ./' + '\n')
    f.write ('if [ -e '+ rootfolder + '/' + folderName + '/obj-gfortran/proclib ]; then    \n')
    f.write ('  mkdir ./obj-gfortran/' + '\n')
    f.write ('  cp -pr ' + rootfolder + '/' + folderName + '/obj-gfortran/proclib  ./obj-gfortran/' + '\n')
    f.write ('  cp -pr ' + rootfolder + '/' + folderName + '/obj-gfortran/*.so  ./obj-gfortran/' + '\n')
    f.write ('fi    \n')

    f.write('\n')

    f.close()
    return filename


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def prepareJobForEvents (tag, i, folderName, EOSfolder) :
    runCommand('rm ' + rootfolder + '/' + folderName + '/log_' + tag + '.log')
    filename = 'run_' + tag + '.sh'

    prepareJob(tag, i, folderName)

    f = open (filename, 'a')
    f.write ('cp -p ' + rootfolder + '/' + folderName + '/*.dat  ./' + '\n')
    f.write ('if [ -e '+ rootfolder + '/' + folderName + '/obj-gfortran/proclib ]; then    \n')
    f.write ('  mkdir ./obj-gfortran/' + '\n')
    f.write ('  cp -pr ' + rootfolder + '/' + folderName + '/obj-gfortran/proclib  ./obj-gfortran/' + '\n')
    f.write ('  cp -pr ' + rootfolder + '/' + folderName + '/obj-gfortran/*.so  ./obj-gfortran/' + '\n')
    f.write ('fi    \n')

    f.write ('cd -' + '\n')

    f.write ('pwd' + '\n')
    f.write ('ls' + '\n')
    f.write ('echo ' + str (i) + ' | ' + rootfolder + '/pwhg_main &> log_' + tag + '.log ' + '\n')
    f.write ('cp -p log_' + tag + '.log ' + rootfolder + '/' + folderName + '/. \n')
    #lhefilename = 'pwgevents-{:04d}.lhe'.format(i) 

    #f.write ('cmsStage ' + lhefilename + ' /store/user/govoni/LHE/powheg/' + EOSfolder + '/\n')
    #f.write ('rm -f ' + lhefilename + '\n')

    f.close ()
    return filename


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def runParallelXgrid(parstage, xgrid, folderName, nEvents, njobs, powInputName, jobtag, rndSeed, process) :
    # parstage, xgrid are strings!

    print 'Running parallel jobs for grid'
    #print folderName

    inputName = folderName + "/powheg.input"

    sedcommand = 'sed -i "s/NEVENTS/' + nEvents + '/ ; s/SEED/'+rndSeed+'/ ; s/.*parallelstage.*/parallelstage '+parstage+'/ ; s/.*xgriditeration.*/xgriditeration '+xgrid+'/ ; s/.*manyseeds.*/manyseeds 1/ ; s/fakevirt.*// " '+inputName

    #print sedcommand
    runCommand(sedcommand)

    if(parstage == '1') :
        if not 'parallelstage' in open(inputName).read() :
            runCommand("echo \'\n\nparallelstage "+parstage+"\' >> "+inputName)
        if not 'xgriditeration' in open(inputName).read() :
            runCommand("echo \'xgriditeration "+xgrid+"\' >> "+inputName)

        if not 'manyseeds' in open(inputName).read() :
            runCommand("echo \'manyseeds 1\' >> "+ inputName)

        if not 'fakevirt' in open(inputName).read() :
            if process != 'b_bbar_4l':
                runCommand("echo \'fakevirt 1\' >> "+inputName)

        #if process == 'ttH' :
        #    if not 'ncall2' in open(inputName).read() :
        #        runCommand("echo \'ncall2 0\' >> "+inputName)
        #    else :
        #        runCommand("sed -i \'s/ncall2.*/ncall2 0/g\' "+inputName)

    #runCommand('cp -p powheg.input ' + folderName)

    runCommand('cp -p '+inputName+' '+inputName+'.'+parstage+'_'+str(xgrid))

    for i in range (1, njobs + 1) :
        jobID = jobtag + '_' + str(i)
        jobname = prepareJob(jobID, i, folderName)

        filename = folderName+'/run_' + jobID + '.sh'
        f = open(filename, 'a')
        #f.write('cd '+rootfolder+'/'+folderName+'/ \n')
        f.write('echo ' + str (i) + ' | ./pwhg_main &> run_' + jobID + '.log ' + '\n')
        f.write('cp -p *.top ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p *.dat ' + rootfolder + '/' + folderName + '/. \n')
        f.write('cp -p *.log ' + rootfolder + '/' + folderName + '/. \n')

        f.close()

        os.system('chmod 755 '+filename)

        #runCommand('bsub -J ' + jobID + ' -u pippopluto -q ' + QUEUE + ' < ' + jobname, 1, TESTING == 0)

        if QUEUE == '':
            print 'Direct running... #'+str(i)+' \n'
            #print 'cd '+rootfolder+'/'+folderName
            os.system('cd '+rootfolder+'/'+folderName+';bash run_'+jobID+'.sh &')
        else:
            print 'Submitting to queue: '+QUEUE+' #'+str(i)+' \n'
            runCommand ('bsub -J ' + jobID + ' -u $USER -q ' + QUEUE + ' '+rootfolder+'/'+folderName+'/run_'+jobID+'.sh ', TESTING == 0)


    #runCommand ('mv *.sh ' + folderName)


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
def runSingleXgrid(parstage, xgrid, folderName, nEvents, powInputName, seed, process, scriptName) :

    print 'Running single job for grid'
    #print folderName

    inputName = folderName + "/powheg.input"

#    runCommand('mv -f powheg.input powheg.input.temp')
#    sedcommand = 'sed "s/parallelstage.*/parallelstage ' + parstage + '/ ; s/xgriditeration.*/xgriditeration ' + xgrid + '/" ' + powInputName + ' > ' + folderName + '/powheg.input'
    sedcommand = 'sed "s/NEVENTS/' + nEvents + '/ ; s/SEED/' + seed + '/" ' + powInputName + ' > ' + folderName + '/powheg.input'

    runCommand(sedcommand)

    #prepareJob('Xgrid', '', folderName)

    #runCommand('rm -f ' + folderName + '/run_Xgrid.sh')
    #runCommand('cp -p run_Xgrid.sh ' + folderName + '/.')

    #filename = folderName + '/run_Xgrid.sh'
    filename = scriptName

    f = open(filename, 'a')
    f.write('cd '+rootfolder+'/'+folderName+'/ \n')

    f.write('export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH} \n\n')
    #f.write('echo $LD_LIBRARY_PATH \n')

    f.write('sed -i "s/NEVENTS/'+nEvents+'/ ; s/SEED/'+seed+'/" powheg.input\n\n')

    if process == 'gg_H_MSSM' :
        if os.path.exists(powInputName) :
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/powheg-fh.in . \n')
        else :
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/powheg-fh.in \n')

    if process == 'gg_H_2HDM' :
        if os.path.exists(powInputName) :
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/br.a3_2HDM . \n')
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/br.l3_2HDM . \n')
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/br.h3_2HDM . \n')
        else :
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/br.a3_2HDM \n')
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/br.h3_2HDM \n')
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/br.l3_2HDM \n')

    if process == 'VBF_HJJJ' :
        if os.path.exists(powInputName) :
            f.write('cp -p '+'/'.join(powInputName.split('/')[0:-1])+'/vbfnlo.input . \n')
        else :
            f.write('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+'/'.join(powInputName.split('/')[0:-1])+'/vbfnlo.input \n')

    m_ncall2 = 500000
    if process == 'ttH' :
        for line in open(inputName) :
            if 'ncall2' in line :
                m_ncall2 = line.split(" ")[2]
                print "The original ncall2 is :", m_ncall2

        f.write('sed -i "s/ncall2.*/ncall2 0/g" powheg.input \n')
        f.write('sed -i "s/fakevirt.*/fakevirt 1  ! number of calls for computing the integral and finding upper bound/g" powheg.input \n')

    f.write('./pwhg_main \n')

    if process == 'ttH' :
        f.write('sed -i "s/ncall2.*/ncall2 '+m_ncall2+'  ! number of calls for computing the integral and finding upper bound/g" powheg.input \n')
        f.write('sed -i "s/fakevirt.*/fakevirt 0/g" powheg.input \n')
        f.write('./pwhg_main \n')

    f.write('echo "\\nEnd of job on " `date` "\\n" \n')
    f.close()

    os.system('chmod 755 '+filename)

    #runCommand ('bsub -J ' + jobID + ' -u pippopluto -q ' + QUEUE + ' < ' + jobname, 1, TESTING == 0)

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
def runGetSource(parstage, xgrid, folderName, powInputName, process, noPdfCheck, tagName) :
    # parstage, xgrid are strings!

    print 'Getting and compiling POWHEG source...'

    #prepareJob(tagName, '', '.')

    filename = './run_'+tagName+'.sh'
    f = open (filename, 'a')

    f.write('export name='+folderName+'\n\n')
#    f.write('export process='+rootfolder+'\n\n')
    f.write('export cardInput='+powInputName+'\n\n')
    f.write('export process='+process+'\n\n')
    f.write('export noPdfCheck='+noPdfCheck+'\n\n')
    f.write('export WORKDIR='+os.getcwd()+'\n\n')
    f.write(
'''
# Release to be used to define the environment and the compiler needed
export RELEASE=${CMSSW_VERSION}
export jhugenversion="v7.1.4"

cd $WORKDIR
pwd

# initialize the CMS environment 

#if [ -e ${name} ]; then
#  echo -e "The directory ${name} exists! Move the directory to old_${name}"
#  echo
#  mv ${name} old_${name}
#  mv output.lhe old_output.lhe
#  rm -rf ${name}
#  echo -e "Move the tar ball to old_${tarball}.tar.gz"
#  echo
#  mv ${tarball}_tarball.tar.gz old_${tarball}_tarball.tar.gz
#fi


mkdir -p ${name} ; 

cd ${name}
export PATH=`pwd`:${PATH}

## Get the input card
#wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${cardinput} -O powheg.input  || cp -p ${cardinput} powheg.input || fail_exit "Failed to get powheg input card " ${card}

if [ -s ../${cardInput} ]; then
  cp -p ../${cardInput} powheg.input
fi

# 5F
is5FlavorScheme=1
defaultPDF=306000


if [[ "$process" == "ST_tch_4f" ]] || [[ "$process" == "bbH" ]] || [[ "$process" == "Wbb_dec" ]] || [[ "$process" == "Wbbj" ]]; then
    # 4F
    is5FlavorScheme=0
    defaultPDF=320900
fi

if [[ $is5FlavorScheme -eq 1 ]]; then
  echo "INFO: The process $process uses the 5F PDF scheme"
else
  echo "INFO: The process $process uses the 4F PDF scheme"
fi

cd $WORKDIR
python make_rwl.py ${is5FlavorScheme} ${defaultPDF}
cd ${name}

if [ -s ../JHUGen.input ]; then
  cp -p ../JHUGen.input JHUGen.input
fi

sed -i -e "s#--#-#g" powheg.input

myDir=`pwd`
card=${myDir}/powheg.input
cardj=${myDir}/JHUGen.input

## Try to get the JHUGen card
#wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${usejhugen} -O JHUGen.input || cp -p ${usejhugen} JHUGen.input

jhugen=0
if [[ -s ./JHUGen.input ]]; then
  jhugen=$(expr $jhugen + 1)
  echo "JHUGen activated!"
  #for decay weights in H->WW and H->ZZ
  wget --no-verbose https://github.com/cms-sw/genproductions/raw/7261679b0fd6464b80479c075b10f9ba1932ab9a/bin/JHUGen/Pdecay/PMWWdistribution.out 
  wget --no-verbose https://github.com/cms-sw/genproductions/raw/7261679b0fd6464b80479c075b10f9ba1932ab9a/bin/JHUGen/Pdecay/PMZZdistribution.out 

fi

### retrieve the powheg source tar ball
export POWHEGSRC=powhegboxV2_rev3511_date20180410.tar.gz

if [ "$process" = "b_bbar_4l" ] || [ "$process" = "HWJ_ew" ] || [ "$process" = "HW_ew" ] || [ "$process" = "HZJ_ew" ] || [ "$process" = "HZ_ew" ]; then 
  export POWHEGSRC=powhegboxRES_rev3478_date20180122.tar.gz 
fi

echo 'D/L POWHEG source...'

if [ ! -f ${POWHEGSRC} ]; then
  wget --no-verbose --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/powheg/V2.0/src/${POWHEGSRC} || fail_exit "Failed to get powheg tar ball "
fi
#cp -p ../${POWHEGSRC} .

tar zxf ${POWHEGSRC}
#
# increase maxseeds to 10000
sed -i -e "s#par_maxseeds=200,#par_maxseeds=10000,#g" POWHEG-BOX/include/pwhg_par.h

if [ -e POWHEG-BOX/${process}.tgz ]; then
  cd POWHEG-BOX/
  tar zxf ${process}.tgz
  cd -
fi

patch -l -p0 -i ${WORKDIR}/patches/pdfweights.patch
patch -l -p0 -i ${WORKDIR}/patches/pwhg_lhepdf.patch

if [ "$process" = "WZ" ] || [ "$process" = "ZZ" ]; then
   patch -l -p0 -i ${WORKDIR}/patches/lhapdf_zanderighi.patch
fi

if [ "$process" = "b_bbar_4l" ]; then
    cd POWHEG-BOX
    patch -l -p0 -i ${WORKDIR}/patches/res_openloops_long_install_dir.patch
    cd ..
fi
if [ "$process" = "ttb_NLO_dec" ]; then
    patch -l -p0 -i ${WORKDIR}/patches/pwhg_ttb_NLO_dec_gen_radiation_hook.patch
fi
if [ "$process" = "ZZ" ]; then
    patch -l -p0 -i ${WORKDIR}/patches/zz_m4lcut.patch
fi


sed -i -e "s#500#1200#g"  POWHEG-BOX/include/pwhg_rwl.h

#if [ "$process" = "HJ" ]; then 
#   sed -i -e "s#maxmulti=10#maxmulti=500#g" POWHEG-BOX/include/pwhg_bookhist-multi.h
#fi

echo ${POWHEGSRC} > VERSION

cd POWHEG-BOX/${process}

# This is just to please gcc 4.8.1
mkdir -p include

# Use dynamic linking and lhapdf
sed -i -e "s#STATIC[ \t]*=[ \t]*-static#STATIC=-dynamic#g" Makefile
sed -i -e "s#PDF[ \t]*=[ \t]*native#PDF=lhapdf#g" Makefile

# Use gfortran, not other compilers which are not free/licensed
sed -i -e "s#COMPILER[ \t]*=[ \t]*ifort#COMPILER=gfortran#g" Makefile

# Remove strange options in fortran (fixed line length, multiCPU compilation)
sed -i -e "s#132#none#g" Makefile
sed -i -e "s#make -j FC#make FC#g" Makefile

# hardcode svn info
sed -i -e 's#^pwhg_main:#$(shell ../svnversion/svnversion.sh>/dev/null) \
\
pwhg_main:#g' Makefile

echo "pwhg_main.o: svn.version" >> Makefile
echo "lhefwrite.o: svn.version" >> Makefile

# Find proper histo booking routine (many of them exist)
BOOK_HISTO="pwhg_bookhist-multi.o"
if [ `echo ${POWHEGSRC} | cut -d "_" -f 1` = "powhegboxV1" ]; then
   BOOK_HISTO="pwhg_bookhist.o"
fi 

if [ "$process" = "gg_H" ] || [ "$process" = "ggHH" ]; then
   BOOK_HISTO=""
   echo "Process using pwhg_bookhist-multi-new"
fi

if [ "$process" = "ggHH" ]; then
   sed -i -e "/PYTHIA8LOCATION/s|^|#|g" Makefile
   sed -i -e "/LIBPYTHIA8/s|^|#|g" Makefile
   sed -i -e "s|LIBHEPMC=|# LIBHEPMC=|g" Makefile
   sed -i -e "/main-PYTHIA8-lhef:/s|^|#|g" Makefile
   sed -i -e "s|LIBS+=-L:\\\$|LIBS+=-L\\\$|g" Makefile
fi

if [ "$process" = "trijet" ]; then 
   BOOK_HISTO+=" observables.o"
   rm -rf ../progress/bbinit.f
fi  

if [ "$process" = "VBF_HJJJ" ]; then 
  sed -i 's/..\/pwhg_book.h/pwhg_book.h/g' pwhg_analysis-dummy.f
fi  
if [ "$process" = "VBF_H" ]; then 
  sed -i '/pwhginihist/d' pwhg_analysis-dummy.f 
  patch -l -p0 -i ${WORKDIR}/patches/vbf_h_init_couplings.patch
fi  
if [ "$process" = "VBF_Z_Z" ]; then 
  patch -l -p0 -i ${WORKDIR}/patches/vbf_z_z_init_couplings.patch
fi  
if [ "$process" = "Wgamma" ] || [ "$process" = "W_ew-BMNNP" ]; then
    patch -l -p0 -i ${WORKDIR}/patches/pwhg_analysis_driver.patch 
fi
if [ "$process" = "HW_ew" ]; then
    patch -l -p0 -i ${WORKDIR}/patches/hwew.patch 
fi
#if [ "$process" = "ttb_NLO_dec" ]; then
#    patch -l -p0 -i ${WORKDIR}/patches/pwhg_analysis_driver_offshellmap.patch
#fi

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
if [ "$process" = "bbH" ]; then
   sed -i -e "s#O2#O0#g" Makefile
fi

# fix fortran options/linking to OpenLoops/missing libraries in VH_ew
if [ "$process" = "HW_ew" ] || [ "$process" = "HZ_ew" ] || [ "$process" = "HZJ_ew" ] || [ "$process" = "HWJ_ew" ] ; then
   sed -i -e "s#OL_process_src#OL_process_src f90_flags=-ffree-line-length-none#g" Makefile
   sed -i -e "s#\$(PWD)/\$(OBJ)#\$(OBJ)#g" Makefile
   sed -i -e "s#\$(OLPATH)/lib_src#lib_src#g" Makefile
   sed -i -e "s#cd \$(OLPATH)#cp -r \$(OLPATH)/* .#g" Makefile
   sed -i -e "s#abspath(os.path.join(config#relpath(os.path.join(config#g" ../OpenLoopsStuff/OpenLoops/SConstruct
   sed -i -e "s#rpath=\$(PWD)/\$(OBJDIR) -L\$(PWD)/\$(OBJDIR)#rpath=\$(OBJDIR) -L\$(OBJDIR)#g" Makefile
fi
if [ "$process" = "HWJ_ew" ] || [ "$process" = "HZJ_ew" ] ; then
   sed -i -e "s#boostrot.o#boostrot.o boostrot4.o#g" Makefile
fi

# 
if [ "$process" = "ttJ" ]; then
  sed -i -e "s#_PATH) -L#_PATH) #g" Makefile
  sed -i -e "s# -lvirtual#/libvirtual.so.1.0.0#g" Makefile
fi

# Use option O0 for ttH (O2 too long)
if [ "$process" = "ttH" ]; then
  sed -i 's/O2/O0/g' Makefile
  sed -i 's/4.5d0/4.75d0/g' init_couplings.f
fi

if [ "$process" = "gg_H_MSSM" ]; then 
  sed -i 's/leq/le/g' nloreal.F
  cp -p ../gg_H_quark-mass-effects/SLHA.h .
  cp -p ../gg_H_quark-mass-effects/SLHADefs.h .
fi
  
echo "ANALYSIS=none " >> tmpfile

if [ "$process" = "Wgamma" ]; then
  echo "PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o uti.o " >> tmpfile
#elif [ "$process" = "HW_ew" ] || [ "$process" = "HWJ_ew" ]; then 
#  echo "PWHGANAL=$BOOK_HISTO pwhg_analysis-HWnJ_res.o pwhg_analysis_paper-HWnJ_res.o observables.o multi_plot.o " >> tmpfile
else
  echo "PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o " >> tmpfile
fi
echo "LHAPDF_CONFIG=${LHAPDF_BASE}/bin/lhapdf-config" >> tmpfile
mv Makefile Makefile.interm
cat tmpfile Makefile.interm > Makefile
rm -f Makefile.interm tmpfile

# Add libraries
echo "LIBS+=-lz -lstdc++" >> Makefile

# Add extra packages
if [ $jhugen = 1 ]; then
  if [ ! -f JHUGenerator.${jhugenversion}.tar.gz ]; then
    wget --no-verbose --no-check-certificate http://spin.pha.jhu.edu/Generator/JHUGenerator.${jhugenversion}.tar.gz || fail_exit "Failed to get JHUGen tar ball "
  fi

  tar zxf JHUGenerator.${jhugenversion}.tar.gz
  cd JHUGenerator
  sed -i -e "s#Comp = ifort#Comp = gfort#g" makefile
  sed -i -e "s#linkMELA = Yes#linkMELA = No#g" makefile
  make

  mkdir -p ${WORKDIR}/${name}
  cp -p JHUGen ${WORKDIR}/${name}/.
  cp -pr pdfs ${WORKDIR}/${name}/.


  cd ..
fi
if [ "$process" = "gg_H_2HDM" ] || [ "$process" = "gg_H_MSSM" ]; then
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
  export LD_LIBRARY_PATH=`pwd`/lib/:`pwd`/lib64/:${LD_LIBRARY_PATH}

  if [ "$process" = "gg_H_MSSM" ]; then
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

echo 'Compiling pwhg_main...'
pwd
if [ "$process" = "HJ" ]; then
  echo "fixing q2min determination for HJ"
  # avoid accessing member 1 for q2min determination. Use member 0, as member 1 may not be available
  sed -i "s/getq2min(1,tmp)/getq2min(0,tmp)/g" setlocalscales.f
fi  

if [ "$process" = "ST_wtch_DR" ] || [ "$process" = "ST_wtch_DS" ]; then   
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


make pwhg_main || fail_exit "Failed to compile pwhg_main"

mkdir -p ${WORKDIR}/${name}
cp -p pwhg_main ${WORKDIR}/${name}/.

if [ -d ./lib ]; then
  cp -a ./lib ${WORKDIR}/${name}/.
fi 
if [ -d ./lib64 ]; then
  cp -a ./lib64 ${WORKDIR}/${name}/.
fi
if [ -d ./obj-gfortran/proclib ]; then
  mkdir ${WORKDIR}/${name}/obj-gfortran/
  cp -a ./obj-gfortran/proclib ${WORKDIR}/${name}/obj-gfortran/.
  cp -a ./obj-gfortran/*.so ${WORKDIR}/${name}/obj-gfortran/.
fi
if [ -d ./QCDLoop-1.9 ]; then                                 
  cp -a ./QCDLoop-1.9 ${WORKDIR}/${name}/.                    
  cp -a ./QCDLoop-1.9/ff/ff*.dat ${WORKDIR}/${name}/.      
fi

cd ${WORKDIR}/${name}

if [ "$process" = "HJ" ]; then
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
  cat makefile | sed -e "s#LHAPDFLIB=.\+#LHAPDFLIB=$(scram tool info lhapdf | grep LIBDIR | cut -d "=" -f2)#g" > makefile
  make || fail_exit "Failed to compile HNNLO"
  cp -p bin/hnnlo ${WORKDIR}/${name}/
  cp bin/br.* ${WORKDIR}/${name}/
  cd ${WORKDIR}/${name}
  cp POWHEG-BOX/HJ/NNLOPS-mass-effects/mergedata.f .
  gfortran -o mergedata mergedata.f

  cd ${WORKDIR}/${name}/POWHEG-BOX/HJ
 
  cp Makefile Makefile.orig
  cat Makefile.orig | sed -e "s#ANALYSIS=.\+#ANALYSIS=NNLOPS#g" |sed -e "s#\$(shell \$(LHAPDF_CONFIG) --libdir)#$(scram tool info lhapdf | grep LIBDIR | cut -d "=" -f2)#g" | sed -e "s#FASTJET_CONFIG=.\+#FASTJET_CONFIG=$(scram tool info fastjet | grep BASE | cut -d "=" -f2)/bin/fastjet-config#g" | sed -e "s#NNLOPSREWEIGHTER+=  fastjetfortran.o#NNLOPSREWEIGHTER+=  fastjetfortran.o pwhg_bookhist-multi.o#g" | sed -e "s#NNLOPSREWEIGHTERNRW+=  fastjetfortran.o#NNLOPSREWEIGHTERNRW+=  fastjetfortran.o pwhg_bookhist-multi.o#g" > Makefile
  make nnlopsreweighter-newrwgt || fail_exit "Failed to compile nnlopsreweighter"
  cp nnlopsreweighter-newrwgt ../../
  cd ${WORKDIR}/${name}
  HMASS=`cat powheg.input | grep "^hmass" | cut -d " " -f2`;
  BEAM=`cat powheg.input | grep "^ebeam1" | cut -d " " -f2 | tr "d" "."`;
  COMENERGY=`echo "( $BEAM*2 )" | bc`
  gawk "/sroot/{gsub(/8000/,$COMENERGY)};/hmass/{gsub(/125.5/, ${HMASS})};/mur,muf/{gsub(/62.750/, $(( $HMASS/2 )))};{print}" POWHEG-BOX/HJ/PaperRun/HNNLO-LHC8-R04-APX2-11.input | sed -e "s#10103#SEED#g" | sed -e "s#HNNLO-LHC8-R04-APX2-11#HNNLO-LHC13-R04-APX2-11#g"> HNNLO-LHC13-R04-APX2-11.input
  gawk "/sroot/{gsub(/8000/,$COMENERGY)};/hmass/{gsub(/125.5/, ${HMASS})};/mur,muf/{gsub(/62.750/, $(( $HMASS )))};{print}" POWHEG-BOX/HJ/PaperRun/HNNLO-LHC8-R04-APX2-11.input | sed -e "s#10103#SEED#g" | sed -e "s#HNNLO-LHC8-R04-APX2-11#HNNLO-LHC13-R04-APX2-22#g"> HNNLO-LHC13-R04-APX2-22.input
  gawk "/sroot/{gsub(/8000/,$COMENERGY)};/hmass/{gsub(/125.5/, ${HMASS})};/mur,muf/{gsub(/62.750/, $(( $HMASS/4 )))};{print}" POWHEG-BOX/HJ/PaperRun/HNNLO-LHC8-R04-APX2-11.input | sed -e "s#10103#SEED#g" | sed -e "s#HNNLO-LHC8-R04-APX2-11#HNNLO-LHC13-R04-APX2-0505#g"> HNNLO-LHC13-R04-APX2-0505.input
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

#mkdir -p workdir
#cd workdir
localDir=`pwd`

# Copy additional files
if [ -e ${WORKDIR}/vbfnlo.input ]; then
  cp -p ${WORKDIR}/vbfnlo.input .
fi 
if [ -e ${WORKDIR}/br.a3_2HDM ]; then
  cp -p ${WORKDIR}/br*2HDM .
fi
if [ -e ${WORKDIR}/powheg-fh.in ]; then
  cp -p ${WORKDIR}/powheg-fh.in .
fi
if [ -e ${WORKDIR}/cteq6m ]; then
    cp -p ${WORKDIR}/cteq6m .
fi 

#rm -f ${WORKDIR}/${name}/powheg.input
#cat ${card} | sed -e "s#SEED#${seed}#g" | sed -e "s#NEVENTS#${nevt}#g" > powheg.input

#cat powheg.input
#if [[ -e ../pwhg_main-gnu ]]; then
#  mv ../pwhg_main-gnu ../pwhg_main
#  chmod a+x ../pwhg_main
#fi

#make sure env variable for pdfsets points to the right place
#export LHAPDF_DATA_PATH=`${myDir}/lhapdf-config --datadir`
#../pwhg_main &> log_${process}_${seed}.txt

#cp -p ../runcms*.sh .

cd ${WORKDIR}

echo 'Compiling finished...'

if [ $jhugen = 1 ]; then
  cp -p ${cardj} .
  
  if [ ! -e ${WORKDIR}/runcmsgrid_powhegjhugen.sh ]; then
   fail_exit "Did not find " ${WORKDIR}/runcmsgrid_powhegjhugen.sh 
  fi
  sed -e 's/PROCESS/'${process}'/g' ${WORKDIR}/runcmsgrid_powhegjhugen.sh > runcmsgrid.sh
else
  if [ ! -e ${WORKDIR}/runcmsgrid_powheg.sh ]; then
   fail_exit "Did not find " ${WORKDIR}/runcmsgrid_powheg.sh 
  fi
  sed -e 's/PROCESS/'${process}'/g' ${WORKDIR}/runcmsgrid_powheg.sh > runcmsgrid.sh
fi

sed -i s/SCRAM_ARCH_VERSION_REPLACE/${SCRAM_ARCH}/g runcmsgrid.sh
sed -i s/CMSSW_VERSION_REPLACE/${CMSSW_VERSION}/g runcmsgrid.sh

chmod 755 runcmsgrid.sh

''')

    f.close()

    os.system('chmod 755 '+filename)

    #print "Source done..."
    #runCommand ('mv *.sh ' + folderName)

    #runCommand ('cp -p runcms*.sh ' + folderName + '/.')

    return

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def runEvents(parstage, folderName, EOSfolder, njobs, powInputName, jobtag, process) :
    print 'run : submitting jobs'
    #runCommand('rm -f ' + folderName + 'powheg.input')

    sedcommand = 'sed -i "s/parallelstage.*/parallelstage ' + parstage + '/ ; s/xgriditeration.*/xgriditeration 1/" '+folderName+'/powheg.input'

    runCommand(sedcommand)
    #runCommand('cp -p powheg.input ' + folderName)
    #runCommand('cp -p JHUGen.input ' + folderName)
    runCommand('cp -p ' + folderName + '/powheg.input ' + folderName + '/powheg.input.' + parstage)

    for i in range (1, njobs + 1) :
        tag = jobtag + '_' + str (i)
        # real run
        if parstage == '4' : jobname = prepareJobForEvents(tag, i, folderName, EOSfolder)
        else               : jobname = prepareJob(tag, i, folderName)
        jobID = jobtag + '_' + str (i)
        #runCommand ('bsub -J ' + jobID + ' -u pippopluto -q ' + QUEUE + ' < ' + jobname, 1, TESTING == 0)

        filename = folderName+'/run_' + tag + '.sh'
        f = open (filename, 'a')
        f.write('cd '+rootfolder+'/'+folderName+'/ \n')
        f.write('echo ' + str (i) + ' | ./pwhg_main &> run_' + tag + '.log ' + '\n')
        f.close()

        os.system('chmod 755 '+filename)

        if QUEUE == '':
            print 'Direct running... #'+str(i)+' \n'
            os.system('cd '+rootfolder+'/'+folderName+';bash run_'+tag+'.sh &')

        else:
            print 'Submitting to queue: '+QUEUE+' #'+str(i)+' \n'
            runCommand ('bsub -J ' + jobID + ' -u $USER -q ' + QUEUE + ' ' + rootfolder +'/'+folderName+'/run_'+tag+'.sh', TESTING == 0)

    #runCommand('mv *.sh ' + folderName)


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

def createTarBall(parstage, folderName, prcName, keepTop, seed, scriptName) :
    print 'Creating tarball distribution for '+args.folderName+'_'+prcName+'.tgz'
    print

    #inputName = folderName + "/powheg.input"

    filename = scriptName

    f = open(filename, 'a')
    #if filename = "" :
    #    f = subprocess.Popen(['/bin/sh', '-c'])

    f.write('export folderName='+folderName+'\n\n')
    f.write('export process='+prcName+'\n\n')
#    f.write('export cardInput='+powInputName+'\n\n')
    f.write('export keepTop='+keepTop+'\n\n')
    f.write('export WORKDIR='+os.getcwd()+'\n\n')
    f.write('export SEED='+seed+'\n\n')
    f.write(
'''

cd $WORKDIR/$folderName
echo "Processing folder: "
pwd

rm -f $WORKDIR/$folderName'_'$process'.tgz'

cp -p $WORKDIR/run_pwg.py $WORKDIR/$folderName

if [ -e $WORKDIR/$folderName/pwggrid-0001.dat ]; then
  cp -p $WORKDIR/$folderName/pwggrid-0001.dat $WORKDIR/$folderName/pwggrid.dat
  cp -p $WORKDIR/$folderName/pwg-0001-stat.dat $WORKDIR/$folderName/pwg-stat.dat
fi


FULLGRIDRM=`ls ${WORKDIR}/${folderName} | grep fullgrid-rm`
FULLGRIDBTL=`ls ${WORKDIR}/${folderName} | grep fullgrid-btl`
if [ ${#FULLGRIDRM} -gt 0 -a ${#FULLGRIDBTL} -gt 0 ]; then
  cp -p $WORKDIR/$folderName/${FULLGRIDRM} $WORKDIR/$folderName/pwgfullgrid-rm.dat
  cp -p $WORKDIR/$folderName/${FULLGRIDBTL} $WORKDIR/$folderName/pwgfullgrid-btl.dat
  cp -p $WORKDIR/$folderName/pwg-0001-st3-stat.dat $WORKDIR/$folderName/pwg-stat.dat
fi

grep -q "NEVENTS" powheg.input; test $? -eq 0 || sed -i "s/^numevts.*/numevts NEVENTS/g" powheg.input
grep -q "SEED" powheg.input; test $? -eq 0 || sed -i "s/^iseed.*/iseed SEED/g" powheg.input

grep -q "manyseeds" powheg.input; test $? -eq 0 || printf "\\n\\nmanyseeds 1\\n" >> powheg.input
grep -q "parallelstage" powheg.input; test $? -eq 0 || printf "\\nparallelstage 4\\n" >> powheg.input
grep -q "xgriditeration" powheg.input; test $? -eq 0 || printf "\\nxgriditeration 1\\n" >> powheg.input
  
# turn into single run mode
sed -i "s/^manyseeds.*/#manyseeds 1/g" powheg.input
sed -i "s/^parallelstage.*/#parallelstage 4/g" powheg.input
sed -i "s/^xgriditeration/#xgriditeration 1/g" powheg.input

# turn off obsolete stuff
grep -q "pdfreweight" powheg.input; test $? -eq 0 || printf "\\n\\npdfreweight 0\\n" >> powheg.input
grep -q "storeinfo_rwgt" powheg.input; test $? -eq 0 || printf "\\nstoreinfo_rwgt 0\\n" >> powheg.input
grep -q "withnegweights" powheg.input; test $? -eq 0 || printf "\\nwithnegweights 1\\n" >> powheg.input
  
sed -i "s/^pdfreweight.*/#pdfreweight 0/g" powheg.input
sed -i "s/^storeinfo_rwgt.*/#storeinfo_rwgt 0/g" powheg.input
sed -i "s/^withnegweights/#withnegweights 1/g" powheg.input

# parallel re-weighting calculation
if [ "$process" = "HW_ew" ] || [ "$process" = "HZ_ew" ] || [ "$process" = "HZJ_ew" ] || [ "$process" = "HWJ_ew" ] ; then
   echo "# no reweighting in first runx" >> powheg.input
else 
   echo "rwl_group_events 2000" >> powheg.input
   echo "lhapdf6maxsets 50" >> powheg.input
   echo "rwl_file 'pwg-rwl.dat'" >> powheg.input
   echo "rwl_format_rwgt 1" >> powheg.input
fi
cp -p $WORKDIR/pwg-rwl.dat pwg-rwl.dat

if [ -e ${WORKDIR}/$folderName/cteq6m ]; then
    cp -p ${WORKDIR}/cteq6m .
fi

if [ -s ${WORKDIR}/$folderName/JHUGen.input ]; then
    sed -e "s/PROCESS/${process}/g" ${WORKDIR}/runcmsgrid_powhegjhugen.sh > runcmsgrid.sh
else
    sed -e "s/PROCESS/${process}/g" ${WORKDIR}/runcmsgrid_powheg.sh > runcmsgrid.sh
fi

sed -i 's/pwggrid.dat ]]/pwggrid.dat ]] || [ -e ${WORKDIR}\/pwggrid-0001.dat ]/g' runcmsgrid.sh

#if [ "$process" = "HJ" ]; then
#  cat runcmsgrid.sh  | gawk '/produceWeightsNNLO/{gsub(/false/, \"true\")};{print}' > runcmsgrid_tmp.sh
#  mv runcmsgrid_tmp.sh runcmsgrid.sh
#fi  

sed -i s/SCRAM_ARCH_VERSION_REPLACE/${SCRAM_ARCH}/g runcmsgrid.sh
sed -i s/CMSSW_VERSION_REPLACE/${CMSSW_VERSION}/g runcmsgrid.sh

sed -i s/SCRAM_ARCH_VERSION_REPLACE/${SCRAM_ARCH}/g runcmsgrid.sh
sed -i s/CMSSW_VERSION_REPLACE/${CMSSW_VERSION}/g runcmsgrid.sh
chmod 755 runcmsgrid.sh
cp -p runcmsgrid.sh runcmsgrid_par.sh

sed -i '/ reweightlog_/c cat <<EOF | ../pwhg_main &>> reweightlog_${process}_${seed}.txt\\n${seed}\\npwgevents.lhe\\nEOF\\n' runcmsgrid_par.sh
sed -i 's/# Check if /sed -i \"s#.*manyseeds.*#manyseeds 1#g\" powheg.input\\n# Check if /g' runcmsgrid_par.sh
sed -i 's/# Check if /sed -i \"s#.*parallelstage.*#parallelstage 4#g\" powheg.input\\n# Check if /g' runcmsgrid_par.sh
sed -i 's/# Check if /sed -i \"s#.*xgriditeration.*#xgriditeration 1#g\" powheg.input\\n\\n# Check if /g' runcmsgrid_par.sh
sed -i 's/# Check if /rm -rf pwgseeds.dat; for ii in $(seq 1 9999); do echo $ii >> pwgseeds.dat; done\\n\\n# Check if /g' runcmsgrid_par.sh
sed -i 's/^..\/pwhg_main/echo \${seed} | ..\/pwhg_main/g' runcmsgrid_par.sh
sed -i 's/\.lhe/\${idx}.lhe/g' runcmsgrid_par.sh
sed -i 's/pwgevents.lhe/fornnlops/g' nnlopsreweighter.input
sed -i "s/^process/idx=-\`echo \${seed} | awk \'{printf \\"%04d\\", \$1}\'\` \\nprocess/g" runcmsgrid_par.sh

chmod 755 runcmsgrid_par.sh

#cd ${WORKDIR}

if [ "$process" = "HJ" ]; then
  echo "This process needs NNLOPS reweighting"
  for i in `echo 11 22 0505`; do
    ./mergedata 1 ${i}/*.top
    mv fort.12 HNNLO-${i}.top 
  done
  #force keep top in this case 
  keepTop='1'
fi

if [ $keepTop == '1' ]; then
    echo 'Keeping validation plots.'
    echo 'Packing...' ${WORKDIR}'/'${process}'_'${SCRAM_ARCH}'_'${CMSSW_VERSION}'_'${folderName}'.tgz'
    tar zcf ${WORKDIR}'/'${process}'_'${SCRAM_ARCH}'_'${CMSSW_VERSION}'_'${folderName}'.tgz' * --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.lhe --exclude=run_*.sh --exclude=*temp --exclude=pwgbtlupb-*.dat --exclude=pwgrmupb-*.dat
else
    echo 'Packing...' ${WORKDIR}'/'${process}'_'${SCRAM_ARCH}'_'${CMSSW_VERSION}'_'${folderName}'.tgz'
    tar zcf ${WORKDIR}'/'${process}'_'${SCRAM_ARCH}'_'${CMSSW_VERSION}'_'${folderName}'.tgz' * --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.top --exclude=*.lhe --exclude=run_*.sh --exclude=*temp --exclude=pwgbtlupb-*.dat --exclude=pwgrmupb-*.dat
fi

cd ${WORKDIR}

date
echo 'Done.'

''')

    f.close()

    os.system('chmod 755 '+filename)

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def runhnnlo(folderName, njobs, QUEUE):
  scales = ["11", "22", "0505"]
  for scale in scales:
    os.system('rm -rf '+ folderName+"/"+scale)
    os.system('mkdir -p '+ folderName+"/"+scale) 
    filename = folderName+"/"+scale+"/launch_NNLO.sh"
    launching_script = open(filename, "w")
    launching_script.write("#!/bin/bash\n")
    launching_script.write('base='+os.getcwd()+"/"+folderName+"/"+scale+'\n\n')
    launching_script.write(
'''
config=$1
seed=$2

cd $base
eval `scram runtime -sh`
cd -

cat $base/../$config | sed -e "s#SEED#$seed#g" > config.input
cat config.input | sed -e "s#MSTW2008nnlo68cl#NNPDF31_nnlo_hessian_pdfas#g" > config.input.temp
mv config.input.temp config.input

cp $base/../hnnlo .
cp $base/../br* .

./hnnlo < config.input &> log_${seed}.txt

cp HNNLO-LHC13* ${base}

cp log_${seed}.txt ${base}
''')
    launching_script.close()
    os.system('chmod 755 '+filename) 
    for ijob in range(njobs):
      config = "HNNLO-LHC13-R04-APX2-"+scale+".input" 
      jobID = scale+"_"+str(ijob)
      print 'Submitting to queue: '+QUEUE+' #'+str(ijob)+' \n'
      runCommand ('bsub -J ' + jobID + ' -u $USER -q ' + QUEUE + ' \"' + rootfolder + "/" + folderName + "/"+ scale + '/launch_NNLO.sh '+config+' '+str(1000+ijob)+'\"', 1, 1)
      
      



# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


if __name__ == "__main__":

    eoscmd = '/afs/cern.ch/project/eos/installation/cms/bin/eos.select' ;

#    folderName = 'test_prod'
#    parser.parstage      = sys.argv[1]
#    xgrid         = sys.argv[2]
#    folderName    = sys.argv[3] # grids folder
#    totEvents     = sys.argv[4]
#    inputTemplate = sys.argv[5] # FIXME build the template... it simply should be the cfg file
#    eosFolderName = sys.argv[6]
    
    # parser = argparse.ArgumentParser (description = 'run phantom productions on lxplus')
    parser = OptionParser()
    parser.add_option('-p', '--parstage'      , dest="parstage",      default= '0',            help='stage of the production process [0]')
    parser.add_option('-x', '--xgrid'         , dest="xgrid",         default= '1',            help='loop number for the girds production [1]')
    parser.add_option('-f', '--folderName'    , dest="folderName",    default='testProd',      help='local folder and last eos folder name[testProd]')
    parser.add_option('-e', '--eosFolder'     , dest="eosFolder",     default='NONE' ,         help='folder before the last one, on EOS')
    parser.add_option('-j', '--numJobs'       , dest="numJobs",       default= '10',           help='number of jobs to be used for multicore grid step 1,2,3')
    parser.add_option('-t', '--totEvents'     , dest="totEvents",     default= '10000',        help='total number of events to be generated [10000]')
    parser.add_option('-n', '--numEvents'     , dest="numEvents",     default= '2000',         help='number of events for a single job [2000]')
    parser.add_option('-i', '--inputTemplate' , dest="inputTemplate", default= 'powheg.input', help='input cfg file (fixed) [=powheg.input]')
    parser.add_option('-g', '--inputJHUGen' , dest="inputJHUGen", default= '', help='input JHUGen cfg file []')
    parser.add_option('-q', '--lsfQueue'      , dest="lsfQueue",      default= '',          help='LSF queue [2nd]')
    parser.add_option('-s', '--rndSeed'       , dest="rndSeed",       default= '42',           help='Starting random number seed [42]')
    parser.add_option('-m', '--prcName'       , dest="prcName",       default= 'DMGG',           help='POWHEG process name [DMGG]')
    parser.add_option('-k', '--keepTop'       , dest="keepTop",       default= '0',           help='Keep the validation top draw plots [0]')
    parser.add_option('-d', '--noPdfCheck'    , dest="noPdfCheck",    default= '0',           help='If 1, deactivate automatic PDF check [0]')

    # args = parser.parse_args ()
    (args, opts) = parser.parse_args(sys.argv)
    
    QUEUE = args.lsfQueue
    EOSfolder = args.folderName

    print
    print 'RUNNING PARAMS: parstage = ' + args.parstage + ' , xgrid = ' + args.xgrid  + ' , folderName = ' + args.folderName 
    print '                Total Events = ' + args.totEvents 
    print '                Number of Events = ' + args.numEvents 
    print '                powheg input cfg file : ' + args.inputTemplate 
    print '                powheg process name : ' + args.prcName
    print '                working folder : ' + args.folderName
    print '                EOS folder : ' + args.eosFolder + '/' + EOSfolder
    print '                base folder : ' + rootfolder
    print
 
    if (TESTING == 1) :     
        print '  --- TESTNG, NO submissions will happen ---  '
        print

    res = os.path.exists(rootfolder+'/'+args.folderName)

    if args.parstage == '1' and args.xgrid == '1' and (not res) :
        print 'Creating working folder ' + args.folderName + '...'
        # Assuming the generator binaries are in the current folder.
        os.system('mkdir '+rootfolder+'/'+args.folderName)
        if os.path.exists(rootfolder+'/pwhg_main') :
            print 'Copy pwhg_main'
            os.system('cp -p pwhg_main '+args.folderName+'/.')

        if os.path.exists(rootfolder+'JHUGen') :
            print 'Copy JHUGen'
            os.system('cp -p JHUGen '+args.folderName+'/.')

    if args.parstage == '1' and args.xgrid == '1' :
        if not os.path.exists(args.folderName) :
            print 'Creating working folder ' + args.folderName + '...'
            # Assuming the generator binaries are in the current folder.
            os.system('mkdir '+args.folderName)
            if os.path.exists('pwhg_main') :
                os.system('cp -p pwhg_main '+args.folderName+'/.')

            if os.path.exists('JHUGen') :
                os.system('cp -p JHUGen '+args.folderName+'/.')

        if not os.path.exists(args.inputTemplate) :
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate+' -O '+args.folderName+'/powheg.input')
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate)

            os.system('sed -i "s/^numevts.*/numevts '+args.numEvents+'/" '+
                      args.folderName+'/powheg.input')

        if not os.path.exists(args.folderName+'/powheg.input') :
            os.system('cp -p '+args.inputTemplate+' '+
                      args.folderName+'/powheg.input')
            os.system('sed -i "s/^numevts.*/numevts '+args.numEvents+'/" '+
                      args.folderName+'/powheg.input')

#        runCommand ('mkdir ' + args.folderName)
#        runCommand ('cp -p pwgseeds.dat ' + args.folderName)
        if not os.path.exists(args.folderName + '/pwgseeds.dat') :
            fseed = open(args.folderName + '/pwgseeds.dat', 'w')
            for ii in range(1, 10000) :
                fseed.write(str(ii)+'\n')
            fseed.close()
#        #FIXME this is a crude hardcoded trick to overcome some problems in LHAPDF usage
#        runCommand ('ln -s /afs/cern.ch/user/g/govoni/work/HiggsPlusJets/lhapdf/share/lhapdf/PDFsets/CT10.LHgrid ./'  + args.folderName)

    if args.parstage == '4' :    
        runCommand (eoscmd + ' mkdir /eos/cms/store/user/${user}/LHE/powheg/' + args.eosFolder, 1, 1)
        runCommand (eoscmd + ' mkdir /eos/cms/store/user/${user}/LHE/powheg/' + args.eosFolder + '/' + EOSfolder, 1, 1)

    njobs = int (args.numJobs)

    powInputName = args.inputTemplate
    #powInputName = args.inputTemplate + '_tempo'
    #sedcommand = 'sed "s/numevts.*/numevts ' + args.numEvents + '/" ' + args.inputTemplate + ' > ' + powInputName
    #runCommand (sedcommand)

    jobtag = args.parstage + '_' + args.xgrid

    if len(sys.argv) <= 1 :
        print "\t argument '-p', '--parstage'      , default= '0'"
        print "\t argument '-x', '--xgrid'         , default= '1'"
        print "\t argument '-f', '--folderName'    , default='testProd'"
        print "\t argument '-e', '--eosFolder'     , default='NONE'"
        print "\t argument '-t', '--totEvents'     , default= '10000"
        print "\t argument '-n', '--numEvents'     , default= '2000'"
        print "\t argument '-i', '--inputTemplate' , default= 'powheg.input'"
        print "\t argument '-g', '--inputJHUGen'   , default= ''"
        print "\t argument '-q', '--lsfQueue'      , default= ''"
        print "\t argument '-s', '--rndSeed'       , default= '42'"
        print "\t argument '-m', '--prcName'       , default= 'DMGG'"
        print "\t argument '-k', '--keepTop'       , default= '0'"
        print "\t argument '-d', '--noPdfCheck'    , default= '0'"
        print ""

        exit()

    if args.parstage == '0' or \
       args.parstage == '0123' or args.parstage == 'a' or \
       args.parstage == '01239' or args.parstage == 'one' or args.parstage == 'f' : # full single grid in oneshot 

        tagName = 'src_'+args.folderName
        filename = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.')

        if not os.path.exists(args.inputTemplate) :
            m_ret = os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate+' -O '+args.folderName+'/powheg.input')
#            print "return ", m_ret

            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate)
        
        os.system('mkdir -p '+rootfolder+'/'+args.folderName)
        if not os.path.exists(args.inputTemplate) :
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate+' -O '+args.folderName+'/powheg.input')
        else :
            os.system('cp -p '+args.inputTemplate+' '+args.folderName+'/powheg.input')

        os.system('rm -rf JHUGen.input')
        inputJHUGen = args.inputJHUGen
        if args.inputJHUGen == "":
            inputJHUGen = '/'.join(powInputName.split('/')[0:-1])+'/JHUGen.input'

        if not os.path.exists(inputJHUGen) :
            m_ret = os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+inputJHUGen+' -O '+args.folderName+'/JHUGen.input')
            if ((m_ret>>8) & 255) != 0 :
                os.system('rm -rf '+args.folderName+'/JHUGen.input')
#            print 'return value: ', ((m_ret>>8) & 255), m_ret

#            if os.path.exists('JHUGen.input') :
#                os.system('cp -p JHUGen.input '+args.folderName+'/.')
#            if os.path.exists(args.inputJHUGen) :
#                os.system('cp -p '+args.inputJHUGen+' '+args.folderName+'/JHUGen.input')
        else :
            os.system('cp -p '+inputJHUGen+' '+args.folderName+'/JHUGen.input')

        if os.path.exists(args.folderName+'/powheg.input') :
            #card_in = ConfigParser.ConfigParser()
            test_pdf1 = 0
            test_pdf2 = 0

            default_pdf = "306000"  # for 5 flavours

            if args.prcName=="ST_tch_4f" or args.prcName=="bbH" or args.prcName=="Wbb_dec" or args.prcName=="Wbbj" :
                default_pdf = "320900"  # for 4 flavours

            for line in open(args.folderName+'/powheg.input') :
                n_column = line.split()
                if 'lhans1' in line and len(n_column) >= 2:
                    test_pdf1 = n_column[1].strip()
                if 'lhans2' in line and len(n_column) >= 2:
                    test_pdf2 = n_column[1].strip()

            if not (test_pdf1 == test_pdf2) :
                raise RuntimeError("ERROR: PDF settings not equal for the 2 protons: {0} vs {1}... Please check your datacard".format(test_pdf1, test_pdf2))

            if test_pdf1 != default_pdf :
#                print "PDF in card: ", test_pdf1, "PDF default: ", default_pdf, test_pdf1==default_pdf
                message = "The input card does not have the standard 2017 PDF (NNPDF31 NNLO, 306000 for 5F, 320900 for 4F): {0}. Either change the card or run again with -d 1 to ignore this message.\n".format(test_pdf1)

                if args.noPdfCheck == '0' :
                    raise RuntimeError(message)
                else:
                    print "WARNING:", message
                    print "FORCING A DIFFERENT PDF SET FOR CENTRAL VALUE\n"

    if args.parstage == '0' :

        tagName = 'src_'+args.folderName
        filename = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.')

        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, args.noPdfCheck, tagName)

        if QUEUE == '':
            print 'Direct compiling... \n'
            #runCommand ('bash run_source.sh ', TESTING == 1)
            os.system('bash '+filename+' 2>&1 | tee '+filename.split('.sh')[0]+'.log')
            #print "Issue 'bash run_source.sh >& run.log &' to compile powheg..."
        
        else:
            print 'Submitting to queue: '+QUEUE+' \n'
            runCommand ('bsub -J compile_pwg -u $USER -q ' + QUEUE + ' '+rootfolder + '/' +filename, TESTING == 0)

    elif args.parstage == '1' :
        runParallelXgrid(args.parstage, args.xgrid, args.folderName,
                         args.numEvents, njobs, powInputName, jobtag,
                         args.rndSeed, args.prcName)

    elif args.parstage == '123' or args.parstage == 's' : # single grid proc
        tagName = 'grid_'+args.folderName
        scriptName = args.folderName + '/run_'+tagName+'.sh'


        os.system('cp -p '+args.inputTemplate+' '+args.folderName+'/powheg.input')
        os.system('sed -i "s/^numevts.*/numevts '+args.totEvents+'/" '+
                  args.folderName+'/powheg.input')

        prepareJob(tagName, '', args.folderName)
        runSingleXgrid(args.parstage, args.xgrid, args.folderName,
                       args.numEvents, powInputName, args.rndSeed,
                       args.prcName, scriptName)

        if QUEUE == '':
            print 'Direct running single grid... \n'
            #runCommand ('bash run_source.sh ', TESTING == 1)
            os.system('bash '+scriptName+' >& '+scriptName.split('.sh')[0]+'.log &')
            #print "Issue 'bash run_source.sh >& run.log &' to compile powheg..."
        
        else:
            print 'Submitting to queue: '+QUEUE+' \n'
            runCommand ('bsub -J '+args.folderName+' -u $USER -q ' + QUEUE + ' '+rootfolder+'/'+scriptName, TESTING == 0)

    elif args.parstage == '0123' or args.parstage == 'a' : # compile & run
        tagName = 'all_'+args.folderName
        scriptName = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.')
        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, args.noPdfCheck, tagName)

        os.system('sed -i "s/^numevts.*/numevts '+args.numEvents+'/" '+
                  args.folderName+'/powheg.input')
        runSingleXgrid(args.parstage, args.xgrid, args.folderName,
                       args.numEvents, powInputName, args.rndSeed,
                       args.prcName, scriptName)

        if QUEUE == '':
            print 'Direct compiling and running... \n'
            #runCommand ('bash run_source.sh ', TESTING == 1)
            os.system('bash '+scriptName+' >& '+
                      scriptName.split('.sh')[0]+'.log &')
        else:
            print 'Submitting to queue: '+QUEUE+' \n'
            runCommand ('bsub -J all_'+args.folderName+' -u $USER -q ' +
                        QUEUE + ' '+rootfolder + '/' +scriptName, TESTING == 0)

    elif args.parstage == '01239' or args.parstage == 'one' or args.parstage == 'f' : # full single grid in oneshot 
        tagName = 'full_'+args.folderName
        scriptName = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.')
        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, args.noPdfCheck, tagName)

        runSingleXgrid(args.parstage, args.xgrid, args.folderName,
                       args.numEvents, powInputName, args.rndSeed,
                       args.prcName, scriptName)

        createTarBall(args.parstage, args.folderName, args.prcName,
                      args.keepTop, args.rndSeed, scriptName)

        if QUEUE == '':
            print 'Direct running in one shot... \n'
            #runCommand ('bash run_source.sh ', TESTING == 1)
            os.system('bash '+scriptName+' >& '+
                      scriptName.split('.sh')[0]+'.log &')
        else:
            print 'Submitting to queue: '+QUEUE+' \n'
            runCommand ('bsub -J full_'+args.folderName+' -u $USER -q ' + 
                        QUEUE + ' '+rootfolder + '/' +scriptName, TESTING == 0)

    elif args.parstage == '7' :
      print "preparing for NNLO reweighting"
      runhnnlo(args.folderName, njobs, QUEUE)

    elif args.parstage == '9' :
        # overwriting with original
        scriptName = './run_tar_'+args.folderName+'.sh'

        os.system('rm -rf '+scriptName)

        createTarBall(args.parstage, args.folderName, args.prcName,
                      args.keepTop, args.rndSeed, scriptName)

        os.system('cd '+rootfolder+';bash '+scriptName)

    else                    :
        runEvents(args.parstage, args.folderName,
                  args.eosFolder + '/' + EOSfolder, njobs, powInputName,
                  jobtag, args.prcName)
