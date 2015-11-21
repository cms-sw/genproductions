#!/usr/bin/python

'''
Script for POWHEG generator production
By Yuan CHAO  03/30/2015
Based on Pietro's script
'''

import commands
import fileinput
import argparse
import sys
import os

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

    f.write('#!/bin/bash \n\n')

    f.write('echo "Start of job on " `date`\n\n')

    f.write('cd '+os.getcwd()+'\n\n')
#    f.write('pwd \n\n')

    f.write('source /cvmfs/cms.cern.ch/cmsset_default.sh\n\n')
    f.write('eval `scramv1 runtime -sh`\n\n')

    f.write('### Prepare environments for FastJet ### \n\n')

    f.write('export FASTJET_BASE=`scram tool info fastjet | grep FASTJET_BASE | sed -e s%FASTJET_BASE=%%`\n')
    f.write('export PATH=$FASTJET_BASE/bin/:$PATH \n')

    f.write('### Prepare environments for LHAPDF ### \n\n')

    f.write('export LHAPDF_BASE=`scram tool info lhapdf | grep LHAPDF_BASE | sed -e s%LHAPDF_BASE=%%`    \n')
    f.write('echo "LHAPDF_BASE is set to:" $LHAPDF_BASE \n')
    f.write('export PATH=$LHAPDF_BASE/bin/:$PATH \n')
#    f.write('export LHAPATH=`scram tool info lhapdf | grep LHAPATH | sed -e s%LHAPATH=%%`\n')
    f.write('export LHAPDF_DATA_PATH=`$LHAPDF_BASE/bin/lhapdf-config --datadir` \n')
#    f.write('export LHAPDF6TOOLFILE=$CMSSW_BASE/config/toolbox/$SCRAM_ARCH/tools/available/lhapdf6.xml \n\n')
#    f.write('cd ' + rootfolder + '/' + folderName + '\n')

    f.write('\n')

    f.close()
    return filename


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


def prepareJobForEvents (tag, i, folderName, EOSfolder) :
    runCommand('rm ' + rootfolder + '/' + folderName + '/log_' + tag + '.log')
    filename = 'run_' + tag + '.sh'

    prepareJob(tag, i, folderName)

    f = open (filename, 'a')
    f.write ('cp -p ' + rootfolder + '/' + folderName + '/powheg.input ./' + '\n')
    f.write ('cp -p ' + rootfolder + '/' + folderName + '/JHUGen.input ./' + '\n')
    f.write ('cp -p ' + rootfolder + '/' + folderName + '/*.dat  ./' + '\n')

    f.write ('cd -' + '\n')

    f.write ('pwd' + '\n')
    f.write ('ls' + '\n')
    f.write ('echo ' + str (i) + ' | ' + rootfolder + '/pwhg_main &> log_' + tag + '.log &' + '\n')
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
        f.write('cd '+rootfolder+'/'+folderName+'/ \n')
        f.write('echo ' + str (i) + ' | ./pwhg_main &> run_' + jobID + '.log &' + '\n')
        f.close()

        os.system('chmod 755 '+filename)

        #runCommand('bsub -J ' + jobID + ' -u pippopluto -q ' + QUEUE + ' < ' + jobname, 1, TESTING == 0)

        if QUEUE == '':
            print 'Direct running... #'+str(i)+' \n'
            #print 'cd '+rootfolder+'/'+folderName
            os.system('cd '+rootfolder+'/'+folderName+';bash run_'+jobID+'.sh &')
        else:
            print 'Submitting to queue: '+QUEUE+' #'+str(i)+' \n'
            runCommand ('bsub -J ' + jobID + ' -u $USER -q ' + QUEUE + ' '+rootfolder+'/run_'+jobID+'.sh ', TESTING == 0)


    #runCommand ('mv *.sh ' + folderName)


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
def runSingleXgrid(parstage, xgrid, folderName, nEvents, powInputName, seed, process, scriptName) :

    print 'Running single job for grid'
    #print folderName

    inputName = folderName + "/powheg.input"

#    runCommand('mv -f powheg.input powheg.input.temp')
#    sedcommand = 'sed "s/parallelstage.*/parallelstage ' + parstage + '/ ; s/xgriditeration.*/xgriditeration ' + xgrid + '/" ' + powInputName + ' > ' + folderName + '/powheg.input'
    sedcommand = 'sed "s/NEVENTS/' + nEvents + '/ ; s/SEED/' + seed + '/" ' + powInputName.split('/')[-1] + ' > ' + folderName + '/powheg.input'

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
def runGetSource(parstage, xgrid, folderName, powInputName, process, tagName) :
    # parstage, xgrid are strings!

    print 'Getting and compiling POWHEG source...'

    #prepareJob(tagName, '', '.')

    filename = './run_'+tagName+'.sh'
    f = open (filename, 'a')

    f.write('export name='+folderName+'\n\n')
#    f.write('export process='+rootfolder+'\n\n')
    f.write('export cardInput='+powInputName+'\n\n')
    f.write('export process='+process+'\n\n')
    f.write('export WORKDIR='+os.getcwd()+'\n\n')
    f.write(
'''
# Release to be used to define the environment and the compiler needed
export RELEASE=${CMSSW_VERSION}
export jhugenversion="v5.2.5"

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
#wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${cardinput} -O powheg.input  || cp -p ${cardinput} powheg.input || fail_exit "Failed to get powheg input card " ${card}

if [ -s ../${cardInput} ]; then
  cp -p ../${cardInput} powheg.input
fi

if [ -s ../JHUGen.input ]; then
  cp -p ../JHUGen.input JHUGen.input
fi

sed -i -e "s#--#-#g" powheg.input

myDir=`pwd`
card=${myDir}/powheg.input
cardj=${myDir}/JHUGen.input

## Try to get the JHUGen card
#wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/${usejhugen} -O JHUGen.input || cp -p ${usejhugen} JHUGen.input

jhugen=0
if [[ -s ./JHUGen.input ]]; then
  jhugen=$(expr $jhugen + 1)
  echo "JHUGen activated!"
fi

### retrieve the powheg source tar ball
export POWHEGSRC=powhegboxV2_Sep2015.tar.gz 

echo 'D/L POWHEG source...'

if [ ! -f ${POWHEGSRC} ]; then
  wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/powheg/V2.0/src/${POWHEGSRC} || fail_exit "Failed to get powheg tar ball "
fi

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
cd POWHEG-BOX/${process}

# This is just to please gcc 4.8.1
mkdir -p include

# Use dynamic linking and lhapdf
sed -i -e "s#STATIC[ \t]*=[ \t]*-static#STATIC=-dynamic#g" Makefile
sed -i -e "s#PDF[ \t]*=[ \t]*native#PDF=lhapdf#g" Makefile

# Use gfortran, not other compilers which are not free/licensed
sed -i -e "s#COMPILER[ \t]*=[ \t]*ifort#COMPILER=gfortran#g" Makefile

# Find proper histo booking routine (two of them exist)
BOOK_HISTO="pwhg_bookhist-multi.o"
if [ `echo ${name} | cut -d "_" -f 1` = "powhegboxV1" ]; then
   BOOK_HISTO="pwhg_bookhist.o"
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
if [ "$process" = "ttb_NLO_dec" ]; then
    patch -l -p0 -i ${WORKDIR}/patches/pwhg_analysis_driver_offshellmap.patch
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
if [ "$process" = "ttJ" ]; then
  sed -i -e "s#_PATH) -L#_PATH) #g" Makefile
  sed -i -e "s# -lvirtual#/libvirtual.so.1.0.0#g" Makefile
fi
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
else
  echo "PWHGANAL=$BOOK_HISTO pwhg_analysis-dummy.o " >> tmpfile
fi
echo "LHAPDF_CONFIG=${LHAPDF_BASE}/bin/lhapdf-config" >> tmpfile
mv Makefile Makefile.interm
cat tmpfile Makefile.interm > Makefile
rm -f Makefile.interm tmpfile

# Add libraries
echo "LIBS+=-lz -lstdc++" >> Makefile
if [ $jhugen = 1 ]; then
  if [ ! -f JHUGenerator.${jhugenversion}.tar.gz ]; then
    wget --no-check-certificate http://cms-project-generators.web.cern.ch/cms-project-generators/slc6_amd64_gcc481/JHUGenerator.${jhugenversion}.tar.gz || fail_exit "Failed to get JHUGen tar ball "
  fi

  tar zxf JHUGenerator.${jhugenversion}.tar.gz
  cd JHUGenerator
  sed -i -e "s#Comp = ifort#Comp = gfort#g" makefile
  make

  mkdir -p ${WORKDIR}/${name}
  cp -p JHUGen ${WORKDIR}/${name}/.
  cp -pr JHUGen/pdfs ${WORKDIR}/${name}/.

  cd ..
fi
if [ "$process" = "gg_H_2HDM" ] || [ "$process" = "gg_H_MSSM" ]; then
  echo "Adding CHAPLIN 1.2 library"
  if [ ! -f chaplin-1.2.tar ]; then
    wget http://chaplin.hepforge.org/code/chaplin-1.2.tar || fail_exit "Failed to get CHAPLIN tar ball "
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
      wget http://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/newversion/FeynHiggs-2.10.2.tar.gz || fail_exit "Failed to get FeynHiggs tar ball "
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

make pwhg_main || fail_exit "Failed to compile pwhg_main"

mkdir -p ${WORKDIR}/${name}
cp -p pwhg_main ${WORKDIR}/${name}/.

if [ -d ./lib ]; then
  cp -a ./lib ${WORKDIR}/${name}/.
fi 
if [ -d ./lib64 ]; then
  cp -a ./lib64 ${WORKDIR}/${name}/.
fi

cd ${WORKDIR}/${name}

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
        f.write('echo ' + str (i) + ' | ./pwhg_main &> run_' + tag + '.log &' + '\n')
        f.close()

        os.system('chmod 755 '+filename)

        if QUEUE == '':
            print 'Direct running... #'+str(i)+' \n'
            os.system('cd '+rootfolder+'/'+folderName+';bash run_'+tag+'.sh &')

        else:
            print 'Submitting to queue: '+QUEUE+' #'+str(i)+' \n'
            runCommand ('bsub -J ' + jobID + ' -u $USER -q ' + QUEUE + ' ' + rootfolder + '/run_'+tag+'.sh', TESTING == 0)

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

grep -q "NEVENTS" powheg.input; test $? -eq 0 || sed -i "s/^numevts.*/numevts NEVENTS/g" powheg.input
grep -q "SEED" powheg.input; test $? -eq 0 || sed -i "s/^iseed.*/iseed SEED/g" powheg.input

grep -q "manyseeds" powheg.input; test $? -eq 0 || printf "\\n\\nmanyseeds 1\\n" >> powheg.input
grep -q "parallelstage" powheg.input; test $? -eq 0 || printf "\\nparallelstage 4\\n" >> powheg.input
grep -q "xgriditeration" powheg.input; test $? -eq 0 || printf "\\nxgriditeration 1\\n" >> powheg.input

# turn into single run mode
sed -i "s/^manyseeds.*/#manyseeds 1/g" powheg.input
sed -i "s/^parallelstage.*/#parallelstage 4/g" powheg.input
sed -i "s/^xgriditeration/#xgriditeration 1/g" powheg.input

if [ -e ${WORKDIR}/$folderName/cteq6m ]; then
    cp -p ${WORKDIR}/cteq6m .
fi

if [ -e ${WORKDIR}/$folderName/JHUGen.input ]; then
    sed -e "s/PROCESS/${process}/g" ${WORKDIR}/runcmsgrid_powhegjhugen.sh > runcmsgrid.sh
else
    sed -e "s/PROCESS/${process}/g" ${WORKDIR}/runcmsgrid_powheg.sh > runcmsgrid.sh
fi

sed -i 's/pwggrid.dat ]]/pwggrid.dat ]] || [ -e ${WORKDIR}\/pwggrid-0001.dat ]/g' runcmsgrid.sh

chmod 755 runcmsgrid.sh

cp -p runcmsgrid.sh runcmsgrid_par.sh

sed -i '/ reweightlog_/c cat <<EOF | ../pwhg_main &>> reweightlog_${process}_${seed}.txt\\n${seed}\\npwgevents.lhe\\nEOF\\n' runcmsgrid_par.sh

sed -i 's/# Check if /sed -i \"s#.*manyseeds.*#manyseeds 1#g\" powheg.input\\n# Check if /g' runcmsgrid_par.sh
sed -i 's/# Check if /sed -i \"s#.*parallelstage.*#parallelstage 4#g\" powheg.input\\n# Check if /g' runcmsgrid_par.sh
sed -i 's/# Check if /sed -i \"s#.*xgriditeration.*#xgriditeration 1#g\" powheg.input\\n\\n# Check if /g' runcmsgrid_par.sh

sed -i 's/# Check if /rm -rf pwgseeds.dat; for ii in $(seq 1 9999); do echo $ii >> pwgseeds.dat; done\\n\\n# Check if /g' runcmsgrid_par.sh

sed -i 's/^..\/pwhg_main/echo \${seed} | ..\/pwhg_main/g' runcmsgrid_par.sh

sed -i 's/\.lhe/\${idx}.lhe/g' runcmsgrid_par.sh

sed -i "s/^process/idx=-\`echo \${seed} | awk \'{printf \\"%04d\\", \$1}\'\` \\nprocess/g" runcmsgrid_par.sh

chmod 755 runcmsgrid_par.sh

#cd ${WORKDIR}

if [ $keepTop == '1' ]; then
    echo 'Keeping validation plots.'
    echo 'Packing...' ${WORKDIR}'/'${folderName}'_'${process}'.tgz'
    tar zcf ${WORKDIR}'/'${folderName}'_'${process}'.tgz' * --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.lhe --exclude=run_*.sh --exclude=*.log --exclude=*temp
else
    echo 'Packing...' ${WORKDIR}'/'${folderName}'_'${process}'.tgz'
    tar zcf ${WORKDIR}'/'${folderName}'_'${process}'.tgz' * --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.top --exclude=*.lhe --exclude=run_*.sh --exclude=*.log --exclude=*temp
fi

cd ${WORKDIR}

date
echo 'Done.'

''')

    f.close()

    os.system('chmod 755 '+filename)

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
    
    parser = argparse.ArgumentParser (description = 'run phantom productions on lxplus')
    parser.add_argument('-p', '--parstage'      , default= '0',            help='stage of the production process [0]')
    parser.add_argument('-x', '--xgrid'         , default= '1',            help='loop number for the girds production [1]')
    parser.add_argument('-f', '--folderName'    , default='testProd',      help='local folder and last eos folder name[testProd]')
    parser.add_argument('-e', '--eosFolder'     , default='NONE' ,         help='folder before the last one, on EOS')
    parser.add_argument('-t', '--totEvents'     , default= '10000',        help='total number of events to be generated [10000]')
    parser.add_argument('-n', '--numEvents'     , default= '2000',         help='number of events for a single job [2000]')
    parser.add_argument('-i', '--inputTemplate' , default= 'powheg.input', help='input cfg file (fixed) [=powheg.input]')
    parser.add_argument('-q', '--lsfQueue'      , default= '',          help='LSF queue [2nd]')
    parser.add_argument('-s', '--rndSeed'       , default= '42',           help='Starting random number seed [42]')
    parser.add_argument('-m', '--prcName'       , default= 'DMGG',           help='POWHEG process name [DMGG]')
    parser.add_argument('-k', '--keepTop'       , default= '0',           help='Keep the validation top draw plots [0]')

    args = parser.parse_args ()
    
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
#        #FIXME this is a crude hardcoded trick to overcome some problems in LHAPDF usage
#        runCommand ('ln -s /afs/cern.ch/user/g/govoni/work/HiggsPlusJets/lhapdf/share/lhapdf/PDFsets/CT10.LHgrid ./'  + args.folderName)

    if args.parstage == '4' :    
        runCommand (eoscmd + ' mkdir /eos/cms/store/user/${user}/LHE/powheg/' + args.eosFolder, 1, 1)
        runCommand (eoscmd + ' mkdir /eos/cms/store/user/${user}/LHE/powheg/' + args.eosFolder + '/' + EOSfolder, 1, 1)

    njobs = int (args.totEvents) / int (args.numEvents)

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
        print "\t argument '-q', '--lsfQueue'      , default= ''"
        print "\t argument '-s', '--rndSeed'       , default= '42'"
        print "\t argument '-m', '--prcName'       , default= 'DMGG'"
        print "\t argument '-k', '--keepTop'       , default= '0'"
        print ""

        exit()

    if args.parstage == '0' :
        #runCommand('cp -p JHUGen.input '+args.folderName+'/.')

        tagName = 'src_'+args.folderName
        filename = './run_'+tagName+'.sh'

        prepareJob(tagName, '', '.')

        if not os.path.exists(args.inputTemplate) :
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate)
        os.system('mkdir -p '+rootfolder+'/'+args.folderName)
        os.system('cp -p '+args.inputTemplate.split('/')[-1]+' '+args.folderName+'/powheg.input')

        os.system('rm -rf JHUGen.input')
        inputJHUGen = '/'.join(powInputName.split('/')[0:-1])+'/JHUGen.input'
        if not os.path.exists(inputJHUGen) :
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+inputJHUGen)
            if os.path.exists('JHUGen.input') :
                os.system('cp -p JHUGen.input '+args.folderName+'/.')
        else :
            os.system('cp -p '+inputJHUGen+' '+args.folderName+'/.')

        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, tagName)

        if QUEUE == '':
            print 'Direct compiling... \n'
            #runCommand ('bash run_source.sh ', TESTING == 1)
            os.system('bash '+filename+' >& '+filename.split('.sh')[0]+'.log &')
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


        os.system('cp -p '+args.inputTemplate.split('/')[-1]+' '+args.folderName+'/powheg.input')
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

        if not os.path.exists(args.inputTemplate) :
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate)
        os.system('mkdir -p '+rootfolder+'/'+args.folderName)
        os.system('cp -p '+args.inputTemplate.split('/')[-1]+' '+args.folderName+'/powheg.input')
        prepareJob(tagName, '', '.')
        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, tagName)

        os.system('cp -p '+args.inputTemplate.split('/')[-1]+' '+
                  args.folderName+'/powheg.input')
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

        if not os.path.exists(args.inputTemplate) :
            os.system('wget --quiet --no-check-certificate -N http://cms-project-generators.web.cern.ch/cms-project-generators/'+args.inputTemplate)
        os.system('mkdir -p '+rootfolder+'/'+args.folderName)
        os.system('cp -p '+args.inputTemplate.split('/')[-1]+' '+args.folderName+'/powheg.input')
        prepareJob(tagName, '', '.')
        runGetSource(args.parstage, args.xgrid, args.folderName,
                     powInputName, args.prcName, tagName)

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
