export folderName=$folderName
export process=$process
#export cardInput=$$powInputName
export keepTop=$keepTop
export WORKDIR=$rootfolder
export SEED=$seed

cd $$WORKDIR/$$folderName
echo "Processing folder: "
pwd

rm -f $$WORKDIR/$$folderName'_'$$process'.tgz'

if [ -e $$WORKDIR/$$folderName/pwg-0001-stat.dat ]; then
  cp -p $$WORKDIR/$$folderName/pwg-0001-stat.dat $$WORKDIR/$$folderName/pwg-stat.dat
fi

FULLGRIDRM=`ls *fullgrid-rm* | head -n 1`
FULLGRIDBTL=`ls *fullgrid-btl* | head -n 1`
UBOUND=`ls *ubound* | head -n 1`
PWGSTAT=`ls *st3-stat* | head -n 1`

if [ $${#FULLGRIDRM} -gt 0 -a $${#FULLGRIDBTL} -gt 0 ]; then
  cp -p $$WORKDIR/$$folderName/$${FULLGRIDRM} $$WORKDIR/$$folderName/pwgfullgrid-rm.dat
  cp -p $$WORKDIR/$$folderName/$${FULLGRIDBTL} $$WORKDIR/$$folderName/pwgfullgrid-btl.dat
fi
if [ $${#UBOUND} -gt 0 ]; then
  cp -p $$WORKDIR/$$folderName/$${UBOUND} $$WORKDIR/$$folderName/pwgubound.dat
fi
if [ $${#PWGSTAT} -gt 0 ]; then
  cp -p $$WORKDIR/$$folderName/$${PWGSTAT} $$WORKDIR/$$folderName/pwg-stat.dat
fi

sed -i "s/^numevts.*/numevts NEVENTS/g" powheg.input
sed -i "s/^iseed.*/iseed SEED/g" powheg.input
grep -q "withnegweights" powheg.input; test $$? -eq 0 || printf "\nwithnegweights 1\n" >> powheg.input

ISVRES="false"
grep -qi "powhegboxRES" $$WORKDIR/$$folderName/VERSION ; test $$? -ne 0  || ISVRES="true"
# For Powheg vRES, if the gridpack has been produced in several stages (using manyseeds 1),
# we need to turn on manyseeds & parallelstage also for the final gridpack.
# we test whether manyseeds has been used by checking if there is more than one "pwgubound-XXXX.dat" file exists
NUMUBOUND=`ls $${WORKDIR}/$${folderName} | egrep 'pwgubound-.+.dat' | wc -l`
if [ $$ISVRES == "true" -a $$NUMUBOUND -gt 1 ]; then
  echo "Detected Powheg Box RES and a parallel gridpack production. Turning on manyseeds in final gridpack"
  sed -i "s/^.*manyseeds.*/manyseeds 1/g" powheg.input
  sed -i "s/^.*parallelstage.*/parallelstage 4/g" powheg.input
  sed -i "s/^.*xgriditeration.*/xgriditeration 1/g" powheg.input
else
  # turn into single run mode
  sed -i "s/^manyseeds.*/#manyseeds 1/g" powheg.input
  sed -i "s/^parallelstage.*/#parallelstage 4/g" powheg.input
  sed -i "s/^xgriditeration.*/#xgriditeration 1/g" powheg.input
fi

# turn off obsolete stuff
sed -i "s/^pdfreweight.*/#pdfreweight 0/g" powheg.input
sed -i "s/^storeinfo_rwgt.*/#storeinfo_rwgt 0/g" powheg.input

printf "\npdfreweight 0\n" >> powheg.input
printf "storeinfo_rwgt 0\n" >> powheg.input
  
# parallel re-weighting calculation
if [ "$$process" = "HW_ew" ] || [ "$$process" = "HZ_ew" ] || [ "$$process" = "HZJ_ew" ] || [ "$$process" = "HWJ_ew" ] ; then
   echo "# no reweighting in first runx" >> powheg.input
else
   sed -i "s/^rwl_group_events.*/#rwl_group_events 2000/g" powheg.input
   sed -i "s/^lhapdf6maxsets.*/#lhapdf6maxsets 50/g" powheg.input
   sed -i "s/^rwl_file.*/#rwl_file '-'/g" powheg.input
   sed -i "s/^rwl_format_rwgt.*/#rwl_format_rwgt 1/g" powheg.input
   sed -i "s/^rwl_add.*/#rwl_add 0/g" powheg.input
   printf "\nrwl_group_events 2000\n" >> powheg.input
   echo "lhapdf6maxsets 50" >> powheg.input
   echo "rwl_file 'pwg-rwl.dat'" >> powheg.input
   echo "rwl_format_rwgt 1" >> powheg.input

fi

if [ -e $${WORKDIR}/$$folderName/cteq6m ]; then
    cp -p $${WORKDIR}/cteq6m .
fi

if [ -s $${WORKDIR}/$$folderName/JHUGen.input ]; then
    sed -e "s/PROCESS/$${process}/g" $${WORKDIR}/runcmsgrid_powhegjhugen.sh > runcmsgrid.sh
else
    sed -e "s/PROCESS/$${process}/g" $${WORKDIR}/runcmsgrid_powheg.sh > runcmsgrid.sh
fi

sed -i 's/pwggrid.dat ]]/pwggrid.dat ]] || [ -e $${WORKDIR}\/pwggrid-0001.dat ]/g' runcmsgrid.sh

sed -i s/SCRAM_ARCH_VERSION_REPLACE/$${SCRAM_ARCH}/g runcmsgrid.sh
sed -i s/CMSSW_VERSION_REPLACE/$${CMSSW_VERSION}/g runcmsgrid.sh

sed -i s/SCRAM_ARCH_VERSION_REPLACE/$${SCRAM_ARCH}/g runcmsgrid.sh
sed -i s/CMSSW_VERSION_REPLACE/$${CMSSW_VERSION}/g runcmsgrid.sh
chmod 755 runcmsgrid.sh
cp -p runcmsgrid.sh runcmsgrid_par.sh

sed -i '/ reweightlog_/c cat <<EOF | ../pwhg_main &>> reweightlog_$${process}_$${seed}.txt\n$${seed}\npwgevents.lhe\nEOF\n' runcmsgrid_par.sh
sed -i 's/# Check if /sed -i "s#.*manyseeds.*#manyseeds 1#g" powheg.input\n# Check if /g' runcmsgrid_par.sh
sed -i 's/# Check if /sed -i "s#.*parallelstage.*#parallelstage 4#g" powheg.input\n# Check if /g' runcmsgrid_par.sh
sed -i 's/# Check if /sed -i "s#.*xgriditeration.*#xgriditeration 1#g" powheg.input\n\n# Check if /g' runcmsgrid_par.sh
sed -i 's/# Check if /rm -rf pwgseeds.dat; for ii in $$(seq 1 9999); do echo $$ii >> pwgseeds.dat; done\n\n# Check if /g' runcmsgrid_par.sh
sed -i 's/^..\/pwhg_main/echo \$${seed} | ..\/pwhg_main/g' runcmsgrid_par.sh
sed -i 's/\.lhe/\$${idx}.lhe/g' runcmsgrid_par.sh
sed -i 's/pwgevents.lhe/fornnlops/g' nnlopsreweighter.input
sed -i "s/^process/idx=-\`echo \$${seed} | awk '{printf \"%04d\", \$$1}'\` \nprocess/g" runcmsgrid_par.sh

chmod 755 runcmsgrid_par.sh

#cd $${WORKDIR}

if [ "$$process" = "HJ" ]; then
  echo "This process needs NNLOPS reweighting"
  for i in `echo 11 22 0505`; do
    ./mergedata 1 $${i}/*.top
    mv fort.12 HNNLO-$${i}.top 
  done
  #force keep top in this case 
  keepTop='1'
fi

if [ "$$process" = "Zj" ] || [ "$$process" = "Wj" ]; then
  if [ -e $${WORKDIR}/$${folderName}/MINLO-W1-denom.top ]; then
    echo "This gridpack includes NNLOPS reweighting"
    #force keep top in this case
    keepTop='1'
    grep -q "nnlops" powheg.input; test $$? -eq 0 || echo "nnlops 1" >> powheg.input
  fi
fi

exclude_extra=""
if [ "$$process" = "X0jj" ] ; then
  # for X0jj we exclude some additional files to prevent the gridpacks becoming too large
  exclude_extra="--exclude=MG5_aMC*.tar.gz --exclude=pwgbtildeupb-*.dat  --exclude=pwgremnupb-*.dat --exclude=pwgcounters-st1-*.dat --exclude=pwgcounters-st2-*.dat --exclude=pwgcounters-st3-*.dat --exclude=pwg-*-stat.dat"
fi

if [ "$$process" = "WWJ" ] ; then
  echo "Adding MATRIXStuff libs to folderName"
  cd $${WORKDIR}/$${folderName}
  mkdir $${WORKDIR}/$${folderName}/MATRIXStuff/external
  cp -r $${WORKDIR}/$${folderName}/POWHEG-BOX/MATRIXStuff/external/*-install $${WORKDIR}/$${folderName}/MATRIXStuff/external/
  cp -r $${WORKDIR}/$${folderName}/POWHEG-BOX/MATRIXStuff/external/qqvvamp-1.1 $${WORKDIR}/$${folderName}/MATRIXStuff/external/
  cp -r $${WORKDIR}/$${folderName}/POWHEG-BOX/MATRIXStuff/lib $${WORKDIR}/$${folderName}/MATRIXStuff/
  cd -
fi


if [ $$keepTop == '1' ]; then
    echo 'Keeping validation plots.'
    echo 'Packing...' $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz'
    tar --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.lhe --exclude=run_*.sh --exclude=*temp --exclude=pwgbtlupb-*.dat --exclude=pwgrmupb-*.dat --exclude=run_*.out --exclude=run_*.err --exclude=run_*.log --exclude=minlo-run --exclude=dynnlo* $$exclude_extra -zcf $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz' *
else
  if [ $$process == "WWJ" ]; then
    echo 'Preparing WWJ gridpack'
    echo 'Packing...' $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz'
    tar --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.top --exclude=*.lhe --exclude=run_*.sh --exclude=*temp --exclude=pwgbtlupb-*.dat --exclude=pwgrmupb-*.dat --exclude=run_*.out --exclude=run_*.err --exclude=run_*.log --exclude=minlo-run --exclude=dynnlo* -zcf $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz' * 
    echo 'Removing copy of MATRIXStuff'
    rm -rf $${WORKDIR}/$${folderName}/MATRIXStuff/ 
  else
    echo 'Packing...' $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz'
    tar --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.top --exclude=*.lhe --exclude=run_*.sh --exclude=*temp --exclude=pwgbtlupb-*.dat --exclude=pwgrmupb-*.dat --exclude=run_*.out --exclude=run_*.err --exclude=run_*.log --exclude=minlo-run --exclude=dynnlo* $$exclude_extra -zcf $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz' *
  fi
fi

cd $${WORKDIR}

date
echo 'Done.'

