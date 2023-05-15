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
if [ -e $$WORKDIR/$$folderName/pwg-st3-0001-stat.dat ]; then
  cp -p $$WORKDIR/$$folderName/pwg-st3-0001-stat.dat $$WORKDIR/$$folderName/pwg-stat.dat
fi

FULLGRIDRM=`ls $${WORKDIR}/$${folderName} | grep fullgrid-rm | head -n 1`
FULLGRIDBTL=`ls $${WORKDIR}/$${folderName} | grep fullgrid-btl | head -n 1`
FULLGRIDRM=$${FULLGRIDRM%?}
FULLGRIDBTL=$${FULLGRIDBTL%?}

if [ $${#FULLGRIDRM} -gt 0 -a $${#FULLGRIDBTL} -gt 0 ]; then
  cp -p $$WORKDIR/$$folderName/$${FULLGRIDRM} $$WORKDIR/$$folderName/pwgfullgrid-rm.dat
  cp -p $$WORKDIR/$$folderName/$${FULLGRIDBTL} $$WORKDIR/$$folderName/pwgfullgrid-btl.dat
  cp -p $$WORKDIR/$$folderName/pwg-0001-st3-stat.dat $$WORKDIR/$$folderName/pwg-stat.dat
fi

grep -q "NEVENTS" powheg.input; test $$? -eq 0 || sed -i "s/^numevts.*/numevts NEVENTS/g" powheg.input
grep -q "SEED" powheg.input; test $$? -eq 0 || sed -i "s/^iseed.*/iseed SEED/g" powheg.input

grep -q "manyseeds" powheg.input; test $$? -eq 0 || printf "\n\nmanyseeds 1\n" >> powheg.input
grep -q "parallelstage" powheg.input; test $$? -eq 0 || printf "\nparallelstage 4\n" >> powheg.input
grep -q "xgriditeration" powheg.input; test $$? -eq 0 || printf "\nxgriditeration 1\n" >> powheg.input
  
# turn into single run mode
sed -i "s/^manyseeds.*/#manyseeds 1/g" powheg.input
sed -i "s/^parallelstage.*/#parallelstage 4/g" powheg.input
sed -i "s/^xgriditeration/#xgriditeration 1/g" powheg.input

# turn off obsolete stuff
grep -q "pdfreweight" powheg.input; test $$? -eq 0 || printf "\n\npdfreweight 0\n" >> powheg.input
grep -q "storeinfo_rwgt" powheg.input; test $$? -eq 0 || printf "\nstoreinfo_rwgt 0\n" >> powheg.input
grep -q "withnegweights" powheg.input; test $$? -eq 0 || printf "\nwithnegweights 1\n" >> powheg.input
  
sed -i "s/^pdfreweight.*/#pdfreweight 0/g" powheg.input
sed -i "s/^storeinfo_rwgt.*/#storeinfo_rwgt 0/g" powheg.input
sed -i "s/^withnegweights/#withnegweights 1/g" powheg.input

# parallel re-weighting calculation
if [ "$$process" = "HW_ew" ] || [ "$$process" = "HZ_ew" ] || [ "$$process" = "HZJ_ew" ] || [ "$$process" = "HWJ_ew" ] ; then
   echo "# no reweighting in first runx" >> powheg.input
else
   grep -q "rwl_group_events" powheg.input; test $$? -eq 0 || echo "rwl_group_events 2000" >> powheg.input
   grep -q "lhapdf6maxsets" powheg.input; test $$? -eq 0 || echo "lhapdf6maxsets 50" >> powheg.input
   grep -q "rwl_file" powheg.input; test $$? -eq 0 || echo "rwl_file 'pwg-rwl.dat'" >> powheg.input
   grep -q "rwl_format_rwgt" powheg.input; test $$? -eq 0 || echo "rwl_format_rwgt 1" >> powheg.input
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

if [ "$$process" = "WWJ" ]; then
   cp -pr $${WORKDIR}/$${folderName}/POWHEG-BOX/WWJ/TWOLOOP_GRIDS_reg1 .
   cp -pr $${WORKDIR}/$${folderName}/POWHEG-BOX/WWJ/TWOLOOP_GRIDS_reg2 .
   cp -pr $${WORKDIR}/$${folderName}/POWHEG-BOX/WWJ/TWOLOOP_GRIDS_reg3 .
   cp -pr $${WORKDIR}/$${folderName}/POWHEG-BOX/WWJ/TWOLOOP_GRIDS_reg4 .
   #force keep top = 0 in this case 
   keepTop='0'
fi  

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


if [ $$keepTop == '1' ]; then
    echo 'Keeping validation plots.'
    echo 'Packing...' $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz'
    tar zcf $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz' * --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.lhe --exclude=run_*.sh --exclude=*temp --exclude=pwgbtlupb-*.dat --exclude=pwgrmupb-*.dat --exclude=run_*.out --exclude=run_*.err --exclude=run_*.log --exclude=minlo-run --exclude=dynnlo* $$exclude_extra
else
  if [ $$process == 'WWJ' ]; then
    echo 'Preparing WWJ gridpack'
    echo 'Packing...' $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tar.xz'
    tar -cJpsf $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tar.xz' * --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.top --exclude=*.lhe --exclude=run_*.sh --exclude=*temp --exclude=pwgbtlupb-*.dat --exclude=pwgrmupb-*.dat --exclude=run_*.out --exclude=run_*.err --exclude=run_*.log --exclude=minlo-run --exclude=dynnlo*
  else
    echo 'Packing...' $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz'
    tar zcf $${WORKDIR}'/'$${process}'_'$${SCRAM_ARCH}'_'$${CMSSW_VERSION}'_'$${folderName}'.tgz' * --exclude=POWHEG-BOX --exclude=powhegbox*.tar.gz --exclude=*.top --exclude=*.lhe --exclude=run_*.sh --exclude=*temp --exclude=pwgbtlupb-*.dat --exclude=pwgrmupb-*.dat --exclude=run_*.out --exclude=run_*.err --exclude=run_*.log --exclude=minlo-run --exclude=dynnlo* $$exclude_extra
  fi
fi

cd $${WORKDIR}

date
echo 'Done.'

