import os,sys

# my_path = '/tmp/'+os.environ['USER']+'/replace_gridpacks/'
my_path = './'


##########################################
########## START LOOP OVER FILES #########
##########################################
import hashlib
# for subdir, dirs, files in os.walk("/cvmfs/cms.cern.ch/phys_generator/gridpacks/2017/13TeV/madgraph/"):
# for subdir, dirs, files in os.walk("/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc6_amd64_gcc481/13TeV/madgraph/"):
    # for file in files:
        
        # if not 'madgraph' in subdir: continue
        # if not '.tar.xz' in sys.argv[0]: continue
        # if '_noiter.tar.xz' in sys.argv[0]: continue
        
gridpack_cvmfs_path = sys.argv[1]
prepid = hashlib.sha224(gridpack_cvmfs_path).hexdigest()
print 'gridpack_cvmfs_path',gridpack_cvmfs_path; sys.stdout.flush()
print 'prepid',prepid; sys.stdout.flush()
os.system('mkdir -p '+my_path+'/'+prepid)
os.chdir(my_path+'/'+prepid)
##########################################
########## END LOOP OVER FILES ###########
##########################################

gridpack_eos_path = gridpack_cvmfs_path.replace('/cvmfs/cms.cern.ch/phys_generator/gridpacks/','/eos/cms/store/group/phys_generator/cvmfs/gridpacks/')
gridpack_eos_path_noiter = gridpack_eos_path.replace('.tar.xz','_noiter.tar.xz')

# print gridpack_eos_path; sys.stdout.flush()
# print gridpack_eos_path_noiter; sys.stdout.flush()
# os.system('tar xf '+gridpack_cvmfs_path)
# os.system('echo '+gridpack_cvmfs_path)        

# ##############################################
# ############## START BACKUP ##################
# ##############################################
# print('cp -n '+gridpack_eos_path+' '+gridpack_eos_path_noiter); sys.stdout.flush()
# os.system('cp -n '+gridpack_eos_path+' '+gridpack_eos_path_noiter)
# os.chdir(my_path)
# os.system('rm -rf '+prepid)
# continue
# ##############################################
# ############### END BACKUP ###################
# ##############################################

##############################################
############## START CHECK IF PATCHED ########
##############################################
# print('checking '+gridpack_cvmfs_path); sys.stdout.flush()
# exit_code = os.popen('tar xf '+gridpack_cvmfs_path+' ./runcmsgrid.sh; echo $?').read()
# print 'exit_code',exit_code
# if exit_code != "0":
    # os.system('tar --extract --file='+gridpack_cvmfs_path+' ./runcmsgrid.sh; echo $?')
# continue
# run_sh = os.popen('grep run\.sh runcmsgrid.sh').read()
# if run_sh == "":
    # print 'NO run.sh in runcmsgrid.sh, skipping'; sys.stdout.flush()
    # continue
# grep_runcmsgrid = os.popen('grep FORCE\ IT\ TO\ PRODUCE\ EXACTLY\ THE\ REQUIRED\ NUMBER\ OF\ EVENTS runcmsgrid.sh -A71').read()
# if not "merge" in grep_runcmsgrid:
    # print 'runcmsgrid.sh APPARENTLY NOT PATCHED, PLEASE CHECK'
# else:
    # print 'runcmsgrid.sh PATCHED'
    # continue
# # print(grep_runcmsgrid); sys.stdout.flush()
# # else:
    # # with open("\/cvmfs\/cms.cern.ch\/phys_generator\/gridpacks\/mg_amg_patch\/runcmsgrid_backup_5k_mergeFix.sh", 'r') as f:
                # # content = f.read()
    # # diff_runcmsgrid = os.popen('grep FORCE\ IT\ TO\ PRODUCE\ EXACTLY\ THE\ REQUIRED\ NUMBER\ OF\ EVENTS runcmsgrid.sh -A71').read()
# # sys.exit(1)
# # continue
##############################################
########## END CHECK IF PATCHED ##############
##############################################

##############################################
############ START REPLACE ###################
##############################################        
print 'CHECKING IF PATCHED: untarring',gridpack_eos_path; sys.stdout.flush()
os.system('rm -rf '+my_path+'/'+prepid+'/*'); sys.stdout.flush()
os.system('tar xf '+gridpack_eos_path)
merge = os.popen('grep \/cvmfs\/cms.cern.ch\/phys_generator\/gridpacks\/lhe_merger\/merge.pl runcmsgrid.sh').read()
tmpdir = os.popen('grep _CONDOR_SCRATCH_DIR mgbasedir/Template/LO/SubProcesses/refine.sh').read()
tmpdir2 = os.popen('grep _CONDOR_SCRATCH_DIR process/madevent/SubProcesses/refine.sh').read()
if merge == "" or tmpdir == "" or tmpdir2 == "":
    if merge == "":
        print 'needs to replace the string in runcmsgrid.sh'; sys.stdout.flush()
    if tmpdir == "" and os.path.isfile("./mgbasedir/Template/LO/SubProcesses/refine.sh"):
        print 'needs to apply the tmpdir patch in mgbasedir/Template/LO/SubProcesses/refine.sh'; sys.stdout.flush()
    if tmpdir2 == "" and os.path.isfile("./process/madevent/SubProcesses/refine.sh"):
	    print 'needs to apply the tmpdir patch in process/madevent/SubProcesses/refine.sh'; sys.stdout.flush()
else:
    print 'no need to patch! removing noiter backup copy'
    os.system('rm '+gridpack_eos_path_noiter); sys.stdout.flush()
    sys.exit(1)

if not os.path.exists(gridpack_eos_path_noiter):
    print "ERROR: BACKUP GRIDPACK NOT EXISTING! COPYING"
    print('cp -n '+gridpack_eos_path+' '+gridpack_eos_path_noiter); sys.stdout.flush()
    os.system('cp -n '+gridpack_eos_path+' '+gridpack_eos_path_noiter)
    # continue
# else:
    # print "Backup gridpack",gridpack_eos_path_noiter,"already existing, i.e. should be patched!";
    # continue
if "_NLO_" in gridpack_eos_path or "_FXFX_" in gridpack_eos_path:
    print "gridpack seems to be NLO, skipping";
    sys.exit(0)
statinfo_noiter = os.stat(gridpack_eos_path_noiter)
# if statinfo_noiter.st_size <= 10485760:
    # print "ERROR: BACKUP CORRUPTED! SKIPPING"; sys.stdout.flush()
    # sys.exit(0)
# print('rm -rf '+my_path+'/'+prepid+'/*'); sys.stdout.flush()

# TO BE UNCOMMENTED AFTERWARDS
# os.system('ls -lrt '+gridpack_eos_path_noiter)
# print 'statinfo_noiter',statinfo_noiter; sys.stdout.flush()
# if statinfo_noiter.st_size <= 10485760:
    # print "ERROR: backup file size is less than 10MB, skipping\n"; sys.stdout.flush()
    # continue
# else:
    # print "OK: backup file size is greater than 10MB"; sys.stdout.flush()

# os.system('ls -lrt '+gridpack_eos_path)
# statinfo = os.stat(gridpack_eos_path)
# print 'statinfo',statinfo; sys.stdout.flush()

# if statinfo.st_mtime > statinfo_noiter.st_mtime and statinfo.st_size > statinfo_noiter.st_size*0.9:
    # if statinfo.st_size == statinfo_noiter.st_size:
        # print "FROM THE SIZE, GRIDPACK SEEMS TO BE NLO (i.e. no need to be patched)\n"; sys.stdout.flush()
    # else:
        # print "FROM THE SIZE, GRIDPACK SEEMS TO BE PATCHED (i.e. modified more recently than backup)\n"; sys.stdout.flush()
    # continue
    
print 'PATCHING: untarring',gridpack_eos_path_noiter; sys.stdout.flush()
os.system('rm -rf '+my_path+'/'+prepid+'/*'); sys.stdout.flush()
os.system('tar xf '+gridpack_eos_path_noiter)

  
merge = os.popen('grep \/cvmfs\/cms.cern.ch\/phys_generator\/gridpacks\/lhe_merger\/merge.pl runcmsgrid.sh').read()
tmpdir = os.popen('grep _CONDOR_SCRATCH_DIR mgbasedir/Template/LO/SubProcesses/refine.sh').read()
tmpdir2 = os.popen('grep _CONDOR_SCRATCH_DIR process/madevent/SubProcesses/refine.sh').read()
if merge == "" or tmpdir == "" or tmpdir2 == "":
    if merge == "":
        print 'replacing the string in runcmsgrid.sh'; sys.stdout.flush()
        os.system("sed -e '/\.\/run\.sh\ / {' -e 'r /cvmfs/cms.cern.ch/phys_generator/gridpacks/mg_amg_patch/runcmsgrid_backup_5k_mergeFix.sh' -e 'd' -e '}' -i runcmsgrid.sh")
    if tmpdir == "" and os.path.isfile("./mgbasedir/Template/LO/SubProcesses/refine.sh"):
        print 'applying the tmpdir patch in mgbasedir/Template/LO/SubProcesses/refine.sh'; sys.stdout.flush()
        os.system("cd mgbasedir; \
              wget https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/MadGraph5_aMCatNLO/patches/0018-propagate-madevent-exitcode-properly.patch; \
              patch -p1 < 0018-propagate-madevent-exitcode-properly.patch; \
              rm 0018-propagate-madevent-exitcode-properly.patch")
    if tmpdir2 == "" and os.path.isfile("./process/madevent/SubProcesses/refine.sh"):
	    print 'applying the tmpdir patch in process/madevent/SubProcesses/refine.sh'; sys.stdout.flush()
	    os.system("cp mgbasedir/Template/LO/SubProcesses/refine.sh process/madevent/SubProcesses/refine.sh")

        
    print 'tarring to gridpack.tar.xz for',prepid; sys.stdout.flush()
    os.system('tar cfJ gridpack.tar.xz ./')
    statinfo_new = os.stat('gridpack.tar.xz')
    print 'statinfo_new',statinfo_new; sys.stdout.flush()
    if statinfo_new.st_size > statinfo_noiter.st_size*0.9:
        print 'copying gridpack.tar.xz to',gridpack_eos_path; sys.stdout.flush()
        os.system('cp gridpack.tar.xz '+gridpack_eos_path)
        print 'DONE!'; sys.stdout.flush()
    else:
        print 'ERROR: from the size, gridpack.tar.xz seems corrupted, skipping'; sys.stdout.flush()
else:
    print "GRIDPACK SEEMS TO BE PATCHED (i.e. merge already present)\n"; sys.stdout.flush()
    sys.exit(0)
os.system('rm -rf '+my_path+'/'+prepid+'/*'); sys.stdout.flush()


##############################################
################# END REPLACE ################
##############################################

print '\n'; sys.stdout.flush()
# sys.exit(1)

# print os.popen('tar -tvf '+gridpack_cvmfs_path+' | grep runcmsgrid.sh |head -n1').read().split()[2] # Check for correct file size
# sys.stdout.flush()

# os.system('grep i- cmssw -B5 -A5 runcmsgrid.sh |more')
# os.system('ls -lrt '+gridpack_eos_path)
# os.system('ls -lrt '+gridpack_cvmfs_path)
# os.system('ls -lrt '+gridpack_eos_path_noiter)
# os.system('grep max_events_per_iteration '+my_path+'/'+prepid+'/runcmsgrid.sh')
# os.system('grep CMSSW '+my_path+'/'+prepid+'/runcmsgrid.sh')

      

      
