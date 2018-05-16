import os,sys

my_path = '/tmp/'+os.environ['USER']+'/replace_gridpacks/'

requests = [
            # "HIG-RunIIFall17wmLHEGS-01193",
            # "HIG-RunIIFall17wmLHEGS-01216",
            # "HIG-RunIIFall17wmLHEGS-01217",
            # "HIG-RunIIFall17wmLHEGS-01218",
            # "HIG-RunIIFall17wmLHEGS-01219",
            # "HIG-RunIIFall17wmLHEGS-01220",
            # "HIG-RunIIFall17wmLHEGS-01221",
            # "HIG-RunIIFall17wmLHEGS-01222",
            # "HIG-RunIIFall17wmLHEGS-01223",
            # "HIG-RunIIFall17wmLHEGS-01224",
            # "HIG-RunIIFall17wmLHEGS-01225",
            # "HIG-RunIIFall17wmLHEGS-01226",
            # "HIG-RunIIFall17wmLHEGS-01227",
            # "HIG-RunIIFall17wmLHEGS-01228",
            # "HIG-RunIIFall17wmLHEGS-01229",
            # "HIG-RunIIFall17wmLHEGS-01230",
            # "HIG-RunIIFall17wmLHEGS-01231",
            # "HIG-RunIIFall17wmLHEGS-01232",
            # "HIG-RunIIFall17wmLHEGS-01233",
            # "HIG-RunIIFall17wmLHEGS-01234",
            # "HIG-RunIIFall17wmLHEGS-01235",
            # "HIG-RunIIFall17wmLHEGS-01236",
            # "HIG-RunIIFall17wmLHEGS-01237",
            # "HIG-RunIIFall17wmLHEGS-01238",
            "HIG-RunIIFall17wmLHEGS-01673",
            "HIG-RunIIFall17wmLHEGS-01674",
            "HIG-RunIIFall17wmLHEGS-01675",
            "HIG-RunIIFall17wmLHEGS-01676",
            "HIG-RunIIFall17wmLHEGS-01677",
            "HIG-RunIIFall17wmLHEGS-01678",
            "HIG-RunIIFall17wmLHEGS-01679",
            "HIG-RunIIFall17wmLHEGS-01681",
            "HIG-RunIIFall17wmLHEGS-01682",
           ]


# ##########################################
# ######## START LOOP OVER PREPIDS #########
# ##########################################
# for prepid in requests:

        # os.system('echo '+prepid)
        
        # os.system('mkdir -p '+my_path+'/'+prepid)
        # os.chdir(my_path+'/'+prepid)
        # os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+prepid+' -O '+prepid)
        # gridpack_cvmfs_path = os.popen('grep \/cvmfs '+prepid).read()
        # gridpack_cvmfs_path = gridpack_cvmfs_path.replace('\"',"\'").split('\'')[1]
        # os.system('rm '+prepid)
# ##########################################
# ######## END LOOP OVER PREPIDS ###########
# ##########################################

##########################################
########## START LOOP OVER FILES #########
##########################################
import hashlib
# for subdir, dirs, files in os.walk("/cvmfs/cms.cern.ch/phys_generator/gridpacks/2017/13TeV/madgraph/"):
for subdir, dirs, files in os.walk("/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc6_amd64_gcc481/14TeV/madgraph/V5_2.6.0/"):
    for file in files:
        
        if not 'madgraph' in subdir: continue
        if not '.tar.xz' in file: continue
        if '_noiter.tar.xz' in file: continue
        
        gridpack_cvmfs_path = os.path.join(subdir, file)
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
                
        ##############################################
        ############ START REPLACE ###################
        ##############################################        
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
            continue
        statinfo_noiter = os.stat(gridpack_eos_path_noiter)
        if statinfo_noiter.st_size <= 10485760:
            print "ERROR: BACKUP CORRUPTED! SKIPPING"; sys.stdout.flush()
            continue
        # print('rm -rf '+my_path+'/'+prepid+'/*'); sys.stdout.flush()
                
        print 'PATCHING: untarring',gridpack_eos_path_noiter; sys.stdout.flush()
        os.system('rm -rf '+my_path+'/'+prepid+'/*'); sys.stdout.flush()
        os.system('tar xf '+gridpack_eos_path_noiter)
        
          
        merge = os.popen('grep mkdir\ process\/madevent\/Events\/\$\{runlabel\} runcmsgrid.sh').read()
        # tmpdir = os.popen('grep _CONDOR_SCRATCH_DIR mgbasedir/Template/LO/SubProcesses/refine.sh').read()
        if merge == "":
            if merge == "":
                print 'replacing the string in runcmsgrid.sh'; sys.stdout.flush()
                os.system("patch < /eos/cms/store/group/phys_generator/cvmfs/gridpacks/mg_amg_patch/runcmsgrid_fix26x_mkdir.patch")

                
            print 'tarring to gridpack.tar.xz for',prepid; sys.stdout.flush()
            os.system('tar cfJ gridpack.tar.xz ./*')
            statinfo_new = os.stat('gridpack.tar.xz')
            print 'statinfo_new',statinfo_new; sys.stdout.flush()
            if statinfo_new.st_size > statinfo_noiter.st_size*0.9:
                print 'copying gridpack.tar.xz to',gridpack_eos_path; sys.stdout.flush()
                os.system('rm '+gridpack_eos_path+'; cp gridpack.tar.xz '+gridpack_eos_path)
            else:
                print 'ERROR: from the size, gridpack.tar.xz seems corrupted, skipping'; sys.stdout.flush()
        else:
            print "GRIDPACK SEEMS TO BE PATCHED (i.e. merge already present)\n"; sys.stdout.flush()
            continue
        # os.system('rm -rf '+my_path+'/'+prepid+'/*'); sys.stdout.flush()


        ##############################################
        ################# END REPLACE ################
        ##############################################
        
        print '\n'; sys.stdout.flush()
              

              
