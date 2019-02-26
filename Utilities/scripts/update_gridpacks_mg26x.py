import os,sys
import hashlib

my_path = '/tmp/'+os.environ['USER']+'/replace_gridpacks/'

requests=[]
requests = [
	    "HIG-RunIIFall17wmLHEGS-01813",
	    "HIG-RunIIFall17wmLHEGS-01814",
	    "HIG-RunIIFall17wmLHEGS-01816",
	    "HIG-RunIIFall17wmLHEGS-01817",
	    "HIG-RunIIFall17wmLHEGS-01818",
	    "HIG-RunIIFall17wmLHEGS-01819",
	    "HIG-RunIIFall17wmLHEGS-01820",
	    "HIG-RunIIFall17wmLHEGS-01821",
	    "HIG-RunIIFall17wmLHEGS-01822",
	    "HIG-RunIIFall17wmLHEGS-01823",
	    "HIG-RunIIFall17wmLHEGS-01826",
	    "HIG-RunIIFall17wmLHEGS-01836",
	    "HIG-RunIIFall17wmLHEGS-01837",
	    "HIG-RunIIFall17wmLHEGS-01838",
	    "HIG-RunIIFall17wmLHEGS-01841",
	    "HIG-RunIIFall17wmLHEGS-01843",
	    "HIG-RunIIFall17wmLHEGS-01845",
	    "HIG-RunIIFall17wmLHEGS-01846",
           ]

#for num in range(0,60):
#	requests.append('EXO-RunIISummer15wmLHEGS-0'+str(num+6229))
print requests

for prepid in requests:

        os.system('echo '+prepid)
        
        os.system('mkdir -p '+my_path+'/'+prepid)
        os.chdir(my_path+'/'+prepid)
        os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+prepid+' -O '+prepid)
        gridpack_cvmfs_path = os.popen('grep \/cvmfs '+prepid+'| grep -v \'#args\' ').read()
        gridpack_cvmfs_path = gridpack_cvmfs_path.split('\'')[1]
	gridpack_eos_path = gridpack_cvmfs_path.replace("/cvmfs/cms.cern.ch/phys_generator","/eos/cms/store/group/phys_generator/cvmfs")

        print gridpack_cvmfs_path
	if not 'madgraph' in gridpack_cvmfs_path: continue
	if not '.tar.xz' in gridpack_cvmfs_path: continue
	if '_noiter.tar.xz' in gridpack_cvmfs_path: continue
        
	prepid = hashlib.sha224(gridpack_cvmfs_path).hexdigest()
        print 'gridpack_cvmfs_path',gridpack_cvmfs_path; sys.stdout.flush()
        print 'prepid',prepid; sys.stdout.flush()
        os.system('mkdir -p '+my_path+'/'+prepid)
        os.chdir(my_path+'/'+prepid)

        gridpack_eos_path = gridpack_cvmfs_path.replace('/cvmfs/cms.cern.ch/phys_generator/gridpacks/','/eos/cms/store/group/phys_generator/cvmfs/gridpacks/')
        gridpack_eos_path_noiter = gridpack_eos_path.replace('.tar.xz','_noiter_TEST.tar.xz')
                
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
                os.system("patch < /eos/cms/store/group/phys_generator/cvmfs/gridpacks/mg_amg_patch/26x.patch")

                
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
              

              
