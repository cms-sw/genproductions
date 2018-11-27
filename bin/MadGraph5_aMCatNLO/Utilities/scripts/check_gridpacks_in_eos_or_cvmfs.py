import os,sys

my_path = '/tmp/'+os.environ['USER']+'/replace_gridpacks/'

# requests=[]
requests = [
'HIG-PhaseIISummer17wmLHEGENOnly-00097',
]


# for num in range(0,3):
       # requests.append('HIG-RunIIFall17wmLHEGS-0'+str(num+1627))
# print requests

######## START LOOP OVER PREPIDS #########
for prepid in requests:
        os.system('echo '+prepid)
        os.system('mkdir -p '+my_path+'/'+prepid)
        os.chdir(my_path+'/'+prepid)
        os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+prepid+' -O '+prepid)
        gridpack_cvmfs_path = os.popen('grep \/cvmfs '+prepid+'| grep -v \'#args\' ').read()
        gridpack_cvmfs_path = gridpack_cvmfs_path.split('\'')[1]
        gridpack_eos_path = gridpack_cvmfs_path.replace("/cvmfs/cms.cern.ch/phys_generator","/eos/cms/store/group/phys_generator/cvmfs")
#################### Select to Check EOS or CVMFS ####################################
#       os.system('tar xf '+gridpack_eos_path+' -C'+my_path+'/'+prepid)
        # os.system('tar xf '+gridpack_cvmfs_path+' -C'+my_path+'/'+prepid)
#####################################################################################
        os.system('echo "multi-run (0 bad, 1 good)"; more '+my_path+'/'+prepid+'/'+'runcmsgrid.sh | grep -c "FORCE IT TO"')
        os.system('echo "tmpdir-1 (0 bad, 1 good)"; grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+prepid+'/'+'mgbasedir/Template/LO/SubProcesses/refine.sh')
        os.system('echo "tmpdir-2 (0 bad, 1 good)"; grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+prepid+'/'+'process/madevent/SubProcesses/refine.sh')
        os.system('echo "ickkw"; grep "= ickkw" '+my_path+'/'+prepid+'/'+'process/madevent/Cards/run_card.dat')
        os.system('echo "------------------------------------"')
        os.system('rm '+prepid)
######## END LOOP OVER PREPIDS ###########
