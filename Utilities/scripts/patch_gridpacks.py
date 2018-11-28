import os,sys

# ##############################################
# ############ CHECK EOS PERMISSIONS ###########
# ##############################################
# print('assign 755 to all EOS gridpack directories'); sys.stdout.flush()
# os.system('find /eos/cms/store/group/phys_generator/cvmfs/gridpacks/ -type d -exec chmod 755 {} +')
# print('assign 644 to all EOS gridpack files'); sys.stdout.flush()
# os.system('find /eos/cms/store/group/phys_generator/cvmfs/gridpacks/ -type f -exec chmod 644 {} +');
# sys.exit(1)
# ##############################################
# ########## END CHECK EOS PERMISSIONS #########
# ##############################################

my_path = '/tmp/'+os.environ['USER']+'/replace_gridpacks/'

requests = [
            # # 'TOP-PhaseIITDRFall17wmLHEGS-00022',
            # # 'HIG-PhaseIITDRFall17wmLHEGS-00048',
            # # 'SMP-PhaseIITDRFall17wmLHEGS-00029',
            # # 'SMP-PhaseIITDRFall17wmLHEGS-00035',
            # # 'TOP-PhaseIITDRFall17wmLHEGS-00012',
            # # 'HIG-PhaseIITDRFall17wmLHEGS-00040',
            # # 'TOP-PhaseIITDRFall17wmLHEGS-00019',
            # # 'HIG-PhaseIITDRFall17wmLHEGS-00055',
            # # # 'B2G-PhaseIITDRFall17wmLHEGS-00012',
            # 'EXO-RunIIFall17wmLHEGS-00002', 'EXO-RunIIFall17wmLHEGS-00003', 'EXO-RunIIFall17wmLHEGS-00004',
            # 'EXO-RunIIFall17wmLHEGS-00005', 'EXO-RunIIFall17wmLHEGS-00006', 'EXO-RunIIFall17wmLHEGS-00007',
            # 'EXO-RunIIFall17wmLHEGS-00008', 'EXO-RunIIFall17wmLHEGS-00009', 'EXO-RunIIFall17wmLHEGS-00010',
            # 'EXO-RunIIFall17wmLHEGS-00011', 'EXO-RunIIFall17wmLHEGS-00032', 'HIG-RunIIFall17wmLHEGS-00001',
            # 'HIG-RunIIFall17wmLHEGS-00002', 'HIG-RunIIFall17wmLHEGS-00023', 'HIG-RunIIFall17wmLHEGS-00024',
            # 'HIG-RunIIFall17wmLHEGS-00027', 'HIG-RunIIFall17wmLHEGS-00029', 'HIG-RunIIFall17wmLHEGS-00030',
            # 'HIG-RunIIFall17wmLHEGS-00032', 'HIG-RunIIFall17wmLHEGS-00034', 'HIG-RunIIFall17wmLHEGS-00045',
            # 'HIG-RunIIFall17wmLHEGS-00046', 'HIG-RunIIFall17wmLHEGS-00047', 'MUO-RunIIFall17wmLHEGS-00002',
            # 'HIG-RunIIFall17wmLHEGS-00036','HIG-RunIIFall17wmLHEGS-00037','HIG-RunIIFall17wmLHEGS-00038', 'HIG-RunIIFall17wmLHEGS-00039',
            # 'HIG-RunIIFall17wmLHEGS-00274',
            # 'HIG-RunIIFall17wmLHEGS-00015', 'HIG-RunIIFall17wmLHEGS-00016', 'HIG-RunIIFall17wmLHEGS-00017', 'HIG-RunIIFall17wmLHEGS-00018',
            # 'HIG-RunIIFall17wmLHEGS-00019', 'HIG-RunIIFall17wmLHEGS-00020', 'HIG-RunIIFall17wmLHEGS-00021', 'HIG-RunIIFall17wmLHEGS-00022',
            # 'MUO-RunIIFall17wmLHEGS-00001',
            
            # "HIG-RunIISummer15wmLHEGS-01618",
            # "HIG-RunIISummer15wmLHEGS-01581",
            # "HIG-RunIISummer15wmLHEGS-01625",
            # "EXO-RunIISummer15wmLHEGS-05204",
            # "HIG-RunIISummer15wmLHEGS-01740",
            # "HIG-RunIISummer15wmLHEGS-01593",
            # "EXO-RunIISummer15wmLHEGS-05187",
            # "B2G-RunIISummer15wmLHEGS-01381",
            # "HIG-RunIISummer15wmLHEGS-01669",
            # "HIG-RunIISummer15wmLHEGS-01692",
            # "HIG-RunIISummer15wmLHEGS-01583",
            # "EXO-RunIISummer15wmLHEGS-04918",
            # "EXO-RunIISummer15wmLHEGS-04937",
            # "EXO-RunIISummer15wmLHEGS-04893",
            # "SUS-RunIISummer15wmLHEGS-00187",
            # "HIG-RunIISummer15wmLHEGS-01579",
            # "HIG-RunIISummer15wmLHEGS-01695",
            # "HIG-RunIISummer15wmLHEGS-01614",
            # "B2G-RunIISummer15wmLHEGS-01383",
            # "HIG-RunIISummer15wmLHEGS-01617",
            # "EXO-RunIISummer15wmLHEGS-05015",
            # "B2G-RunIISummer15wmLHEGS-01386",
            # "EXO-RunIISummer15wmLHEGS-04908",
            # "EXO-RunIISummer15wmLHEGS-05205",
            # "B2G-RunIISummer15wmLHEGS-01382",
            # "HIG-RunIISummer15wmLHEGS-01687",
            # "EXO-RunIISummer15wmLHEGS-05209",
            # "HIG-RunIISummer15wmLHEGS-01688",
            # "EXO-RunIISummer15wmLHEGS-04916",
            # "B2G-RunIISummer15wmLHEGS-01384",
            # "HIG-RunIISummer15wmLHEGS-01587",
            # "HIG-RunIISummer15wmLHEGS-01689",
            # "B2G-RunIISummer15wmLHEGS-01532",
            # "EXO-RunIISummer15wmLHEGS-05018",
            # "EXO-RunIISummer15wmLHEGS-04935",
            # "EXO-RunIISummer15wmLHEGS-04880",
            # "EXO-RunIISummer15wmLHEGS-05016",
            # "EXO-RunIISummer15wmLHEGS-04905",
            # "HIG-RunIISummer15wmLHEGS-01668",
            # "HIG-RunIISummer15wmLHEGS-01739",
            # "EXO-RunIISummer15wmLHEGS-04931",
            # "B2G-RunIISummer15wmLHEGS-01364",
            # "HIG-RunIISummer15wmLHEGS-01622",
            # "B2G-RunIISummer15wmLHEGS-01380",
            # "EXO-RunIISummer15wmLHEGS-04930",
            # "EXO-RunIISummer15wmLHEGS-04913",
            # "HIG-RunIISummer15wmLHEGS-01642",
            # "HIG-RunIISummer15wmLHEGS-01580",
            # "HIG-RunIISummer15wmLHEGS-01673",
            # "HIG-RunIISummer15wmLHEGS-01597",
            # "HIG-RunIISummer15wmLHEGS-01630",
            # "B2G-RunIISummer15wmLHEGS-01379",
            # "HIG-RunIISummer15wmLHEGS-01683",
            # "EXO-RunIISummer15wmLHEGS-05236",
            # "B2G-RunIISummer15wmLHEGS-01545",
            # "HIG-RunIISummer15wmLHEGS-01607",
            # "EXO-RunIISummer15wmLHEGS-04917",
            # "B2G-RunIISummer15wmLHEGS-01530",
            # "HIG-RunIISummer15wmLHEGS-01636",
            # "B2G-RunIISummer15wmLHEGS-01370",
            # "EXO-RunIISummer15wmLHEGS-05225",
            # "EXO-RunIISummer15wmLHEGS-04890",
            # "HIG-RunIISummer15wmLHEGS-01631",
            # "HIG-RunIISummer15wmLHEGS-01663",
            # "HIG-RunIISummer15wmLHEGS-01693",
            # "EXO-RunIISummer15wmLHEGS-05233",
            # "EXO-RunIISummer15wmLHEGS-04907",
            # "EXO-RunIISummer15wmLHEGS-05010",
            # "HIG-RunIISummer15wmLHEGS-01662",
            # "B2G-RunIISummer15wmLHEGS-01367",
            # "EXO-RunIISummer15wmLHEGS-04906",
            # "HIG-RunIISummer15wmLHEGS-01606",
            # "B2G-RunIISummer15wmLHEGS-01536",
            # "HIG-RunIISummer15wmLHEGS-01589",
            # "EXO-RunIISummer15wmLHEGS-05009",
            # "HIG-RunIISummer15wmLHEGS-01690",
            # "B2G-RunIISummer15wmLHEGS-01534",
            # "HIG-RunIISummer15wmLHEGS-01694",
            # "EXO-RunIISummer15wmLHEGS-05019",
            # "HIG-RunIISummer15wmLHEGS-01596",
            # "HIG-RunIISummer15wmLHEGS-01674",
            # "HIG-RunIISummer15wmLHEGS-01664",
            # "EXO-RunIISummer15wmLHEGS-05012",
            # "B2G-RunIISummer15wmLHEGS-01542",
            # "EXO-RunIISummer15wmLHEGS-04928",
            # "B2G-RunIISummer15wmLHEGS-01537",
            # "EXO-RunIISummer15wmLHEGS-05013",
            # "EXO-RunIISummer15wmLHEGS-05212",
            # "EXO-RunIISummer15wmLHEGS-04887",
            # "HIG-RunIISummer15wmLHEGS-01616",
            # "SMP-RunIISummer15wmLHEGS-00174",
            # "B2G-RunIISummer15wmLHEGS-01543",
            # "EXO-RunIISummer15wmLHEGS-04912",
            # "HIG-RunIISummer15wmLHEGS-01620",
            # "B2G-RunIISummer15wmLHEGS-01527",
            # "EXO-RunIISummer15wmLHEGS-05007",
            # "EXO-RunIISummer15wmLHEGS-04882",
            # "HIG-RunIISummer15wmLHEGS-01601",
            # "HIG-RunIISummer15wmLHEGS-01672",
            # "B2G-RunIISummer15wmLHEGS-01374",
            # "EXO-RunIISummer15wmLHEGS-04960",
            # "B2G-RunIISummer15wmLHEGS-01369",
            # "B2G-RunIISummer15wmLHEGS-01528",
            # "SMP-RunIISummer15wmLHEGS-00200",
            # "HIG-RunIISummer15wmLHEGS-01634",
            # "EXO-RunIISummer15wmLHEGS-04892",
            # "EXO-RunIISummer15wmLHEGS-05223",
            # "HIG-RunIISummer15wmLHEGS-01691",
            # "EXO-RunIISummer15wmLHEGS-04874",
            # "B2G-RunIISummer15wmLHEGS-01385",
            # "B2G-RunIISummer15wmLHEGS-01371",
            # "EXO-RunIISummer15wmLHEGS-04876",
            # "EXO-RunIISummer15wmLHEGS-05221",
            # "EXO-RunIISummer15wmLHEGS-05017",
            # "B2G-RunIISummer15wmLHEGS-01363",
            # "B2G-RunIISummer15wmLHEGS-01533",
            # "EXO-RunIISummer15wmLHEGS-04938",
            # "B2G-RunIISummer15wmLHEGS-01387",
            # "HIG-RunIISummer15wmLHEGS-01675",
            # "EXO-RunIISummer15wmLHEGS-05216",
            # "B2G-RunIISummer15wmLHEGS-01535",
            # "HIG-RunIISummer15wmLHEGS-01635",
            # "B2G-RunIISummer15wmLHEGS-01529",
            # "EXO-RunIISummer15wmLHEGS-04883",
            # "HIG-RunIISummer15wmLHEGS-01680",
            # "HIG-RunIISummer15wmLHEGS-01677",
            # "EXO-RunIISummer15wmLHEGS-05037",
            # "SMP-RunIISummer15wmLHEGS-00196",
            # "HIG-RunIISummer15wmLHEGS-01632",
            # "SMP-RunIISummer15wmLHEGS-00197",
            # "EXO-RunIISummer15wmLHEGS-04877",
            # "EXO-RunIISummer15wmLHEGS-05021",
            # "HIG-RunIISummer15wmLHEGS-01660",
            # "EXO-RunIISummer15wmLHEGS-05232",
            # "EXO-RunIISummer15wmLHEGS-05222",
            # "HIG-RunIISummer15wmLHEGS-01588",
            # "HIG-RunIISummer15wmLHEGS-01623",
            # "B2G-RunIISummer15wmLHEGS-01378",
            # "HIG-RunIISummer15wmLHEGS-01627",
            # "EXO-RunIISummer15wmLHEGS-04875",
            # "B2G-RunIISummer15wmLHEGS-01368",
            # "EXO-RunIISummer15wmLHEGS-04915",
            # "EXO-RunIISummer15wmLHEGS-05011",
            # "EXO-RunIISummer15wmLHEGS-05248",
            # "HIG-RunIISummer15wmLHEGS-01643",
            # "EXO-RunIISummer15wmLHEGS-05235",
            # "EXO-RunIISummer15wmLHEGS-04942",
            # "EXO-RunIISummer15wmLHEGS-05020",
            # "EXO-RunIISummer15wmLHEGS-04800",
            # "EXO-RunIISummer15wmLHEGS-05186",
            # "HIG-RunIISummer15wmLHEGS-01602",
            # "EXO-RunIISummer15wmLHEGS-04939",
            # "EXO-RunIISummer15wmLHEGS-04919",
            # "EXO-RunIISummer15wmLHEGS-04894",
            # "B2G-RunIISummer15wmLHEGS-01375",
            # "HIG-RunIISummer15wmLHEGS-01586",
            # "EXO-RunIISummer15wmLHEGS-04884",
            # "HIG-RunIISummer15wmLHEGS-01681",
            # "B2G-RunIISummer15wmLHEGS-01373",
            # "B2G-RunIISummer15wmLHEGS-01365",
            # "HIG-RunIISummer15wmLHEGS-01639",
            # "B2G-RunIISummer15wmLHEGS-01526",
            # "B2G-RunIISummer15wmLHEGS-01366",
            # "HIG-RunIISummer15wmLHEGS-01600",
            # "B2G-RunIISummer15wmLHEGS-01376",
            # "EXO-RunIISummer15wmLHEGS-05198",
            # "EXO-RunIISummer15wmLHEGS-04940",
            # "HIG-RunIISummer15wmLHEGS-01665",
            # "EXO-RunIISummer15wmLHEGS-04896",
            # "EXO-RunIISummer15wmLHEGS-05245",
            # "EXO-RunIISummer15wmLHEGS-05008",
            # "HIG-RunIISummer15wmLHEGS-01626",
            # "B2G-RunIISummer15wmLHEGS-01541",
            # "B2G-RunIISummer15wmLHEGS-01372",
            # "EXO-RunIISummer15wmLHEGS-05242",
            # "EXO-RunIISummer15wmLHEGS-04909",
            # "HIG-RunIISummer15wmLHEGS-01595",
            # "B2G-RunIISummer15wmLHEGS-01531",
            # "B2G-RunIISummer15wmLHEGS-01377",
            # "HIG-RunIISummer15wmLHEGS-01670",
            # "B2G-RunIISummer15wmLHEGS-01540",
            # "EXO-RunIISummer15wmLHEGS-05193",
            # "B2G-RunIISummer15wmLHEGS-01538",
            # "B2G-RunIISummer15wmLHEGS-01539",
            # "B2G-RunIISummer15wmLHEGS-01362",
            # "EXO-RunIISummer15wmLHEGS-05190",
            # "HIG-RunIISummer15wmLHEGS-01684",
            # "EXO-RunIISummer15wmLHEGS-05217",
            # "B2G-RunIISummer15wmLHEGS-01544",
            # "EXO-RunIISummer15wmLHEGS-05210",
            # "EXO-RunIISummer15wmLHEGS-04910",
            # "EXO-RunIISummer15wmLHEGS-05014",
            # "EXO-RunIISummer15wmLHEGS-05208",
            # "HIG-RunIISummer15wmLHEGS-01676",
            # "EXO-RunIISummer15wmLHEGS-05227",
            # "EXO-RunIISummer15wmLHEGS-04929",
            # "HIG-RunIISummer15wmLHEGS-01582",
            # "HIG-RunIISummer15wmLHEGS-01585",
            "SMP-RunIISummer15wmLHEGS-00201",
            "SMP-RunIISummer15wmLHEGS-00203",
            "SMP-RunIISummer15wmLHEGS-00204",
            "SMP-RunIISummer15wmLHEGS-00205",
            "SMP-RunIISummer15wmLHEGS-00206",
            "SMP-RunIISummer15wmLHEGS-00207",
            "SMP-RunIISummer15wmLHEGS-00208",
            "SMP-RunIISummer15wmLHEGS-00212",
            "SMP-RunIISummer15wmLHEGS-00213",
            "SMP-RunIISummer15wmLHEGS-00214",
            "SMP-RunIISummer15wmLHEGS-00215",
            "SMP-RunIISummer15wmLHEGS-00216",
            "SMP-RunIISummer15wmLHEGS-00217",
            "SMP-RunIISummer15wmLHEGS-00218",
            "SMP-RunIISummer15wmLHEGS-00219",
            "SMP-RunIISummer15wmLHEGS-00220",
            "FSQ-RunIISummer15wmLHEGS-00003",
            "FSQ-RunIISummer15wmLHEGS-00005",
            "FSQ-RunIISummer15wmLHEGS-00006",
            "FSQ-RunIISummer15wmLHEGS-00007",
            "FSQ-RunIISummer15wmLHEGS-00008",
            "FSQ-RunIISummer15wmLHEGS-00009",
            "FSQ-RunIISummer15wmLHEGS-00011",
            "FSQ-RunIISummer15wmLHEGS-00012",
            "FSQ-RunIISummer15wmLHEGS-00014",
           ]


##########################################
######## START LOOP OVER PREPIDS #########
##########################################
# for prepid in requests:

        # os.system('echo '+prepid)
        
        # os.system('mkdir -p '+my_path+'/'+prepid)
        # os.chdir(my_path+'/'+prepid)
        # os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+prepid+' -O '+prepid)
        # gridpack_cvmfs_path = os.popen('grep \/cvmfs '+prepid).read()
        # gridpack_cvmfs_path = gridpack_cvmfs_path.replace('\"',"\'").split('\'')[1]
        # os.system('rm '+prepid)
##########################################
######## END LOOP OVER PREPIDS ###########
##########################################

##########################################
########## START LOOP OVER FILES #########
##########################################
import hashlib
# for subdir, dirs, files in os.walk("/cvmfs/cms.cern.ch/phys_generator/gridpacks/2017/13TeV/madgraph/"):
for subdir, dirs, files in os.walk("/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc6_amd64_gcc481/13TeV/madgraph/"):
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
        if not os.path.exists(gridpack_eos_path_noiter):
            print "ERROR: BACKUP GRIDPACK NOT EXISTING! COPYING"
            print('cp -n '+gridpack_eos_path+' '+gridpack_eos_path_noiter); sys.stdout.flush()
            os.system('cp -n '+gridpack_eos_path+' '+gridpack_eos_path_noiter)
            # continue
        else:
            print "Backup gridpack already existing, i.e. should be patched!";
            continue
        if "_NLO_" in gridpack_eos_path or "_FXFX_" in gridpack_eos_path:
            print "gridpack seems to be NLO, skipping";
            continue
        statinfo_noiter = os.stat(gridpack_eos_path_noiter)
        if statinfo_noiter.st_size <= 10485760:
            print "ERROR: BACKUP CORRUPTED! SKIPPING"; sys.stdout.flush()
            continue
        # print('rm -rf '+my_path+'/'+prepid+'/*'); sys.stdout.flush()
        
        os.system('ls -lrt '+gridpack_eos_path_noiter)
        print 'statinfo_noiter',statinfo_noiter; sys.stdout.flush()
        if statinfo_noiter.st_size <= 10485760:
            print "ERROR: backup file size is less than 10MB, skipping\n"; sys.stdout.flush()
            continue
        else:
            print "OK: backup file size is greater than 10MB"; sys.stdout.flush()

        os.system('ls -lrt '+gridpack_eos_path)
        statinfo = os.stat(gridpack_eos_path)
        print 'statinfo',statinfo; sys.stdout.flush()
        
        if statinfo.st_mtime > statinfo_noiter.st_mtime and statinfo.st_size > statinfo_noiter.st_size*0.9:
            if statinfo.st_size == statinfo_noiter.st_size:
                print "FROM THE SIZE, GRIDPACK SEEMS TO BE NLO (i.e. no need to be patched)\n"; sys.stdout.flush()
            else:
                print "FROM THE SIZE, GRIDPACK SEEMS TO BE PATCHED (i.e. modified more recently than backup)\n"; sys.stdout.flush()
            continue
        

        print 'PATCHING: untarring',gridpack_eos_path_noiter; sys.stdout.flush()
        os.system('rm -rf '+my_path+'/'+prepid+'/*'); sys.stdout.flush()
        os.system('tar xf '+gridpack_eos_path_noiter)
        
          
        merge = os.popen('grep \/cvmfs\/cms.cern.ch\/phys_generator\/gridpacks\/lhe_merger\/merge.pl runcmsgrid.sh').read()
        tmpdir = os.popen('grep _CONDOR_SCRATCH_DIR mgbasedir/Template/LO/SubProcesses/refine.sh').read()
        if merge == "" or tmpdir == "":
            if merge == "":
                print 'replacing the string in runcmsgrid.sh'; sys.stdout.flush()
                os.system("sed -e '/\.\/run\.sh\ / {' -e 'r \/cvmfs\/cms.cern.ch\/phys_generator\/gridpacks\/mg_amg_patch\/runcmsgrid_backup_5k_mergeFix.sh' -e 'd' -e '}' -i runcmsgrid.sh")
            if tmpdir == "" and os.path.isfile("./mgbasedir/Template/LO/SubProcesses/refine.sh"):
                print 'applying the tmpdir patch in mgbasedir/Template/LO/SubProcesses/refine.sh'; sys.stdout.flush()
                os.system("cd mgbasedir; \
                      wget https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/MadGraph5_aMCatNLO/patches/0018-propagate-madevent-exitcode-properly.patch; \
                      patch -p1 < 0018-propagate-madevent-exitcode-properly.patch; \
                      rm 0018-propagate-madevent-exitcode-properly.patch")


                
            print 'tarring to gridpack.tar.xz for',prepid; sys.stdout.flush()
            os.system('tar cfJ gridpack.tar.xz ./')
            statinfo_new = os.stat('gridpack.tar.xz')
            print 'statinfo_new',statinfo_new; sys.stdout.flush()
            if statinfo_new.st_size > statinfo_noiter.st_size*0.9:
                print 'copying gridpack.tar.xz to',gridpack_eos_path; sys.stdout.flush()
                os.system('cp gridpack.tar.xz '+gridpack_eos_path)
            else:
                print 'ERROR: from the size, gridpack.tar.xz seems corrupted, skipping'; sys.stdout.flush()
        else:
            print "GRIDPACK SEEMS TO BE PATCHED (i.e. merge already present)\n"; sys.stdout.flush()
            continue
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
        
              

              
