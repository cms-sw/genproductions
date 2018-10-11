import os,sys,time,string,re, argparse,textwrap
from textwrap import dedent

parser = argparse.ArgumentParser(
    formatter_class=argparse.RawDescriptionHelpFormatter,
    description=textwrap.dedent('''\
            python request_fragment_check.py [prepid in mcm]
            ------------------------------------------------ 
               This script currently checks for the following to give an OK, WARNING, or ERROR
            
               WARNINGS:
                  * [WARNING] if time per event > 150 seconds
                  * [WARNING] if CMSSW version is not 10_2 and 9_3 and 7_1
                  * [WARNING] total number of events > 100000000
                  * [WARNING] powheg+pythia sample contains Pythia8PowhegEmissionVetoSettings 
                              - warning to check whether it is a loop induced process
                  * [WARNING] if CP5 tune is used but campaign is not Fall18 or Fall17
                  * [WARNING] if Fall18 campaing but no parton shower weights configuration in the fragment

               ERRORS:
                  * [ERROR] Gridpack should have used cvmfs path instead of eos path
                  * [ERROR] May be wrong fragment: powheg/madgraph/mcatnlo in dataset name but settings in 
                            fragment not correct or vice versa"
                  * [ERROR] Tune configuration wrong in the fragment"
                  * [ERROR] Parton shower weight configuretion not OK in the fragment"

               The script also checks if there is no fragment there is a hadronizer used.'''))
parser.add_argument('--prepid', type=str, help="check single mcm request", nargs='+')
args = parser.parse_args()

parser.parse_args('--prepid 1'.split())
print "---> "+str(len(args.prepid))+" requests will be checked:"
for indnum in range(0,len(args.prepid)):
    print(args.prepid[indnum])
print " "

os.system('source /afs/cern.ch/cms/PPD/PdmV/tools/McM/getCookie.sh')
os.system('cern-get-sso-cookie -u https://cms-pdmv.cern.ch/mcm/ -o ~/private/prod-cookie.txt --krb --reprocess')
sys.path.append('/afs/cern.ch/cms/PPD/PdmV/tools/McM/')

from rest import McM
from json import dumps
from itertools import groupby

mcm = McM(dev=False)

for num in range(0,len(args.prepid)):
    query_str = 'prepid='+args.prepid[num]
    res = mcm.get('requests', query=query_str)
    if len(res) == 0 :
        print "***********************************************************************************"
        print "Something's wrong - can not get the request parameters"
        print "***********************************************************************************"
        exit()

    my_path =  '/tmp/'+os.environ['USER']+'/gridpacks/'
    print ""
    print "***********************************************************************************"

    for r in res:
        pi = r['prepid']
        dn = r['dataset_name']
        te = r['time_event']
        totalevents = r['total_events']
        cmssw = r['cmssw_release']
        mem = r['memory']
        print (pi)
        check = []
        tunecheck = []
        psweightscheck = []
        ME = ["PowhegEmissionVeto","aMCatNLO"]
        MEname = ["powheg","madgraph","mcatnlo"]
        tunename = ["CP5","CUETP8M1"]
        tune = ["CP5","CUEP8M1"] 
        matching = 10
        ickkw = 'del'
        for item in te:
            timeperevent = float(item)
        if timeperevent > 150.0 :
            print "* [WARNING] Large time/event - please check"
        if '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw :
            print "* [WARNING] Are you sure you want to use "+cmssw+"release which is not standard"
            print "*           which may not have all the necessary GEN code."
        if totalevents >= 100000000 :
            print "* [WARNING] Is "+str(totalevents)+" events what you really wanted - please check!"
        fsize = os.popen('wget --spider --server-response https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+pi+' -O - 2>&1 | sed -ne \'/Length/{s/.*: //;p}\'').read()
        if 'unspecified' in fsize :
            os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_test/'+pi+' -O '+pi)
            gettest = os.popen('grep cff '+pi+' | grep curl').read()
            for index, aword in enumerate(MEname):
                if aword in dn.lower() :
                    print "* "+gettest
                    print "* [OK] Probably OK if the above hadronizer is what you intended to use"
                else:
                    print "* [ERROR] Something may be wrong - name/hadronizer mismatch - please check!"
                    print "***********************************************************************************"
                    print ""
                    exit()
        ftest = os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_test/'+pi+' -O '+pi+'_get_test')
        ttxt = os.popen('grep nThreads '+pi+'_get_test').read()
        if int(os.popen('grep -c nThreads '+pi+'_get_test').read()) == 0 :
            nthreads = 1
        else :
            nthreads = int(re.search('nThreads(.*?) --',ttxt).group(1))
        if mem != 2300 and mem != 4000 :
            print "* [ERROR] Memory is not 2300 or 4000 GB"
        if mem == 2300 and nthreads != 1 :
            print "* [ERROR] Memory is "+str(mem)+" GB while number of cores is "+str(nthreads)+" but not = 1"
        if mem == 4000 and nthreads == 1 :
            print "* [ERROR] Memory is "+str(mem)+" GB while number of cores is "+str(nthreads)+" but not = 2,4 or 8"
        os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+pi+' -O '+pi)
        os.system('mkdir -p '+my_path+'/'+pi)
        if int(os.popen('grep -c eos '+pi).read()) == 1 :
            print "* [ERROR] Gridpack should have used cvmfs path instead of eos path"
        for ind, word in enumerate(MEname):
            if word in dn.lower() :
                if ind == 2 :
                    knd = 1 
                else :
                    knd = ind
                check.append(int(os.popen('grep -c pythia8'+ME[knd]+'Settings '+pi).read()))
                check.append(int(os.popen('grep -c "from Configuration.Generator.Pythia8'+ME[knd]+'Settings_cfi import *" '+pi).read()))
                check.append(int(os.popen('grep -c "pythia8'+ME[knd]+'SettingsBlock," '+pi).read()))
                if ind > 0 :
                    os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+pi+' -O '+my_path+'/'+pi+'/'+pi)
                    gridpack_cvmfs_path = os.popen('grep \/cvmfs '+my_path+'/'+pi+'/'+pi+'| grep -v \'#args\' ').read()
                    gridpack_cvmfs_path = gridpack_cvmfs_path.split('\'')[1]
                    os.system('tar xf '+gridpack_cvmfs_path+' -C'+my_path+'/'+pi)
                    fname = my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'
                    fname2 = my_path+'/'+pi+'/'+'process/Cards/run_card.dat'
                    if os.path.isfile(fname) is True :
                        ickkw = os.popen('grep "= ickkw" '+fname).read()
                    elif os.path.isfile(fname2) is True :    
                        ickkw = os.popen('grep "= ickkw" '+fname2).read()
                    ickkw = str(ickkw)    
                    matching = int(re.search(r'\d+',ickkw).group())
                if matching >= 2 and check[0] == 2 and check[1] == 1 and check[2] == 1 :
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    if matching ==3 :  
                        print "* [WARNING] This is a FxFx sample. Please check 'JetMatching:nJetMax' is set correctly"
                        print "*                as number of partons in born matrix element for highest multiplicity."
                    if matching > 3 :
                        print "* [WARNING] This is a Powheg NLO sample. Please check 'nFinal' is set correctly"
                        print "*                        as number of final state particles (BEFORE THE DECAYS)"
                        print "*                                   in the LHE other than emitted extra parton."
                elif matching == 1 and check[0] == 0 and check[1] == 0 and check[2] == 0 :    
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    print "* [WARNING] This is a MadGraph LO sample with Jet matching sample. Please check"
                    print "*                   'JetMatching:nJetMax' is set correctly as number of partons"
                    print "*                              in born matrix element for highest multiplicity."
                elif matching == 0 and word == "madgraph" and check[0] == 0 and check[1] == 0 and check[2] == 0 :
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                elif matching == 0 and word == "mcatnlo" and check[0] == 2 and check[1] == 1 and check[2] == 1 :
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    print "* [WARNING] This is a MadGraph NLO sample without matching. Please check 'TimeShower:nPartonsInBorn'"
                    print "*                                                   is set correctly as number of coloured particles"
                    print "*                                                  (before resonance decays) in born matrix element."
                else:     
                    print "* [ERROR] May be wrong fragment: "+word+" in dataset name but settings in fragment not correct or vice versa"
                    if word == "powheg" :
                        print "* [WARNING] if this is a"+word+" sample but a loop induced process such as ggZH," 
                        print "*           then fragment is OK (no need to have Pythia8PowhegEmissionVetoSettings)"
        for kk in range (0, 2):   
            tunecheck.append(int(os.popen('grep -c -i '+tune[kk]+' '+pi).read()))
        if tunecheck[0] < 3 and tunecheck[1] < 3 :
            print "* [ERROR] Tune configuration wrong in the fragment"
        elif tunecheck[0] > 2 or tunecheck[1] >2 :
            print "* [OK] Tune configuration probably OK in the fragment"
            if tunecheck[0] > 2 :
                if 'Fall18' not in pi and 'Fall17' not in pi :
                    print "* [WARNING] Do you really want to have tune "+tunename[0] +" in this campaign?"
        if 'Fall18' in pi :
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) != 1 :
                print "* [WARNING] No parton shower weights configuration in the fragment. In the Fall18 campaign, we recommend to include Parton Shower weights"
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) == 1 :
                if '10_2_3' not in cmssw :
                    "* [ERROR] PS weights in config but CMSSW version is not 10_2_3 - please check!"
                    exit() 		    
                psweightscheck.append(int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettingsBlock," '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettings" '+pi).read()))
                if psweightscheck[0] == 1 and psweightscheck[1] == 1 and psweightscheck[2] == 2 :
                    print "* [OK] Parton shower weight configuration probably OK in the fragment"
                else:
                    print "* [ERROR] Parton shower weight configuretion not OK in the fragment" 
                                                                                                                                    
    print "***********************************************************************************"
    print ""
                                                                                                                                    
