import os
import sys
import time
import string
import re 
import argparse
import textwrap
from textwrap import dedent

parser = argparse.ArgumentParser(
    formatter_class=argparse.RawDescriptionHelpFormatter,
    description=textwrap.dedent('''\
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
                  * [WARNING] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                  *                    --> Please check that you use:"
                  *                        >=  CMSSW_7_1_32_patch1 in 7_1_X or  
                  *                        >= CMSSW_9_3_9_patch1 in 9_3_X or
                  *                        >= 10_1_3 in 10_1_X or" 
                  *                        >= CMSSW_10_2_0_pre2 in 10_2_X.
                  *                        Your request uses version xxxxx :
                  *                         If you are not using a proper CMSSW version, please switch to that or
                  *                         re-create the gridpack using the updated genproductions area

               ERRORS:
                  * [ERROR] Memory is not 2300 or 4000 MB"
                  * [ERROR] Memory is 2300 MB while number of cores is XX but not = 1
                  * [ERROR] Memory is 4000 MB while number of cores is 1 but not = 2,4 or 8
                  * [ERROR] Gridpack should have used cvmfs path instead of eos path
                  * [ERROR] Matched sample but matching efficiency is 1!
                  * [ERROR] MG5_aMC@NLO multi-run patch missing in gridpack - please re-create a gridpack
                  *            using updated genproductions area
                  * [ERROR] May be wrong fragment: powheg/madgraph/mcatnlo in dataset name but settings in 
                            fragment not correct or vice versa"
                  * [ERROR] Fragment may be wrong: check "+word+" settings in the fragment"
                  *         if madgraph: You run MG5_aMC@NLO at LO but you have  Pythia8aMCatNLOSettings_cfi in fragment
                  *                --> please remove it from the fragment
                  * [ERROR] Tune configuration wrong in the fragment
                  * [ERROR] PS weights in config but CMSSW version is not 10_2_3 - please check!	
                  * [ERROR] Parton shower weight configuration not OK in the fragment
                  * [ERROR] Filters in the fragment but filter efficiency = 1
                  * [ERROR] You are using a loop induced process, [noborn=QCD].
                  *         Please remove all occurances of Pythia8aMCatNLOSettings from the fragment
                  * [ERROR] You are using a loop induced process, [noborn=QCD].
                  *         Please remove all TimeShower:nPartonsInBorn from the fragment 

               The script also checks if there is no fragment there is a hadronizer used.'''))
parser.add_argument('--prepid', type=str, help="check mcm requests using prepids", nargs='+')
parser.add_argument('--ticket', type=str, help="check mcm requests using ticket number", nargs=1)
args = parser.parse_args()

if args.prepid is not None:
    parser.parse_args('--prepid 1'.split())
    print "---> "+str(len(args.prepid))+" requests will be checked:"
    prepid = args.prepid
print " "

os.system('source /afs/cern.ch/cms/PPD/PdmV/tools/McM/getCookie.sh')
os.system('cern-get-sso-cookie -u https://cms-pdmv.cern.ch/mcm/ -o ~/private/prod-cookie.txt --krb --reprocess')
sys.path.append('/afs/cern.ch/cms/PPD/PdmV/tools/McM/')

from rest import McM
from json import dumps
from itertools import groupby

mcm = McM(dev=False)

def root_requests_from_ticket(ticket_prepid, include_docs=False):
    """
    Return list of all root (first ones in the chain) requests of a ticket.
    By default function returns list of prepids.
    If include_docs is set to True, function will return whole documents
    """
    mccm = mcm.get('mccms',ticket_prepid)
    query = ''
    for root_request in mccm.get('requests',[]):
        if isinstance(root_request,str) or isinstance(root_request,unicode):
            query += '%s\n' % (root_request)
        elif isinstance(root_request,list):
             # List always contains two elements - start and end of a range
            query += '%s -> %s\n' % (root_request[0], root_request[1])
    requests = mcm.get_range_of_requests(query)
    if not include_docs:
        # Extract only prepids
        requests = [r['prepid'] for r in requests]
    return requests


if args.ticket is not None:
    parser.parse_args('--ticket 1'.split())
    ticket = args.ticket
    ticket = ticket[0]
    print "------------------------------------"
    print "--> Ticket = "+ticket
    print "------------------------------------"
#    print(root_requests_from_ticket(ticket))
    prepid = []
    for rr in root_requests_from_ticket(ticket):
        if 'GS' in rr or 'wmLHE' in rr or 'pLHE' in rr:
            prepid.append(rr)



prepid = list(set(prepid)) #to avoid requests appearing x times if x chains have the same request 
       
for x in range(0,len(prepid)):
    print(prepid[x])           

for num in range(0,len(prepid)):
    query_str = 'prepid='+prepid[num]
    res = mcm.get('requests', query=query_str)
    if len(res) == 0 :
        print "***********************************************************************************"
        print "Something's wrong - can not get the request parameters"
        print "***********************************************************************************"

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
        filter_eff = r['generator_parameters'][-1]['filter_efficiency']
        match_eff = r['generator_parameters'][-1]['match_efficiency']
        print (pi)
        check = []
        purepythiacheck = []
        powhegcheck = []
        tunecheck = []
        psweightscheck = [] #ps = parton shower
        MGpatch = [] 
        ME = ["PowhegEmissionVeto","aMCatNLO"] # ME = matrix element
        MEname = ["powheg","madgraph","mcatnlo"]
        tune = ["CP5","CUEP8M1","CP1","CP2","CP3","CP4"] 
        mcatnlo_flag = 0
        loop_flag = 0
        knd =  10
        nPartonsInBorn_flag = 0
        matching = 10
        ickkw = 'del' # ickkw = matching parameter in madgraph
        autoptjmjj_c = 'del'
        drjj = 1000
        for item in te:
            timeperevent = float(item)
        if timeperevent > 150.0 :
            print "* [WARNING] Large time/event - please check"
        if '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw :
            print "* [WARNING] Are you sure you want to use "+cmssw+"release which is not standard"
            print "*           which may not have all the necessary GEN code."
        if totalevents >= 100000000 :
            print "* [WARNING] Is "+str(totalevents)+" events what you really wanted - please check!"
        os.popen('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+pi+' -O '+pi).read()
        fsize = os.path.getsize(pi)
        os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_test/'+pi+' -O '+pi)
        gettest = os.popen('grep cff '+pi+' | grep curl').read()
        if fsize == 0:
            print "* No fragment associated to this request"
            print "       [Probably OK] if this is the hadronizer you intended to use?: "+gettest
        ftest = os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_test/'+pi+' -O '+pi+'_get_test')
        ttxt = os.popen('grep nThreads '+pi+'_get_test').read()
        if int(os.popen('grep -c nThreads '+pi+'_get_test').read()) == 0 :
            nthreads = 1
        else :
            nthreads = int(re.search('nThreads(.*?) --',ttxt).group(1))
        if mem != 2300 and mem != 4000 :
            print "* [ERROR] Memory is not 2300 or 4000 MB"
        if mem == 2300 and nthreads != 1 :
            print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 1"
        if mem == 4000 and nthreads == 1 :
            print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 2,4 or 8"
        os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+pi+' -O '+pi)
        os.system('mkdir -p '+my_path+'/'+pi)
        if int(os.popen('grep -c eos '+pi).read()) == 1 :
            print "* [ERROR] Gridpack should have used cvmfs path instead of eos path"
        if int(os.popen('grep -c nPartonsInBorn '+pi).read()) == 1:
            nPartonsInBorn_flag = 1
        for ind, word in enumerate(MEname):
            if word in dn.lower() :
                if ind == 2 :
                    knd = 1 
                else :
                    knd = ind
                check.append(int(os.popen('grep -c pythia8'+ME[knd]+'Settings '+pi).read()))
                check.append(int(os.popen('grep -c "from Configuration.Generator.Pythia8'+ME[knd]+'Settings_cfi import *" '+pi).read()))
                check.append(int(os.popen('grep -c "pythia8'+ME[knd]+'SettingsBlock," '+pi).read()))
                if check[2] == 1:
                    mcatnlo_flag = 1
                if ind > 0 :
                    os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+pi+' -O '+my_path+'/'+pi+'/'+pi)
                    gridpack_cvmfs_path = os.popen('grep \/cvmfs '+my_path+'/'+pi+'/'+pi+'| grep -v \'#args\' ').read()
                    gridpack_cvmfs_path = gridpack_cvmfs_path.split('\'')[1]
                    os.system('tar xf '+gridpack_cvmfs_path+' -C'+my_path+'/'+pi)
                    fname_p = my_path+'/'+pi+'/'+'process/madevent/Cards/proc_card_mg5.dat'
                    fname_p2 = my_path+'/'+pi+'/'+'process/Cards/proc_card.dat'
                    if os.path.isfile(fname_p) is True :
                        loop_flag = int(os.popen('more '+fname_p+' | grep -c "noborn=QCD"').read())
                    elif os.path.isfile(fname_p2) is True : 
                        loop_flag = int(os.popen('more '+fname_p2+' | grep -c "noborn=QCD"').read())
                    fname = my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'
                    fname2 = my_path+'/'+pi+'/'+'process/Cards/run_card.dat'
                    if os.path.isfile(fname) is True :
#                        ickkw = os.popen('grep "= ickkw" '+fname).read()
                       ickkw = os.popen('more '+fname+' | tr -s \' \' | grep "= ickkw"').read()
                       autoptjmjj_c = os.popen('more '+fname+' | tr -s \' \' | grep "= auto_ptj_mjj"').read()
                       drjj_c = os.popen('more '+fname+' | tr -s \' \' | grep "= drjj"').read()
                    elif os.path.isfile(fname2) is True :    
#                        ickkw = os.popen('grep "= ickkw" '+fname2).read()
                       ickkw = os.popen('more '+fname2+' | tr -s \' \' | grep "= ickkw"').read()
                       autoptjmjj_c = os.popen('more '+fname2+' | tr -s \' \' | grep "= auto_ptj_mjj"').read()
                       drjj_c = os.popen('more '+fname2+' | tr -s \' \' | grep "= drjj"').read()
                    test_autoptjmjj = re.search(r'true',autoptjmjj_c,re.M|re.I).group()
                    test_drjj_c = re.search(r'\d*\.\d+|\d+', drjj_c, re.M|re.I).group()
                    matching = int(re.search(r'\d+',ickkw).group())
                    ickkw = str(ickkw)  
                    if matching == 1 and test_autoptjmjj.lower() != "true":
                        print "* [ERROR] Please set True = auto_ptj_mjj for MLM"    
                    if matching == 1 and test_drjj_c > 0:
                        print "* [ERROR] drjj should be set to 0.0 for MLM"
                    if matching == 1 or matching == 2:
                        if match_eff == 1:
                            print "* [ERROR] Matched sample but matching efficiency is 1!"
                    if ind < 2:
                        MGpatch.append(int(os.popen('more '+my_path+'/'+pi+'/'+'runcmsgrid.sh | grep -c "FORCE IT TO"').read()))
                        MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'mgbasedir/Template/LO/SubProcesses/refine.sh').read()))
                        MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'process/madevent/SubProcesses/refine.sh').read()))
                        if MGpatch[0] == 1 and MGpatch[1] == 1 and MGpatch[2] == 1:
                            print "* [OK] MG5_aMC@NLO leading order patches OK in gridpack"
                        if MGpatch[0] != 1:
                            print "* [ERROR] MG5_aMC@NLO multi-run patch missing in gridpack - please re-create a gridpack"
                            print "*            using updated genproductions area"
                        if MGpatch[1] == 0 or MGpatch[2] == 0:
                            print "* [WARNING] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                            print "*           --> Please check that you use:"
                            print "*               >=  CMSSW_7_1_32_patch1 in 7_1_X or"  
                            print "*               >= CMSSW_9_3_9_patch1 in 9_3_X or"
                            print "*               >= 10_1_3 in 10_1_X or" 
                            print "*               >= CMSSW_10_2_0_pre2 in 10_2_X."
                            print "*           Your request uses "+cmssw+" :"
                            print "*           If you are not using a proper CMSSW version, please switch to that or"
                            print "*           re-create the gridpack using the updated genproductions area"
                if matching >= 2 and check[0] == 2 and check[1] == 1 and check[2] == 1 :
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    if matching ==3 :  
                        print "* [Caution: To check manually] This is a FxFx sample. Please check 'JetMatching:nJetMax' is set"
                        print "*           correctly as number of partons in born matrix element for highest multiplicity."
                    if matching > 3 :
                        print "* [Caution: To check manually] This is a Powheg NLO sample. Please check 'nFinal' is"
                        print "*               set correctly as number of final state particles (BEFORE THE DECAYS)"
                        print "*                                   in the LHE other than emitted extra parton."
                elif matching == 1 and check[0] == 0 and check[1] == 0 and check[2] == 0 :    
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    print "* [Caution: To check manually] This is a MadGraph LO sample with Jet matching sample. Please check"
                    print "*                   'JetMatching:nJetMax' is set correctly as number of partons"
                    print "*                              in born matrix element for highest multiplicity."
                elif matching == 0 and word == "madgraph" and check[0] == 0 and check[1] == 0 and check[2] == 0 :
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                elif matching == 0 and word == "mcatnlo" and check[0] == 2 and check[1] == 1 and check[2] == 1 and loop_flag != 1:
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    print "* [Caution: To check manually] Is this a MadGraph NLO sample without matching. Please check 'TimeShower:nPartonsInBorn'"
                    print "*                                                   is set correctly as number of coloured particles"
                    print "*                                                  (before resonance decays) in born matrix element."
                else:     
                    print "* [ERROR] Fragment may be wrong: check "+word+" settings in the fragment"
                    if matching <= 1 and word == "madgraph":
                        print "*        You run MG5_aMC@NLO at LO but you have  Pythia8aMCatNLOSettings_cfi in fragment"
                        print "*           --> please remove it from the fragment"
                    if word == "powheg" :
                        print "* [However: To check manually] if this is a "+word+" but loop induced process such as gg->ZH," 
                        print "*           then fragment is OK (no need to have Pythia8PowhegEmissionVetoSettings)"
        if knd == 1 :
             powhegcheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
             if powhegcheck[0] > 0 :
                 print "* [ERROR] Please remove POWHEG settings for MG requests."
        if knd > 2 :
             purepythiacheck.append(int(os.popen('grep -c -i Pythia8aMCatNLOSettings '+pi).read()))
             purepythiacheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
             if purepythiacheck[0] > 0 or purepythiacheck[1] >0 :
                 print "* [ERROR] Please remove aMCatNLO or POWHEG settings for pure Pythia requests."
        if loop_flag == 1:
            if mcatnlo_flag == 1: 
                print "* [ERROR] You are using a loop induced process, [noborn=QCD]."
                print "*         Please remove all occurances of Pythia8aMCatNLOSettings from the fragment"
            if nPartonsInBorn_flag == 1:
                print "* [ERROR] You are using a loop induced process, [noborn=QCD]."
                print "*         Please remove all TimeShower:nPartonsInBorn from the fragment"                        
        for kk in range (0, 6):   
            tunecheck.append(int(os.popen('grep -c -i '+tune[kk]+' '+pi).read()))
        tune_check_tmp = [i for i, n in enumerate(tunecheck) if n > 2]
        if tune_check_tmp[0] < 3 or len(tune_check_tmp) > 1 and fsize != 0:
            print "* [ERROR] Tune configuration may be wrong in the fragment"
        elif tune_check_tmp[0] > 2 and fsize != 0:    
            print "* [OK] Tune configuration probably OK in the fragment"
            if tunecheck[0] > 2 :
                if 'Fall18' not in pi and 'Fall17' not in pi :
                    print "* [WARNING] Do you really want to have tune "+tune[0] +" in this campaign?"
        if 'Fall18' in pi and fsize != 0:
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) != 1 :
                print "* [WARNING] No parton shower weights configuration in the fragment. In the Fall18 campaign, we recommend to include Parton Shower weights"
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) == 1 :
                cmssw_version    = int(re.search("_[0-9]?[0-9]_[0-9]?[0-9]_[0-9]?[0-9]",cmssw).group().replace('_',''))
                if cmssw_version < int('10_2_3'.replace('_','')) :
                    print "* [ERROR] PS weights in config but CMSSW version is previous 10_2_3 - please check!"	    
                psweightscheck.append(int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettingsBlock," '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettings" '+pi).read()))
                if psweightscheck[0] == 1 and psweightscheck[1] == 1 and psweightscheck[2] == 2 :
                    print "* [OK] Parton shower weight configuration probably OK in the fragment"
                else:
                    print "* [ERROR] Parton shower weight configuretion not OK in the fragment" 
        if int(os.popen('grep -c -i filter '+pi).read()) > 3 and filter_eff == 1:
            print "* [ERROR] Filters in the fragment but filter efficiency = 1"
    os.popen("rm -rf "+my_path+pi).read()  
print "***********************************************************************************"
print ""
