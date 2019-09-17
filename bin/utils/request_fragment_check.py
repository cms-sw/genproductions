#!/bin/env python2
import os
import sys
import re 
import argparse
import textwrap
#import json
from datetime import datetime
###########Needed to check for ultra-legacy sample consistency check############################################
os.system('cern-get-sso-cookie -u https://cms-pdmv.cern.ch/mcm/ -o prod-cookie.txt --krb --reprocess')
################################################################################################################
sys.path.append('/afs/cern.ch/cms/PPD/PdmV/tools/McM/')
from rest import McM

parser = argparse.ArgumentParser(
    formatter_class=argparse.RawDescriptionHelpFormatter,
    description=textwrap.dedent('''\
            ------------------------------------------------ 
               This script currently checks for the following to give an OK, WARNING, or ERROR
               and does a patch for the MG5_aMC LO nthreads problem if needed. 
            
               WARNINGS:
                  * [WARNING] comphep, calchep, or herwigpp request. Please check manually
                  * [WARNING] if time per event > 150 seconds
                  * [WARNING] if CMSSW version is not 10_6, 10_2, 9_3, and 7_1 or (8_0 but not Summer16FSPremix) or (9_4 but not Fall17FSPremix) 
                  * [WARNING] total number of events > 100000000
                  * [WARNING] CMSSW version is different than its base UL17 request
                  * [WARNING] No fragment associated to this request"
                  *           is this the hadronizer you intended to use?:
                  * [WARNING] PDF:pSet is missing (if you want to use NNPDF3.1)
                  *           For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1
                  *           PDF access method should be like"
                  *           e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'
                  * [WARNING] PDF access method is wrong (if you want to use NNPDF3.1). Please correct:
                  *           e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'"
                  * [WARNING] Dataset name is not regular:
                  *           Please add the Generator name to the dataset.
                  * [WARNING] gridpack patch problem (this is still being investigated)
                  * [WARNING] You may try to request more events per phase-space region in the gridpack.
                  * [WARNING] Didn't find powheg process in runcmsgrid.sh
                  * [WARNING] nJetMax(=X) is not equal to the number of jets specified in the proc card(=Y).
                  *            Is it because this is an exclusive production with additional samples with higher multiplicity generated separately?
                  * [WARNING] To check manually - This is a matched MadGraph LO sample. Please check 'JetMatching:nJetMax' =X is OK and
                  *            correctly set as number of partons in born matrix element for highest multiplicity.
                  * [WARNING] This a MadGraph NLO sample without matching. Please check 'TimeShower:nPartonsInBorn'
                  *                                                   is set correctly as number of coloured particles
                  *                                                  (before resonance decays) in born matrix element.
                  * [WARNING] You're using MG5_aMC vXX in an Ultra Legacy Campaign. Are you sure that's what you want?
                  * [WARNING] nFinal(=X) may not be equal to the number of final state particles before decays (=Y)
                  * [WARNING] To check manually - This is a Powheg NLO sample. Please check 'nFinal' is
                  *               set correctly as number of final state particles (BEFORE THE DECAYS)
                  *                                   in the LHE other than emitted extra parton.
                  * [WARNING] foldphi = X, should be at least 2 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] foldy = X, should be at least 5 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] ncall1 = X, should be at least 550000 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] ncall2 = X, should be at least 75000 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] foldcsi = X, should be at least 2 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] itmx1 = X, should be at least 7 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] powheg+pythia sample contains Pythia8PowhegEmissionVetoSettings 
                              - warning to check whether it is a loop induced process
                  * [WARNING] if this is a Powheg request but loop induced process such as gg->ZH,
                  *           then fragment is OK (no need to have Pythia8PowhegEmissionVetoSettings)
                  * [WARNING] Please remove aMCatNLO or POWHEG settings if this is a pure Pythia request.
                  *           If it's not a pure request, in the future, please include madgraph/powheg or amcatnlo
                  *           in the name of the dataset
                  * [WARNING] Check if there is some extra tune setting
                  * [WARNING] No automated check of Sherpa ps/tune parameters yet
                  * [WARNING] Number of extra or replaced tune parameters is at least 
                  *           Please check tune configuration carefully (e.g. are the non-replaced parameters the ones you want)
		  * [WARNING] None standard tune - please check the fragment carefully.
                  * [WARNING] Do you really want to have tune X in this campaign?
                  * [WARNING] No parton shower weights configuration in the fragment. In the Fall18 campaign, we recommend to include Parton Shower weights
		  * [WARNING] Filters in the fragment but filter efficiency = 1
                  * [WARNING] bwcutoff set to X (> 15 GeV). Note that large bwcutoff values can cause problems in production.
                  * [WARNING] Matched sample but matching efficiency is 1!
                  * [WARNING] Please remove aMCatNLO or POWHEG settings if this is a pure Pythia request.
                  *           If it's not a pure request, in the future, please include madgraph/powheg or amcatnlo
                  *           in the name of the dataset


               ERRORS:
                  * [ERROR] Fragment of XX is different than its base UL17 request: 
                  *         Please make sure that XX has _exactly_ the same settings as XX
                  * [ERROR] missing fragment line(s) for herwig
                  * [ERROR] Herwig7LHEPowhegSettings_cfi should be loaded in the fragment
                  * [ERROR] herwig7LHEPowhegSettingsBlock missing for powheg+herwig7 request"
                  * [ERROR] hw_lhe_Powheg_settings missing for powheg+herwig7 request
                  * [ERROR] Herwig7LHEMG5aMCatNLOSettings_cfi should be loaded in the fragment
                  * [ERROR] herwig7LHEMG5aMCatNLOSettingsBlock missing for MG5_aMC[NLO]+herwig7 request
                  * [ERROR] hw_lhe_MG5aMCatNLO_settings missing for MG5_aMC[NLO]+herwig7 request
                  * [ERROR] please try to increase the filter efficiency if (8*3600/timeperevent)*filter_eff < 50 
                            and timeperevent > 0 and not a ppd request
                  * [ERROR] 8 core request with memory different from 15900 GB. Please set the memory to 15900 GB if CMSSW version >= 10_6_X and nthreads = 8 
                            and mem != 15900 and not ppd request
                  * [ERROR] Memory is not 2300, 4000 or 15900 MB
                  * [ERROR] Memory is 2300 MB while number of cores is XX but not = 1
                  * [ERROR] Memory is 4000 MB while number of cores is 1 but not = 2,4 or 8
                  * [ERROR] Memory is 15900 MB while number of cores is not 8 and 16
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign but Memory is not 14700, 5900, 400, or 2300 MB
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 14700 but nthreads != 8
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 5900 but nthreads != 4
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 4000 but nthreads != 2
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 2300 but nthreads != 1
                  * [ERROR] Gridpack should have used cvmfs path instead of eos path
                  * [ERROR] minbias in CMSSW_10_X: SigmaTotal:mode should have been set to 0
                  * [ERROR] minbias in CMSSW_10_X: SigmaTotal:sigmaEl should have been set to 21.89
                  * [ERROR] minbias in CMSSW_10_X: SigmaTotal:sigmaTot should have been set to 100.309
                  * [ERROR] minbias in CMSSW_10_X: PDF access method is wrong. Please correct.
                  * [ERROR] Dataset name does not have the tune name
                  *         Please add the tune name to the dataset.
                  * [ERROR] Dataset name does not contain a parton shower code name
                  *         Please add the parton shower name to the dataset name.
                  * [ERROR] herwigpp = parton_shower not in run_card.dat
                  * [ERROR] Although the name of the dataset has ~Madgraph, the gridpack doesn't seem to be a MG5_aMC one. Please check.
                  * [ERROR] Please add \'set FxFxHandler:MergeMode FxFx\'
	          *         and set FxFxHandler:njetsmax 4
                  * [ERROR] Please load herwig7CommonMergingSettingsBlock
                  * [ERROR] MG5_aMC@NLO multi-run patch missing in gridpack - please re-create a gridpack
                  *            using updated genproductions area
                  * [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing
                  *         And the request is using a version X that does not contain the patch.
                  *         Please use >= 7_1_32_patch1 or CMSSW_9_3_9_patch1 or 10_2_0_pre2
                  * [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.
                  *         And the request is using a version X that does not contain the patch.
                  *         In this release, please at least use CMSSW_7_1_32_patch1
                  * [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.
                  *         And the request is using a version X that does not contain the patch.
                  *         In this release, please at least use CMSSW_9_3_9_patch1
                  * [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.
                  *         And the request is using a version X that does not contain the patch.
                  *         In this release, please at least use CMSSW_10_2_0_pre2
                  * [ERROR] didn't find name in powheg.input
                  * [ERROR] MG5_aMC@NLO LO nthreads patch not made in EOS
                  * [ERROR] Fragment may be wrong: check powheg settings in the fragment
                  * [ERROR] You run MG5_aMC@NLO at LO but you have  Pythia8aMCatNLOSettings_cfi in fragment
                  *           --> please remove it from the fragment
                  * [ERROR] Please remove POWHEG settings for MG requests.
                  * [ERROR] You are using a loop induced process, [noborn=QCD].
                  *         Please remove all occurances of Pythia8aMCatNLOSettings from the fragment
                  * [ERROR] You are using a loop induced process, [noborn=QCD].
                  *         Please remove all TimeShower:nPartonsInBorn from the fragment
                  * [ERROR] Tune configuration may be wrong in the fragment
 	    	  *          or pythia8CUEP8M1Settings are overwritten by some other parameters as in CUETP8M2T4
                  * [ERROR] PS weights in config but CMSSW version is < 10_2_3 - please check!
                  * [ERROR] Parton shower weight configuration not OK in the fragment
                                                                                    '''))
parser.add_argument('--prepid', type=str, help="check mcm requests using prepids", nargs='+')
parser.add_argument('--ticket', type=str, help="check mcm requests using ticket number", nargs=1)
parser.add_argument('--bypass_status', help="don't check request status in mcm", action='store_false')
parser.add_argument('--bypass_validation', help="proceed to next prepid even if there are errors", action='store_true')
parser.add_argument('--apply_many_threads_patch', help="apply the many threads MG5_aMC@NLO LO patch if necessary", action='store_true')
parser.add_argument('--dev', help="Run on DEV instance of McM", action='store_true')
parser.add_argument('--debug', help="Print debugging information", action='store_true')
args = parser.parse_args()

if args.prepid is not None:
    if len(args.prepid) == 1:
        print "---> "+str(len(args.prepid))+" request will be checked:"
    if len(args.prepid) != 1:     
        print "---> "+str(len(args.prepid))+" requests will be checked:"
    prepid = args.prepid
print " "


# Use no-id as identification mode in order not to use a SSO cookie
mcm = McM(id='no-id', dev=args.dev, debug=args.debug)
mcm2 = McM(cookie='cookiefile.txt', dev=args.dev, debug=args.debug)

if args.dev is True:
    mcm_link = "https://cms-pdmv-dev.cern.ch/mcm/"
else:
    mcm_link = "https://cms-pdmv.cern.ch/mcm/"

def get_request(prepid):
    result = mcm._McM__get('public/restapi/requests/get/%s' % (prepid))
    if not result:
        return {}

    result = result.get('results', {})
    return result


def get_range_of_requests(query):
    result = mcm._McM__put('public/restapi/requests/listwithfile', data={'contents': query})
    if not result:
        return {}

    result = result.get('results', {})
    return result


def get_ticket(prepid):
    result = mcm._McM__get('public/restapi/mccms/get/%s' % (prepid))
    if not result:
        return {}

    result = result.get('results', {})
    return result

def get_requests_from_datasetname(dn):
    result = mcm2.get('requests', query='dataset_name=%s' % (dn))
    if not result:
        return {}

    return result    

if args.dev:
    print "Running on McM DEV!\n"


def root_requests_from_ticket(ticket_prepid, include_docs=False):
    """
    Return list of all root (first ones in the chain) requests of a ticket.
    By default function returns list of prepids.
    If include_docs is set to True, function will return whole documents
    """
    mccm = get_ticket(ticket_prepid)
    query = ''
    for root_request in mccm.get('requests',[]):
        if isinstance(root_request,str) or isinstance(root_request,unicode):
            query += '%s\n' % (root_request)
        elif isinstance(root_request,list):
             # List always contains two elements - start and end of a range
            query += '%s -> %s\n' % (root_request[0], root_request[1])
    requests = get_range_of_requests(query)
    if not include_docs:
        # Extract only prepids
        requests = [r['prepid'] for r in requests]
    return requests


if args.ticket is not None:
    ticket = args.ticket
    ticket = ticket[0]
    print "------------------------------------"
    print "--> Ticket = "+ticket
    print "------------------------------------"
#    print(root_requests_from_ticket(ticket))
    prepid = []
    for rr in root_requests_from_ticket(ticket):
        if 'GS' in rr or 'wmLHE' in rr or 'pLHE' in rr or 'FS' in rr:
            prepid.append(rr)


prepid = list(set(prepid)) #to avoid requests appearing x times if x chains have the same request 

print "Current date and time: %s" % (datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
for x in prepid:
    print(x)

for num in range(0,len(prepid)):
    res = get_request(prepid[num])
    if len(res) == 0 :
        print "***********************************************************************************"
        print "Something's wrong - can not get the request parameters"
        print "***********************************************************************************"

    my_path =  '/tmp/'+os.environ['USER']+'/gridpacks/'
#    print "JSON Dump:"
#    print "----------"
#    print(json.dumps(res,indent = 2))
    print "***********************************************************************************"

    # Create an array of one element so further for loop would not be removed and code re-indented
    res = [res]
    for r in res:
        pi = r['prepid']
        dn = r['dataset_name']
        te = r['time_event']
        totalevents = r['total_events']
        cmssw = r['cmssw_release']
        test_cs_version = cmssw.split('_')
        mgversion = 0
        mem = r['memory']
        filter_eff = r['generator_parameters'][-1]['filter_efficiency']
        match_eff = r['generator_parameters'][-1]['match_efficiency']
        print pi+"    Status= "+r['status']
	print dn
#        os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/reqs_to_bypass_check_cmssw_vs_mg.txt -O reqs_to_bypass_check_cmssw_vs_mg.txt')
#        file1 = set(line.strip() for line in open('reqs_to_bypass_check_cmssw_vs_mg.txt'))
#        list_bypass_check = []
#        for line in file1:
#            list_bypass_check.append(line)
        if args.bypass_status and r['status'] != "defined":
	    print "--> Skipping since the request is not in defined state"
	    print "--> Use --bypass_status option to look at all requests irrespective of state" 
	    continue
        check = []
        purepythiacheck = []
        powhegcheck = []
        tunecheck = []
        psweightscheck = [] #ps = parton shower
        MGpatch = []
        MGpatch2 = []
        ME = ["PowhegEmissionVeto","aMCatNLO"] # ME = matrix element
        MEname = ["powheg","madgraph","mcatnlo","jhugen","mcfm"]
        tune = ["CP5","CUEP8M1","CP1","CP2","CP3","CP4","CP5TuneUp","CP5TuneDown"]
        tunename = ["CP5","CUETP8M1","CUETP8M2T4","CP1","CP2","CP3","CP4","CP5TuneUp","CP5TuneDown","CH3"]
        psname = ["pythia","herwig","sherpa"]
        n_ext_par = 0
        mcatnlo_flag = 0
        loop_flag = 0
        knd =  -1
	slha_flag = 0
        grid_points_flag = 0
        nPartonsInBorn_flag = 0
        matching = 10
        matching_c = 0
        ickkw = 'del' # ickkw = matching parameter in madgraph
        ickkw_c = 100
        maxjetflavor = 0
        nJetMax = 100
        particle_gun = 0
        tunparmark = 0
	jet_count_tmp = []
        nFinal = 100
        jet_count = 0
        bw = -1 
        error = 0
        warning = 0
        et_flag = 0
        herwig_flag = 0
        pf = []
        ppd = 0
        if "ppd" in pi.lower():
            ppd = 1
        req_type = "dummy"
        if "gen" in pi.lower(): 
            req_type = "genonly"
        if "gs" in pi.lower(): 
            req_type = "gs"
        if "plhe" in pi.lower(): 
            req_type = "plhe"
        if "herwig" in dn.lower():
            herwig_flag = 1
        if "comphep" in dn.lower() or "calchep" in dn.lower() or "herwigpp" in dn.lower():
            print "* [WARNING] comphep, calchep, or herwigpp request. Please check manually"
            warning = warning + 1
            continue
        for item in te:
            timeperevent = float(item)
        if timeperevent > 150.0 :
            print "* [WARNING] Large time/event="+str(timeperevent)+" - please check"
            warning = warning + 1
        version_not_ok = 0    
        if '8_0' in cmssw and "Summer16FSPremix" not in pi:
            version_not_ok = 1
        if '9_4' in cmssw and "Fall17FSPremix" not in pi:
            version_not_ok = 1      
        if '10_6' not in cmssw and '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw and version_not_ok == 1:
            print "* [WARNING] Are you sure you want to use "+cmssw+" release which is not standard"
            print "*           which may not have all the necessary GEN code."
            warning = warning + 1
        if totalevents >= 100000000 :
            print "* [WARNING] Is "+str(totalevents)+" events what you really wanted - please check!"
            warning = warning + 1
        os.popen('wget -q '+mcm_link+'public/restapi/requests/get_fragment/'+pi+' -O '+pi).read()
        fsize = os.path.getsize(pi)
        f1 = open(pi,"r") 
        f2 = open(pi+"_tmp","w")
        data_f1 = f1.read()
        data_f2 = re.sub(r'(?m)^ *#.*\n?', '',data_f1)
        # Ultra-legacy sample settings' compatibility
        if "Summer19UL16" in pi or "Summer19UL18" in pi:
            prime = get_requests_from_datasetname(dn)
            for rr in prime:
                pi_prime = rr['prepid']
                cmssw_prime = rr['cmssw_release']
                if 'UL17' in pi_prime and 'GEN' in pi_prime:
                    pi_prime = pi_prime
                    break
            print"This is an UL16 or UL18 request so GEN settings will be compared to the corresponding UL17 request: "+pi_prime       
            os.popen('wget -q '+mcm_link+'public/restapi/requests/get_fragment/'+pi_prime+' -O '+pi_prime).read()
            f1_prime = open(pi_prime,"r")
            f2_prime = open(pi_prime+"_tmp","w")
            data_f1_prime = f1_prime.read()
            data_f2_prime = re.sub(r'(?m)^ *#.*\n?', '',data_f1_prime)
            if (data_f2 == data_f2_prime) == True:
                print"[OK] Two requests have the same fragment."
            else:
                print"[ERROR] Fragment of "+pi+" is different than its base UL17 request: "+pi_prime
                print"        Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime
                error=error+1
            if (cmssw == cmssw_prime) == True:
                print"[OK] Two requests have the same CMSSW version."
            else:
                print"[WARNING] CMSSW version of "+pi+" is different than its base UL17 request: "+pi_prime
                print"        Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime
                warning=warning+1                
            f1_prime.close()
            f2_prime.write(data_f2_prime)
            f2_prime.close()
        f1.close()
        f2.write(data_f2)
        f2.close()
        os.system('mkdir -p '+my_path+'/'+pi)
        os.system('mkdir -p '+my_path+'/eos/'+pi)
        os.system('mv '+pi+'_tmp '+pi)
        os.system('cp '+pi+' '+my_path+'/'+pi+'/.')
        os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_test/'+pi+' -O '+pi+'_get_test')
        gettest = os.popen('grep cff '+pi+'_get_test'+' | grep curl').read()

        if herwig_flag != 0:
            os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_frag_lines.txt -O herwig_frag_lines.txt')
            file1 = set(line.strip().replace(",","")  for line in open('herwig_frag_lines.txt'))
            file2 = set(line.strip().replace(",","") for line in open(pi))
            herwig_check = []
            herwig_mat_err = 0
            for line in file1:
                if line not in file2:
                    herwig_check.append(line)
            if len(herwig_check) != 0:
                print "* [ERROR] "+ str(len(herwig_check)) + " missing fragment line(s) for herwig:"
                print herwig_check
                error = error + len(herwig_check)
            if "powheg" in dn.lower():
                if int(os.popen('grep -c Herwig7LHEPowhegSettings_cfi '+pi).read()) == 0:
                    print "* [ERROR] Herwig7LHEPowhegSettings_cfi should be loaded in the fragment"
                    error = error + 1   
                    herwig_mat_err = 1
                if int(os.popen('grep -c herwig7LHEPowhegSettingsBlock '+pi).read()) == 0:
                    print "* [ERROR] herwig7LHEPowhegSettingsBlock missing for powheg+herwig7 request"
                    error = error + 1 
                    herwig_mat_err = 1
                if int(os.popen('grep -c hw_lhe_Powheg_settings '+pi).read()) == 0:
                    print "* [ERROR] hw_lhe_Powheg_settings missing for powheg+herwig7 request"
                    error = error + 1 
                    herwig_mat_err = 1
            if "mcatnlo" in dn.lower():
                if int(os.popen('grep -c Herwig7LHEMG5aMCatNLOSettings_cfi '+pi).read()) == 0:
                    print "* [ERROR] Herwig7LHEMG5aMCatNLOSettings_cfi should be loaded in the fragment"
                    error = error + 1 
                    herwig_mat_err = 1
                if int(os.popen('grep -c herwig7LHEMG5aMCatNLOSettingsBlock '+pi).read()) == 0:
                    print "* [ERROR] herwig7LHEMG5aMCatNLOSettingsBlock missing for MG5_aMC[NLO]+herwig7 request"
                    error = error + 1 
                    herwig_mat_err = 1
                if int(os.popen('grep -c hw_lhe_MG5aMCatNLO_settings '+pi).read()) == 0:
                    print "* [ERROR] hw_lhe_MG5aMCatNLO_settings missing for MG5_aMC[NLO]+herwig7 request"
                    error = error + 1 
                    herwig_mat_err = 1
            if herwig_mat_err == 0 and len(herwig_check) == 0:  
                print "*"
                print "* [OK] Herwig7 fragment probably OK"
                print "*"
        if fsize == 0:
            print "* [WARNING] No fragment associated to this request"
            print "*           is this the hadronizer you intended to use?: "+gettest
            warning = warning + 1
        ttxt = os.popen('grep nThreads '+pi+'_get_test').read()
        if int(os.popen('grep -c nThreads '+pi+'_get_test').read()) == 0 :
            nthreads = 1
        else :
            nthreads = int(re.search('nThreads(.*?) --',ttxt).group(1))
#        if (nthreads != 16 or mem != 15900) and (8*3600/timeperevent)*filter_eff < 50 and timeperevent > 0 and int(test_cs_version[1]) > 9:
        if  (8*3600/timeperevent)*filter_eff < 50 and timeperevent > 0 and int(test_cs_version[1]) > 9 and ppd == 0:
            print ("* [ERROR] please try to increase the filter efficiency")
            error = error + 1    
        if  (8*3600/timeperevent)*filter_eff < 50 and timeperevent > 0 and int(test_cs_version[1]) <= 9 and ppd == 0:
            print ("* [ERROR] please try to increase the filter efficiency")
            error = error + 1
        if int(test_cs_version[1]) >= 10 and int(test_cs_version[2]) >= 6 and nthreads == 8 and mem != 15900 and ppd == 0:
            print ("* [ERROR] 8 core request with memory different from 15900 GB. Please set the memory to 15900 GB")
            error = error + 1
        if "HIN-HINPbPbAutumn18GSHIMix" not in pi and "HINPbPbAutumn18wmLHEGSHIMix" not in pi and "HINPbPbAutumn18GS" not in pi and ppd == 0:    
            if mem != 2300 and mem != 4000 and mem != 15900:
                print "* [ERROR] Memory is not 2300, 4000 or 15900 MB"
                error = error + 1
            if mem == 2300 and nthreads != 1 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 1"
                error = error + 1
            if mem == 4000 and nthreads == 1 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 2,4 or 8"
                error = error + 1
            if mem == 15900 and (nthreads != 8 and nthreads != 16) :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 8 or 16"
                error = error + 1
#            if filter_eff < 0.001 and "gs" in req_type and (mem != 15900 or nthreads != 16):
#                print "* [ERROR] Low filter efficiency (<0.001) GS request w/o 15900 GB memory or 16 cores"
#                error = error +1
        if "HIN-HINPbPbAutumn18GSHIMix" in pi or "HINPbPbAutumn18wmLHEGSHIMix" in pi or "HINPbPbAutumn18GS" in pi and ppd == 0:
            if mem != 14700 and mem != 5900 and mem != 4000 and mem != 2300:
                print "* [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign but Memory is not 14700, 5900, 400, or 2300 MB"
                error = error + 1
            if mem == 14700 and nthreads != 8 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 8"
                error = error + 1
            if mem == 5900 and nthreads != 4 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 4"
                error = error + 1
            if mem == 4000 and nthreads != 2 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 2"
                error = error + 1
            if mem == 2300 and nthreads != 1:
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 1"
                error = error + 1
#        os.system('wget -q https://cms-pdmv.cern.ch/mcm/public/restapi/requests/get_fragment/'+pi+' -O '+pi)
#        os.system('mkdir -p '+my_path+'/'+pi)
#        os.system('mkdir -p '+my_path+'/eos/'+pi)

        gridpack_cvmfs_path_tmp = os.popen('grep \/cvmfs '+my_path+'/'+pi+'/'+pi).read()
#        gridpack_cvmfs_path_tmp = re.findall("/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.xz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tgz",gridpack_cvmfs_path_tmp)
#        gridpack_cvmfs_path = gridpack_cvmfs_path_tmp[0]
        if int(os.popen('grep -c grid_points '+pi).read()) != 0:
            grid_points_flag = 1
        gp_size = len(gridpack_cvmfs_path_tmp)
        if fsize != 0:    
            if int(os.popen('grep -c eos '+pi).read()) == 1 :
                print "* [ERROR] Gridpack should have used cvmfs path instead of eos path"
                error = error + 1
            if int(os.popen('grep -c nPartonsInBorn '+pi).read()) == 1:
                nPartonsInBorn_flag = 1
                print(os.popen('grep nPartonsInBorn '+pi).read())
            if int(os.popen('grep -c nJetMax '+pi).read()) == 1:  
                nJetMax = os.popen('grep nJetMax '+pi).read()
                if grid_points_flag == 1:
                    nJetMax = re.findall('nJetMax = \d+',nJetMax)
                    nJetMax = nJetMax[0]  
                nJetMax = re.findall('\d+',nJetMax)
                nJetMax = int(nJetMax[0])
            if int(os.popen('grep -c nFinal '+pi).read()) == 1:
                nFinal = os.popen('grep nFinal '+pi).read()
                nFinal =  re.findall('\d+',nFinal)
                nFinal = int(nFinal[0])
                print "nFinal="+str(nFinal)
            if int(os.popen('grep -c FlatRandomEGunProducer '+pi).read()) == 1 or int(os.popen('grep -c FlatRandomPtGunProducer '+pi).read()) == 1:
                particle_gun = 1
            print "Using CMSSW release: "+cmssw    
            if int(test_cs_version[2]) == 6 and ('CMSSW_10_6_0' not in cmssw or 'CMSSW_10_6_0_patch1' not in cmssw):
                tunparmark = 1
            if int(test_cs_version[1]) >= 10 and int(test_cs_version[2]) >= 5 and int(test_cs_version[2]) <= 6 and int(test_cs_version[3]) >= 0 and '10_5_0_pre1' not in cmssw and particle_gun == 0 and tunparmark == 0 and herwig_flag == 0:
                mb_mode = os.popen('grep SigmaTotal:mode '+pi).read()
                mb_mode = re.findall('\d*\.\d+|\d+',mb_mode)
                mb_SigmaEl = os.popen('grep SigmaTotal:sigmaEl '+pi).read()
                mb_SigmaEl = re.findall('\d*\.\d+|\d+',mb_SigmaEl)
                mb_SigmaTot = os.popen('grep SigmaTotal:sigmaTot '+pi).read()
                mb_SigmaTot = re.findall('\d*\.\d+|\d+',mb_SigmaTot)
                PDF_pSet_test = os.popen('grep PDF:pSet '+pi).read()
                PDF_pSet_test = re.findall('\d*\.\d+|\d+',PDF_pSet_test)
                PDF_pSet = os.popen('grep PDF:pSet '+pi+' | grep -c LHAPDF6:NNPDF31_nnlo_as_0118').read()
                tmp_flag = 0
                if len(mb_mode) == 0:
                    print "* [ERROR] SigmaTotal:mode is missing"
                    print "*         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1"
                    print "*         SigmaTotal:mode shoud be added by hand and set to 0"
                    error = error + 1
                    tmp_flag = 1
                if len(mb_SigmaEl) == 0:
                    print "* [ERROR] SigmaTotal:sigmaEl is missing"
                    print "*         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1"
                    print "*         SigmaTotal:sigmaEl should be added by hand and set to 21.89"
                    error = error + 1
                    tmp_flag = 1
                if len(mb_SigmaTot) == 0:
                    print "* [ERROR] SigmaTotal:sigmaTot is missing"
                    print "*         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1"
                    print "*         SigmaTotal:sigmaTot should be added by hand and set to 100.309"
                    error = error + 1
                    tmp_flag = 1
                if len(PDF_pSet_test) == 0:
                    print "* [WARNING] PDF:pSet is missing (if you want to use NNPDF3.1)"
                    print "*         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1"
                    print "*         PDF access method should be like"
                    print "*         e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'"
                    warning = warning + 1
                    tmp_flag = 1
                if tmp_flag == 0:
                    if int(mb_mode[0]) != 0:  
                        print "* [ERROR] SigmaTotal:mode should have been set to 0"
                        error = error+1
                    if abs(float(mb_SigmaEl[0])-21.88) > 0.1:
                        print "* [ERROR] SigmaTotal:sigmaEl should have been set to 21.89"
                        error = error+1
                    if abs(float(mb_SigmaTot[0])-100.308) > 0.01:
                        print "* [ERROR] SigmaTotal:sigmaTot should have been set to 100.309"
                        error = error+1
                    if int(PDF_pSet[0]) != 1:
                        print "* [WARNING] PDF access method is wrong (if you want to use NNPDF3.1). Please correct:"
                        print "*         e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'"
                        warning = warning + 1
                
#            gridpack_cvmfs_path = os.popen('grep \/cvmfs '+my_path+'/'+pi+'/'+pi+'| grep -v \'#args\' ').read()
#            gp_size = len(gridpack_cvmfs_path)
            if gp_size != 0:
#                gridpack_cvmfs_path_tmp = os.popen('grep \/cvmfs '+my_path+'/'+pi+'/'+pi).read()
                gridpack_cvmfs_path_tmp = re.findall("/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.xz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tgz",gridpack_cvmfs_path_tmp)
                gridpack_cvmfs_path = gridpack_cvmfs_path_tmp[0]
#                gridpack_cvmfs_path = gridpack_cvmfs_path.split('\'')[1]
                gridpack_eos_path = gridpack_cvmfs_path.replace("/cvmfs/cms.cern.ch/phys_generator","/eos/cms/store/group/phys_generator/cvmfs")
                print gridpack_cvmfs_path
                print gridpack_eos_path
                if int(os.popen('grep -c slha '+pi).read()) != 0 or int(os.popen('grep -c \%i '+pi).read()) != 0 or int(os.popen('grep -c \%s '+pi).read()) != 0:
                    slha_flag = 1
                if slha_flag == 1:
                    if int(os.popen('grep -c \%i '+pi).read()) != 0:
                        gridpack_cvmfs_path = gridpack_cvmfs_path.replace("%i","*")
                    if int(os.popen('grep -c \%s '+pi).read()) != 0:    
                        gridpack_cvmfs_path = gridpack_cvmfs_path.replace("%s","*")
                    if int(os.popen('grep -c \%d '+pi).read()) != 0:
                        gridpack_cvmfs_path = gridpack_cvmfs_path.replace("%d","*")
                    slha_all_path = os.path.dirname(gridpack_eos_path)    
                    gridpack_cvmfs_path = os.popen('ls '+ gridpack_cvmfs_path+' | head -1 | tr \'\n\' \' \'').read()
                    print "SLHA request - checking single gridpack:"
                    print gridpack_cvmfs_path
                os.system('tar xf '+gridpack_cvmfs_path+' -C '+my_path+'/'+pi)
                pw_gp = os.path.isfile(my_path+'/'+pi+'/'+'powheg.input')
                mg_gp = os.path.isfile(my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat')
                amcnlo_gp = os.path.isfile(my_path+'/'+pi+'/'+'process/Cards/run_card.dat')
                print "powheg "+str(pw_gp)
                print "mg "+str(mg_gp)        
                if any(word in dn for word in tunename):
                    print "* [OK] Data set name has a known tune" 
                else:
                    print "* [ERROR] Dataset name does not have the tune name: "+dn
                    print "*         Please add the tune name to the dataset."
                    error=error+1
                if any(word in dn.lower() for word in psname):
                    print "* [OK] Dataset name contains a parton shower code name"
                else:
                    print "* [ERROR] Dataset name does not contain a parton shower code name: "+dn
                    print "*         Please add the parton shower name to the dataset name."
                    error = error + 1
                if any(word in dn.lower() for word in MEname):
                    print "Data set name is regular: "+dn
                else:    
                    print "* [WARNING] Dataset name is not regular:"+dn
                    print "*           Please add the Generator name to the dataset."
                    warning=warning+1
                    if pw_gp is True:
                        dn = dn + "-powheg"
                    if mg_gp is True:
                        dn = dn + "-madgraph"
                    if amcnlo_gp is True:    
                        if matching_c == 0:
                            dn = dn + "-amcatnlo"
                        if matching_c == 3:
                            dn = dn + "-amcatnloFXFX"
                gp_log_loc = my_path+'/'+pi+'/gridpack_generation.log'
                if mg_gp is True or amcnlo_gp is True and os.path.isfile(gp_log_loc) is True:
                    pf.append(os.popen('grep \"saving rejects to\" '+gp_log_loc).read())
                    pf.append(os.popen('grep \"INFO: fail to reach target\" '+gp_log_loc).read())
                    pf.append(os.popen('grep \"INFO: Not enough events for at least one production mode\" '+gp_log_loc).read())
                    if len(pf[0]) != 0:
                        print "* [WARNING] "+pf[0]
                        print "*             gridpack patch problem."
                        warning = warning + 1
                    if len(pf[1]) !=0 or len(pf[2]) != 0:    
                        print "* [WARNING] "+pf[1]
                        print "*           "+pf[2]
                        print "*           You may try to request more events per phase-space region in the gridpack."
                        warning = warning + 1
                if mg_gp is True:        
                    ickkw_c = os.popen('more '+my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'+' | tr -s \' \' | grep "= ickkw"').read()
                    matching_c = int(re.search(r'\d+',ickkw_c).group())
                    maxjetflavor = os.popen('more '+my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'+' | tr -s \' \' | grep "= maxjetflavor"').read()
                    maxjetflavor = int(re.search(r'\d+',maxjetflavor).group())
                    print "maxjetflavor = "+str(maxjetflavor)
                    if matching_c == 3 and herwig_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat')
                        if herwigpp not in ps_hw:
                            print "* [ERROR] herwigpp = parton_shower not in run_card.dat"
                            error = error + 1
                        if int(os.popen('grep -c "set FxFxHandler:MergeMode FxFx" '+pi).read()) == 0:
			    print "* [ERROR] Please add \'set FxFxHandler:MergeMode FxFx\'"
			    print "*         and set FxFxHandler:njetsmax 4"
			    error = error + 1		
                    if matching_c == 2 and herwig_flag != 0:
			if int(os.popen('grep -c herwig7CommonMergingSettingsBlock').read()) == 0:
			    print "* [ERROR] Please load herwig7CommonMergingSettingsBlock"
			    error = error + 1		
                if amcnlo_gp is True:
                    ickkw_c = os.popen('more '+my_path+'/'+pi+'/'+'process/Cards/run_card.dat'+' | tr -s \' \' | grep "= ickkw"').read()
                    matching_c = int(re.search(r'\d+',ickkw_c).group())
                    print ickkw_c
                    if herwig_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+my_path+'/'+pi+'/'+'process/Cards/run_card.dat')
                        if herwigpp not in ps_hw:
                            print "* [ERROR] herwigpp = parton_shower not in run_card.dat"
                            error = error + 1
                    
        for ind, word in enumerate(MEname):
            if fsize == 0:
                break
            if ind == 3:
                break
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
                if ind == 0:
                    if gp_size == 0:
                        break
                    file_pwg_check =  my_path+'/'+pi+'/'+'pwhg_checklimits'
                    if os.path.isfile(file_pwg_check) is True :
                        print "grep from powheg pwhg_checklimits files"
                        nemit = os.popen('grep emitter '+file_pwg_check+' | grep process | head -n 1').read().replace('process','').replace('\n','').split(',')
                        nemitsplit = nemit[1].split()
                        nemitsplit_pr = nemitsplit[2:]
			nemitsplit = [x for x in nemitsplit_pr if x!=nemitsplit[0] and x!=nemitsplit[1]]
			print nemitsplit
                        nemitsplit = [100 if x == "***" else x for x in nemitsplit]
			nemitsplit_wo_leptons = [int(x) for x in nemitsplit]
			nemitsplit_wo_leptons = [abs(x) for x in nemitsplit_wo_leptons]
			nemitsplit_wo_leptons = [x for x in nemitsplit_wo_leptons if x < 11 or x > 18]
			nfinstatpar = len(nemitsplit_wo_leptons)-nemitsplit_wo_leptons.count(0)
			print nemitsplit_wo_leptons
                        if nfinstatpar == nFinal :
                            print "* [OK] nFinal(="+str(nFinal) + ") is equal to the number of final state particles before decays (="+str(nfinstatpar)+")"
                        if nfinstatpar != nFinal :
                            print "* [WARNING] nFinal(="+str(nFinal) + ") may not be equal to the number of final state particles before decays (="+str(nfinstatpar)+")"
                            warning = warning + 1
                    with open(os.path.join(my_path, pi, "runcmsgrid.sh")) as f:
                        content = f.read()
                        match = re.search(r"""process=(["']?)([^"']*)\1""", content)
                    if os.path.isfile(my_path+'/'+pi+'/'+'external_tarball/runcmsgrid.sh') is True:
                        with open(os.path.join(my_path, pi, "external_tarball/runcmsgrid.sh")) as f2:
                            content2 = f2.read()
                            match = re.search(r"""process=(["']?)([^"']*)\1""", content2)
                            et_flag = 1
                    if match:
                        process = match.group(2)
                        if process == "gg_H_quark-mass-effects":
                            #for more information on this check, see
                            #https://its.cern.ch/jira/browse/CMSCOMPPR-4874

                            #this configuration is ok at 125 GeV, but causes trouble starting at around 170:
                            #  ncall1=50000, itmx1=5, ncall2=50000, itmx2=5, foldcsi=1, foldy=1, foldphi=1
                            #from mH=300 GeV to 3 TeV, this configuration seems to be fine:
                            #  ncall1=550000, itmx1=7, ncall2=75000, itmx2=5, foldcsi=2, foldy=5, foldphi=2

                            #I'm printing warnings here for anything less than the second configuration.
                            #Smaller numbers are probably fine at low mass
                            desiredvalues = {
                              "ncall1": 550000,
                              "itmx1": 7,
                              "ncall2": 75000,
                              "itmx2": 5,
                              "foldcsi": 2,
                              "foldy": 5,
                              "foldphi": 2,
                            }
                            if et_flag == 0:
                                with open(os.path.join(my_path, pi, "powheg.input")) as f:
                                    content = f.read()
                                    matches = dict((name, re.search(r"^"+name+" *([0-9]+)", content, flags=re.MULTILINE)) for name in desiredvalues)
                            if et_flag == 1:
                                with open(os.path.join(my_path, pi, "external_tarball/powheg.input")) as f:
                                    content = f.read()
                                    matches = dict((name, re.search(r"^"+name+" *([0-9]+)", content, flags=re.MULTILINE)) for name in desiredvalues)                                
                            bad = False
                            for name, match in matches.iteritems():
                                if match:
                                    actualvalue = int(match.group(1))
                                    if actualvalue < desiredvalues[name]:
                                        bad = True
                                        print "* [WARNING] {0} = {1}, should be at least {2} (may be ok if hmass < 150 GeV, please check!)".format(name, actualvalue, desiredvalues[name])
                                        warning = warning + 1
                                else:
                                    bad = True
                                    print "* [ERROR] didn't find "+name+" in powheg.input"
                                    error = error + 1
                            if not bad:
                                print "* [OK] integration grid setup looks ok for gg_H_quark-mass-effects"
                    else:
                        print "* [WARNING] Didn't find powheg process in runcmsgrid.sh"
                        warning = warning + 1

                if ind > 0:
                    if gp_size == 0:
                        break
                    filename = my_path+'/'+pi+'/'+'process/madevent/Cards/proc_card_mg5.dat'
                    fname_p2 = my_path+'/'+pi+'/'+'process/Cards/proc_card.dat'
                    fname_p3 = my_path+'/'+pi+'/'+'process/Cards/proc_card_mg5.dat'
                    if os.path.isfile(fname_p2) is True :
                        filename = fname_p2
                    if os.path.isfile(fname_p3) is True :
                        filename = fname_p3
                    if os.path.isfile(filename) is True :
                        loop_flag = int(os.popen('more '+filename+' | grep -c "noborn=QCD"').read())
                        gen_line = os.popen('grep generate '+filename).read()
                        print(gen_line)
                        proc_line = os.popen('grep process '+filename).read()
                        print(proc_line)
                        if gen_line.count('@') <= proc_line.count('@'):
                            nproc = proc_line.count('@')
                            nproc = '@'+str(nproc)
#                            proc_line = proc_line.split('\n')
			    proc_line = proc_line.split('add process')	
#                            jet_line = next((s for s in proc_line if nproc in s), None).replace('add process','')
			    jet_line = proc_line[len(proc_line)-1]
			    jet_line_arr = jet_line.split(',')	
			    for x in range(0,len(jet_line_arr)):
#			        jet_count_tmp.append(jet_line_arr[x].count('j') + jet_line_arr[x].count('b') + jet_line_arr[x].count('c'))	 
                                nbtomatch = jet_line_arr[x].count('b') if maxjetflavor > 4 else 0
                                nc = jet_line_arr[x].count('c') if "chi" not in jet_line_arr[x] else 0
			        jet_count_tmp.append(jet_line_arr[x].count('j') + nbtomatch + nc)	 
		            jet_count = max(jet_count_tmp)
                        else :
                            jet_line = gen_line.replace('generate','')
                            jet_count = jet_line.count('j') + jet_line.count('b') + jet_line.count('c')
                        if nJetMax == jet_count:
                            print "* [OK] nJetMax(="+str(nJetMax) + ") is equal to the number of jets in the process(="+str(jet_count)+")"
#                        if nJetMax != jet_count and str(jet_count)+"jet" not in dn.lower() and gen_line.count('@') != 0 and matching_c !=0:
                        if nJetMax != jet_count and gen_line.count('@') != 0 and matching_c !=0:
                            print "* [WARNING] nJetMax(="+str(nJetMax)+") is NOT equal to the number of jets specified in the proc card(="+str(jet_count)+")"
                            warning = warning + 1
                        if nJetMax != jet_count and str(jet_count)+"jet" in dn.lower() and matching_c !=0:
                            print "* [WARNING] nJetMax(="+str(nJetMax)+") is not equal to the number of jets specified in the proc card(="+str(jet_count)+")."
                            print "*           Is it because this is an exclusive production with additional samples with higher multiplicity generated separately?"
                            warning = warning + 1                         
                    fname = my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'
                    fname2 = my_path+'/'+pi+'/'+'process/Cards/run_card.dat'
                    if os.path.isfile(fname) is True :
                       ickkw = os.popen('more '+fname+' | tr -s \' \' | grep "= ickkw"').read()
                       bw = os.popen('more '+fname+' | tr -s \' \' | grep "= bwcutoff"').read()
                    elif os.path.isfile(fname2) is True :    
                       ickkw = os.popen('more '+fname2+' | tr -s \' \' | grep "= ickkw"').read()
                       bw = os.popen('more '+fname2+' | tr -s \' \' | grep "= bwcutoff"').read()
                    else:
                        if gp_size != 0:
                            print "* [ERROR] Although the name of the dataset has ~Madgraph, the gridpack doesn't seem to be a MG5_aMC one. Please check."
                            error = error + 1
                            break                            
                    version_file = my_path+'/'+pi+'/'+'mgbasedir/VERSION'
                    if os.path.isfile(version_file) is True:
                        mgversion_tmp = os.popen('grep version '+version_file).read()
                        mgversion = mgversion_tmp.split()
                        mgversion = mgversion[2].split(".")
                        mgversion_tmp = mgversion_tmp.split("\n")
                        if "UL" in pi and int(mgversion[0]) <= 2 and int(mgversion[1]) < 6:
                            print"* [WARNING] You're using MG5_aMC "+str(mgversion_tmp[0])+" in an Ultra Legacy Campaign. Are you sure that's what you want?"
                            warning = warning + 1
                    test_bw = bw.split() 
                    if float(test_bw[0]) > 15.:
                        print " [WARNING] bwcutoff set to "+str(test_bw[0])+". Note that large bwcutoff values can cause problems in production."
                        warning = warning + 1
                    matching = int(re.search(r'\d+',ickkw).group())
                    ickkw = str(ickkw)  
                    if matching == 1 or matching == 2:
                        if match_eff == 1:
                            print "* [WARNING] Matched sample but matching efficiency is 1!"
                            warning = warning + 1
                    if ind < 2:
                        MGpatch.append(int(os.popen('more '+my_path+'/'+pi+'/'+'runcmsgrid.sh | grep -c "FORCE IT TO"').read()))
                        MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'mgbasedir/Template/LO/SubProcesses/refine.sh').read()))
                        MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'process/madevent/SubProcesses/refine.sh').read()))
                        if MGpatch[0] == 1 and MGpatch[1] == 1 and MGpatch[2] == 1:
                            print "*"
                            print "* [OK] MG5_aMC@NLO leading order patches OK in gridpack"
                        if MGpatch[0] != 1:
                            print "* [ERROR] MG5_aMC@NLO multi-run patch missing in gridpack - please re-create a gridpack"
                            print "*            using updated genproductions area"
                            error = error + 1
                        if MGpatch[1] == 0 or MGpatch[2] == 0:
                            if '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw :
                                print "* [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                                print "* And the request is using a version "+str(cmssw)+" that does not contain the patch."
                                print "* Please use >= 7_1_32_patch1 or CMSSW_9_3_9_patch1 or 10_2_0_pre2"
                                error = error + 1 
                            elif '7_1' in cmssw:
                                test_version = cmssw.split('_')
                                if (len(test_version) == 4 and int(test_version[3]) < 33) or (len(test_version) == 5 and (int(test_version[3]) < 32 or (int(test_version[3]) == 32 and "patch1" not in cmssw))):
                                    print "* [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                                    print "* And the request is using a version "+str(cmssw)+" that does not contain the patch."
                                    print "* In this release, please at least use CMSSW_7_1_32_patch1"
                                    error = error + 1
                            elif '9_3' in cmssw:
                                test_version = cmssw.split('_')
                                if (len(test_version) == 4 and int(test_version[3]) < 10) or (len(test_version) == 5 and (int(test_version[3]) < 9 or (int(test_version[3]) == 9 and "patch1" not in cmssw))):
                                    print "* [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                                    print "* And the request is using a version "+str(cmssw)+" that does not contain the patch."
                                    print "* In this release, please at least use CMSSW_9_3_9_patch1"
                                    error = error + 1
                            elif '10_2' in cmssw:
                                test_version = cmssw.split('_')
                                if len(test_version) == 4 and int(test_version[3]) < 1:
                                    print "* [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                                    print "* And the request is using a version "+str(cmssw)+" that does not contain the patch."
                                    print "* In this release, please at least use CMSSW_10_2_0_pre2"
                                    error = error + 1
                        print "*"    
                        print "-------------------------MG5_aMC LO/MLM Many Threads Patch Check --------------------------------------"   
                        ppp_ind_range = 0
                        if slha_flag == 1:
                            slha_file_list =  os.listdir(slha_all_path)
                            ppp_ind_range = len(slha_file_list)
                        if slha_flag == 0:
                            ppp_ind_range = 1
                        for ppp in range(0,ppp_ind_range):
                            if gp_size == 0:
                                break
                            del MGpatch2[:]
                            if slha_flag == 1:
                                gridpack_cvmfs_path_tmp = slha_all_path+'/'+slha_file_list[ppp]
                                if "runmode0_TEST" in gridpack_cvmfs_path_tmp:
                                    continue
                                gridpack_cvmfs_path = gridpack_cvmfs_path_tmp
                                gridpack_eos_path = gridpack_cvmfs_path_tmp.replace("/cvmfs/cms.cern.ch/phys_generator","/eos/cms/store/group/phys_generator/cvmfs")
                            os.system('tar xf '+gridpack_eos_path+' -C '+my_path+'/eos/'+pi)
                            MGpatch2.append(int(os.popen('more '+my_path+'/'+pi+'/'+'runcmsgrid.sh | grep -c "To overcome problem of taking toomanythreads"').read()))
                            MGpatch2.append(int(os.popen('more '+my_path+'/eos/'+pi+'/'+'runcmsgrid.sh | grep -c "To overcome problem of taking toomanythreads"').read()))
                            if MGpatch2[1] == 1:
                                print "* [OK] MG5_aMC@NLO LO nthreads patch OK in EOS"
                            if MGpatch2[0] == 1:
                                print "* [OK] MG5_aMC@NLO LO nthreads patch OK in CVMFS"
                            if MGpatch2[0] == 0 and MGpatch2[1] == 1:
                                print "* [OK] MG5_aMC@NLO LO nthreads patch not made in CVMFS but done in EOS waiting for CVMFS-EOS synch"
                            if MGpatch2[1] == 0:
                                print "* [ERROR] MG5_aMC@NLO LO nthreads patch not made in EOS"
                                error = error + 1
                                if args.apply_many_threads_patch:
                                    print "Patching for nthreads problem... please be patient."
                                    if slha_flag == 0: 
                                        os.system('python ../../Utilities/scripts/update_gridpacks_mg242_thread.py --prepid '+pi)
                                    if slha_flag == 1:
                                        os.system('python ../../Utilities/scripts/update_gridpacks_mg242_thread.py --gridpack '+gridpack_cvmfs_path)
                            print "-------------------------EOF MG5_aMC LO/MLM Many Threads Patch Check ----------------------------------"
                            print "*"
                if matching >= 2 and check[0] == 2 and check[1] == 1 and check[2] == 1 :
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    if matching > 3 and os.path.isfile(file_pwg_check) is False :
                        print "* [WARNING] To check manually - This is a Powheg NLO sample. Please check 'nFinal' is"
                        print "*               set correctly as number of final state particles (BEFORE THE DECAYS)"
                        print "*                                   in the LHE other than emitted extra parton."
                        warning = warning + 1
                elif matching == 1 and check[0] == 0 and check[1] == 0 and check[2] == 0 :    
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    if matching_c != 0:
                        print "* [WARNING] To check manually - This is a matched MadGraph LO sample. Please check 'JetMatching:nJetMax' ="+str(nJetMax)+" is OK and"
                        print "*            correctly set as number of partons in born matrix element for highest multiplicity."
                        warning = warning + 1
                elif matching == 0 and word == "madgraph" and check[0] == 0 and check[1] == 0 and check[2] == 0 :
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                elif matching == 0 and word == "mcatnlo" and check[0] == 2 and check[1] == 1 and check[2] == 1 and loop_flag != 1:
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    if matching_c == 0:
                        print "* [WARNING] This a MadGraph NLO sample without matching. Please check 'TimeShower:nPartonsInBorn'"
                        print "*                                                   is set correctly as number of coloured particles"
                        print "*                                                  (before resonance decays) in born matrix element."
                        warning = warning + 1	
                else:     
                    if word != "powheg":
                        print "* [ERROR] Fragment may be wrong: check "+word+" settings in the fragment"
                        error = error + 1
                    if matching <= 1 and word == "madgraph":
                        print "* [ERROR] You run MG5_aMC@NLO at LO but you have  Pythia8aMCatNLOSettings_cfi in fragment"
                        print "*           --> please remove it from the fragment"
                        error = error + 1
                    if word == "powheg" :
                        print "* [WARNING] if this is a "+word+" request but loop induced process such as gg->ZH," 
                        print "*           then fragment is OK (no need to have Pythia8PowhegEmissionVetoSettings)"
			warning = warning + 1
        if knd == 1 :
             powhegcheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
             if powhegcheck[0] > 0 :
                 print "* [ERROR] Please remove POWHEG settings for MG requests."
                 error = error + 1
        if knd == -1 :
             purepythiacheck.append(int(os.popen('grep -c -i Pythia8aMCatNLOSettings '+pi).read()))
             purepythiacheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
             if purepythiacheck[0] > 0 or purepythiacheck[1] >0 :
                 print "* [WARNING] Please remove aMCatNLO or POWHEG settings if this is a pure Pythia request."
                 print "*           If it's not a pure request, in the future, please include madgraph/powheg or amcatnlo"
                 print "*           in the name of the dataset"
                 warning = warning + 1
        if loop_flag == 1:
            if mcatnlo_flag == 1: 
                print "* [ERROR] You are using a loop induced process, [noborn=QCD]."
                print "*         Please remove all occurances of Pythia8aMCatNLOSettings from the fragment"
                error = error + 1
            if nPartonsInBorn_flag == 1:
                print "* [ERROR] You are using a loop induced process, [noborn=QCD]."
                print "*         Please remove all TimeShower:nPartonsInBorn from the fragment"       
                error = error + 1
        for kk in range (0, 8):   
            tunecheck.append(int(os.popen('grep -v "#" '+pi+' | grep -v "annotation" | grep -v "name" | grep -c -i '+tune[kk]).read()))
        if tunecheck[6] == 3 or tunecheck[7] == 3:
            if tunecheck[0] != 3:
                print "* [WARNING] Check if there is some extra tune setting"
                warning = warning + 1
        if 'sherpa' in dn.lower():
            print "* [WARNING] No automated check of Sherpa ps/tune parameters yet"
            warning = warning + 1
        if 3 not in tunecheck and herwig_flag == 0:
            with open(pi) as f:
                tot = f.read()
                n_ext_par += tot.count('MultipartonInteractions')
                n_ext_par += tot.count('ColourReconnection')
                n_ext_par += tot.count('SpaceShower')
                n_ext_par += tot.count('TimeShower')
                n_ext_par += tot.count('reweightGenEmp')
                print "* [WARNING] Number of extra or replaced tune parameters is at least "+str(n_ext_par)
                print "*           Please check tune configuration carefully (e.g. are the non-replaced parameters the ones you want)"
                warning = warning + 1
        if 3 not in tunecheck and 'sherpa' not in dn.lower() and fsize != 0 and n_ext_par == 0 and herwig_flag == 0:
            if  any(tunecheck[0]<3 and it!=0 for it in tunecheck) :
              	print tunecheck
              	print "* [ERROR] Tune configuration may be wrong in the fragment"
 	    	print "*         or pythia8CUEP8M1Settings are overwritten by some other parameters as in CUETP8M2T4"
            	error = error + 1
	    else :
		print "* [WARNING] None standard tune - please check the fragment carefully."
		warning = warning + 1	
        elif 3 in tunecheck:
            print "* [OK] Tune configuration probably OK in the fragment"
            if tunecheck[0] > 2 :
                if 'Summer19UL' not in pi and 'Fall18' not in pi and 'Fall17' not in pi and 'Run3' not in pi:
                    print "* [WARNING] Do you really want to have tune "+tune[0] +" in this campaign?"
                    warning = warning + 1
        if 'Fall18' in pi and fsize != 0 and herwig_flag == 0:
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) != 1 :
                print "* [WARNING] No parton shower weights configuration in the fragment. In the Fall18 campaign, we recommend to include Parton Shower weights"
                warning = warning + 1
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) == 1 :
                cmssw_version    = int(re.search("_[0-9]?[0-9]_[0-9]?[0-9]_[0-9]?[0-9]",cmssw).group().replace('_',''))
                if cmssw_version < int('10_2_3'.replace('_','')) :
                    print "* [ERROR] PS weights in config but CMSSW version is < 10_2_3 - please check!"
                    error = error + 1
                psweightscheck.append(int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettingsBlock," '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettings" '+pi).read()))
                if psweightscheck[0] == 1 and psweightscheck[1] == 1 and psweightscheck[2] == 2 :
                    print "* [OK] Parton shower weight configuration probably OK in the fragment"
                else:
                    print "* [ERROR] Parton shower weight configuration not OK in the fragment" 
                    error = error + 1
        if int(os.popen('grep -c -i filter '+pi).read()) > 3 and filter_eff == 1:
            print "* [WARNING] Filters in the fragment but filter efficiency = 1"
            warning = warning + 1
        os.popen("rm -rf "+my_path+pi).read()  
        os.popen("rm -rf "+my_path+'eos/'+pi).read()
        print "***********************************************************************************"
        print "Number of warnings = "+ str(warning)
        print "Number of errors = "+ str(error)
        if error > 0:
            print "There is at least 1 error. Request won't proceed to VALIDATION"

# Valid range for exit codes is 0-255
        if error > 255 or error < 0:
            error = 255

# Exit with code, 0 - good, not 0 is bad
        if args.bypass_validation:
            continue
        else:    
            sys.exit(error)
