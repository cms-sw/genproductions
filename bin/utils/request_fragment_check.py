#!/bin/env python2
import os
import sys
import re
import argparse
import textwrap
import fnmatch
#import json
from datetime import datetime
###########Needed to check for ultra-legacy sample consistency check############################################
os.system('cern-get-sso-cookie -u https://cms-pdmv.cern.ch/mcm/ -o cookiefile.txt --krb --reprocess')
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
                  * [WARNING] comphep or calchep request. Please check manually
                  * [WARNING] Large time/event (> 150 sec)=xx - please check
                  * [WARNING] if CMSSW version is not 10_6, 10_2, 9_3, and 7_1 or (8_0 but not Summer16FSPremix) or (9_4 but not Fall17FSPremix)
                  *           Are you sure you want to use a release which is not standard which may not have all the necessary GEN code.
                  * [WARNING] total number of events > 100000000 - Is xx events what you really wanted - please check!
                  * [WARNING] CMSSW version is different than its base UL17 request.
                  *           Please make sure that the request has _exactly_ the same settings as the base request.
                  * [WARNING] No fragment associated to this request"
                  *           is this the hadronizer you intended to use?
                  * [WARNING] Not checking sherpacks for now.
                  * [WARNING] PDF:pSet is missing (if you want to use NNPDF3.1)
                  *           For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1
                  *           PDF access method should be like"
                  *           e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'
                  * [WARNING] PDF access method is wrong (if you want to use NNPDF3.1). Please correct:
                  *           e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'"
                  * [WARNING] Dataset name is not regular:
                  *           Please add the Generator name to the dataset.
                  * [WARNING] gridpack patch problem
                  *           You may try to request more events per phase-space region in the gridpack.
                  * [WARNING] The gridpack uses PDF = xx but not the recommended sets for UL requests
                  * [WARNING] Didn't find powheg process in runcmsgrid.sh
                  * [WARNING] nJetMax(=X) is not equal to the number of jets specified in the proc card(=Y).
                  *            Is it because this is an exclusive production with additional samples with higher multiplicity generated separately?
                  * [WARNING] To check manually - This is a matched MadGraph LO sample. Please check 'JetMatching:nJetMax' =X is OK and
                  *            correctly set as number of partons in born matrix element for highest multiplicity.
                  * [WARNING] This a MadGraph NLO sample without matching. Please check 'TimeShower:nPartonsInBorn'
                  *                                                   is set correctly as number of coloured particles
                  *                                                  (before resonance decays) in born matrix element.
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
                  * [WARNING] There may be a problem with scale variations. Please check pwg-rwl.dat
                  * [WARNING] There may be a problem with PDF variations. Please check pwg-rwl.dat
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
                  * [WARNING] bornonly = xx (if xx = 1 and if (Pythia8PowhegEmissionVetoSettings or SpaceShower:pTmaxMatch
                  *           or  TimeShower:pTmaxMatch) this becomes an error.
                  * [ERROR]   parton_shower in  mg5_aMC NLO configurations is not consistent with PS (HERWIGX or PYTHIA8)
                  * [WARNING] You're using MG5_aMC xx in an Ultra Legacy Campaign. You should use MG5_aMCv2.6.1+.
                  *           This becomes an error if it is not a PPD request.
                  * [WARNING] There will be no PDF variations! Please check the runcmsgrid file in the gridpack.
                  * [WARNING] pdfsets in runcmsgrid file does not contain one of the recommended sets: xxxx
                  *           Your runcmsgrid file contains these sets: xxxx
                  * [WARNING] Main pdf recommended set xx or yy is listed in runcmsgrid file but it is
                  *             also included as a variation??
                  * [WARNING] Are you sure EvtGenExtraParticles extension is not needed?
                  * [WARNING] Are you sure you do not want the 2014 evt pdl table?
                  * [WARNING] Are you sure you do not want the 2014 decay dec table?

               ERRORS:
                  * [ERROR] No corresponing UL17 request to compare to for consistency.
                  *         Please first create the corresponding UL17 requests.
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
                            and timeperevent > 0 and not a ppd request (this is a CMSSW version dependent check)
                  * [ERROR] 8 core request with memory different from 15900 GB. Please set the memory to 15900 GB if CMSSW version >= 10_6_X and nthreads = 8
                            and mem != 15900 and not ppd request
                  * [ERROR] Memory is not <=2300,  or =4000 or =15900 MB
                  * [ERROR] Memory is <=2300 MB while number of cores is XX but not = 1
                  * [ERROR] Memory is 4000 MB while number of cores is 1 but not = 2,4 or 8
                  * [ERROR] Memory is 15900 MB while number of cores is not 8 and 16
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign but Memory is not 14700, 5900, 400, or <=2300 MB
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 14700 but nthreads != 8
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 5900 but nthreads != 4
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 4000 but nthreads != 2
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is <=2300 but nthreads != 1
                  * [ERROR] Gridpack should have used cvmfs path instead of eos path
                  * [ERROR] minbias in version >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_X and not particle gun and not CMSSW_10_6_0 and not CMSSW_10_6_0_patch1:
                  *         SigmaTotal:mode should have been set to 0
                  *         SigmaTotal:sigmaEl should have been set to 21.89
                  *         SigmaTotal:sigmaTot should have been set to 100.309
                  *         PDF access method is wrong. Please correct.
                  * [ERROR] SigmaTotal:mode is missing
                  *         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1
                  *         SigmaTotal:mode shoud be added by hand and set to 0
                  * [ERROR] SigmaTotal:sigmaEl is missing"
                  *         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1
                  *         SigmaTotal:sigmaEl should be added by hand and set to 21.89
                  * [ERROR] SigmaTotal:sigmaTot is missing"
                  *         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1
                  *         SigmaTotal:sigmaTot should be added by hand and set to 100.309
                  * [ERROR] Dataset name does not have the tune name
                  *         Please add the tune name to the dataset.
                  * [ERROR] Dataset name does not contain a parton shower code name
                  *         Please add the parton shower name to the dataset name.
                  * [ERROR] HERWIGPP = parton_shower not in run_card.dat
                  * [ERROR] Although the name of the dataset has ~Madgraph, the gridpack doesn't seem to be a MG5_aMC one. Please check.
                  * [ERROR] Please add \'set FxFxHandler:MergeMode FxFx\'
	              *         and set FxFxHandler:njetsmax to the number of additional partons in the proc_card
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
                  * [ERROR] Tune configuration may be wrong in the Fragment
                  *          or pythia8CUEP8M1Settings are overwritten by some other parameters as in CUETP8M2T4
                  * [ERROR] PS weights in config but CMSSW version is < 10_2_3 - please check!
                  * [ERROR] Parton shower weight configuration not OK in the fragment
                  * [ERROR] You're using MG5_aMC version < 2.6.1 in an Ultra Legacy Campaign. You should use MG5_aMCv2.6.1+
                  * [ERROR] bornonly = 1 and (Pythia8PowhegEmissionVetoSettings or SpaceShower:pTmaxMatch or  TimeShower:pTmaxMatch)
                  * [ERROR] something's wrong - LO and NLO configs together.
                  * [ERROR] store_rwgt_info set to false for MG5_aMC >= 260.
                  * [ERROR] No store_rwgt_info set for MG5_aMC >= 260.
                  * [ERROR] use_syst set to false for MG5_aMC >= 260.
                  * [ERROR] No use_syst set for MG5_aMC >= 260.
                  * [ERROR] evtgen flag defined in dataset name but not set within the fragment
                  * [ERROR] EvtGenInterface not definied within the fragment
                  * [ERROR] Turn off the PythiaCodes conversion when using EvtGen
                  * [ERROR] EvtGen settings within fragment but no evtgen flag at dataset name
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

def find_file(dir_path,patt):
    for root, dirs, files in os.walk(dir_path):
        for file in files:
            if file.endswith(patt):
                return root+'/'+str(file)

def xml_check_and_patch(f,cont,gridpack_eos_path,my_path,pi):
    xml = str(re.findall('xmllint.*',cont))
    cur_dir = os.getcwd()
    warning_xml = 0
    error_xml = 0
    if "stream" not in xml or len(xml) < 3:
	targz_flag = 0
	if "stream" not in xml and len(xml) > 3:
	  print "* [WARNING] --stream option is missing in XMLLINT, will update runcmsgrid."
	  warning_xml += 1
        if len(xml) < 3:
	  print "* [WARNING] XMLLINT does not exist in runcmsgrid, will update it."
          warning_xml += 1
	if ".tar.gz" in gridpack_eos_path:
	  targz_flag = 1
	  gridpack_eos_path_backup = gridpack_eos_path.replace('.tar.gz','_original.tar.gz')
	if ".tgz" in gridpack_eos_path:
	  gridpack_eos_path_backup = gridpack_eos_path.replace('.tgz','_original.tgz')
    	if ".tar.xz" in gridpack_eos_path:
      	  gridpack_eos_path_backup = gridpack_eos_path.replace('.tar.xz','_original.tar.xz')
	  targz_flag = 2
	if not os.path.exists(gridpack_eos_path_backup):
	  print "* Backup gridpack is not existing."
	  print "* Copying "+gridpack_eos_path+" to "+gridpack_eos_path_backup+" before patching runcms.grid"
	  os.system('cp -n -p '+gridpack_eos_path+' '+gridpack_eos_path_backup)
	  md5_1 = os.popen('md5sum'+' '+gridpack_eos_path).read().split(' ')[0]
	  md5_2 = os.popen('md5sum'+' '+gridpack_eos_path_backup).read().split(' ')[0]
	  if md5_1 == md5_2:
	    print "* Backup and original file checksums are equal."
	  else:
	    print "* [ERROR] backup gridpack has a problem."
	    error_xml += 1
	print "* Updating XMLLINT line in runcmsgrid."
        os.chdir(my_path+'/'+pi)
	if "stream" not in xml and len(xml) > 3:
	  cont = re.sub("xmllint","xmllint --stream",cont)
	if len(xml) < 3:
          newlinetoadd = 'xmllint --stream --noout ${file}_final.lhe > /dev/null 2>&1; test $? -eq 0 || fail_exit "xmllint --stream integrity check failed on pwgevents.lhe" \ncp ${file}_final.lhe ${WORKDIR}/.'
          string_orig = "cp \$\{file\}\_final.lhe \$\{WORKDIR\}\/\."
          cont = re.sub(string_orig,newlinetoadd,cont)
	f.seek(0)
	f.write(cont)
	f.truncate()
    	if targz_flag == 0:
	  gridpackname = "gridpack.tgz"
	if targz_flag == 1:
	  gridpackname = "gridpack.tar.gz"
        if targz_flag == 2:
	  gridpackname = "gridpack.tar.xz"
        os.chdir(my_path+'/'+pi)
	os.system('tar cfJ '+gridpackname+' ./* --exclude='+gridpackname+' --exclude='+pi)
	os.system('cp '+gridpackname+' '+gridpack_eos_path)
	md5_1 = os.popen('md5sum '+gridpackname).read().split(' ')[0]
	md5_2 = os.popen('md5sum'+' '+gridpack_eos_path).read().split(' ')[0]
	if md5_1 == md5_2:
	  print "* Updated gridpack copied succesfully."
	else:
	  print "* [ERROR] there was a problem copying in the updated gridpack to eos."
	  error_xml += 1
	os.chdir(cur_dir)
    return warning_xml,error_xml


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
        print "***************************************************************************************"
        print "Something's wrong - can not get the request parameters - check that the request exists."
        print "***************************************************************************************"
        continue

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
        mg5_aMC_version = 0
        mem = r['memory']
        filter_eff = r['generator_parameters'][-1]['filter_efficiency']
        match_eff = r['generator_parameters'][-1]['match_efficiency']
	ext = r['extension']
        print pi+"    Status= "+r['status']
        print dn
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
        MEname = ["powheg","madgraph","mcatnlo","jhugen","mcfm","sherpa"]
        tune = ["CP5","CUEP8M1","CP1","CP2","CP3","CP4","CP5TuneUp","CP5TuneDown"]
        tunename = ["CP5","CUETP8M1","CUETP8M2T4","CP1","CP2","CP3","CP4","CP5TuneUp","CP5TuneDown","CH3"]
        psname = ["pythia","herwig","sherpa"]
        UL_PDFs= ["NNPDF31_nnlo_as_0118_mc_hessian_pdfas","NNPDF31_nnlo_as_0118_nf_4_mc_hessian"]
        UL_PDFs_N = [325300,325500]
        n_ext_par = 0
        mg_lo = 0
        mg_lo_w_matching = 0
        mg_nlo = 0
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
        et_flag_external = 0
        bornonly = 0
        herwig_flag = 0
        herwig_count = []
        herwig7_bypass_error = 0
        pythia8_flag = 0
        evtgen_flag = 0
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
        if "evtgen" in dn.lower():
            evtgen_flag = 1
        if "comphep" in dn.lower() or "calchep" in dn.lower():
            print "* [WARNING] comphep or calchep request. Please check manually"
            warning += 1
            continue
        for item in te:
            timeperevent = float(item)
        if timeperevent > 150.0 :
            print "* [WARNING] Large time/event (> 150 sec)="+str(timeperevent)+" - please check"
            warning += 1
        version_not_ok = 0
        if '8_0' in cmssw and "Summer16FSPremix" not in pi:
            version_not_ok = 1
        if '9_4' in cmssw and "Fall17FSPremix" not in pi:
            version_not_ok = 1
        if '10_6' not in cmssw and '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw and version_not_ok == 1:
            print "* [WARNING] Are you sure you want to use "+cmssw+" release which is not standard"
            print "*           which may not have all the necessary GEN code."
            warning += 1
        if totalevents >= 100000000 :
            print "* [WARNING] Is "+str(totalevents)+" events what you really wanted - please check!"
            warning += 1
        os.popen('wget -q '+mcm_link+'public/restapi/requests/get_fragment/'+pi+' -O '+pi).read()
        fsize = os.path.getsize(pi)
        f1 = open(pi,"r")
        f2 = open(pi+"_tmp","w")
        data_f1 = f1.read()
        data_f2 = re.sub(r'(?m)^ *#.*\n?', '',data_f1)
        # Ultra-legacy sample settings' compatibility
        pi_prime = "NULL"
        if "Summer19UL16" in pi or "Summer19UL18" in pi:
            prime = get_requests_from_datasetname(dn)
            if len(prime) == 0:
                print "* [ERROR] No corresponing UL17 request to compare to for consistency."
                print "*         Please first create the corresponding UL17 requests."
                error = error + 1
            if len(prime) != 0:
                for rr in prime:
                    if "UL17" in rr['prepid'] and "GEN" in rr['prepid'] and ext == rr['extension']:
                        pi_prime = rr['prepid']
                        cmssw_prime = rr['cmssw_release']
            if "NULL" in pi_prime:
	       print "* [ERROR] No corresponing UL17 request to compare to for consistency."
               print "*         Please first create the corresponding UL17 requests."
	       error = error + 1
	    else:
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
                  error += 1
               if (cmssw == cmssw_prime) == True:
                  print"[OK] Two requests have the same CMSSW version."
               else:
                  print"[WARNING] CMSSW version of "+pi+" is different than its base UL17 request: "+pi_prime
                  print"        Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime
                  warning += 1
                  f1_prime.close()
                  f2_prime.write(data_f2_prime)
                  f2_prime.close()
		  sys.exit()
        f1.close()
        f2.write(data_f2)
        f2.close()
        os.system('mkdir -p '+my_path+'/'+pi)
        os.system('mkdir -p '+my_path+'/eos/'+pi)
        os.system('mv '+pi+'_tmp '+pi)
        os.system('cp '+pi+' '+my_path+'/'+pi+'/.')
        os.system('wget -q '+mcm_link+'public/restapi/requests/get_test/'+pi+' -O '+pi+'_get_test')
        gettest = os.popen('grep cff '+pi+'_get_test'+' | grep curl').read()
        scram_arch = os.popen('grep SCRAM_ARCH '+pi+'_get_test').read()
        scram_arch = scram_arch.split('=')[1].rstrip()
        print "##################################################"
        print "* CMSSW release for the request: "+str(cmssw)
        print "* scram_arch = "+str(scram_arch)
        ps_version = "/cvmfs/cms.cern.ch/"+str(scram_arch)+"/cms/cmssw"
#        pythia8_version = "/cvmfs/cms.cern.ch/"+str(scram_arch)+"/cms/cmssw"
        if "patch" in cmssw:
            ps_version = ps_version + "-patch"
        if "pythia8" in dn.lower():
            pythia8_flag = 1
            pythia8_version = ps_version + "/"+str(cmssw)+"/config/toolbox/"+str(scram_arch)+"/tools/selected/pythia8.xml"
            pythia8_version_file = os.path.isfile(pythia8_version)
            pythia8_version = "grep version "+pythia8_version
            if pythia8_version_file is True:
                pythia8_version = os.popen(pythia8_version).read().rstrip().split('=')[2].replace(">","")
                print "* PYTHIA8 version = "+str(pythia8_version)
        if "herwig" in dn.lower():
            herwig_version = ps_version + "/"+str(cmssw)+"/config/toolbox/"+str(scram_arch)+"/tools/selected/herwigpp.xml"
            herwig_version_file = os.path.isfile(herwig_version)
            herwig_version = "grep version "+herwig_version
            if herwig_version_file is True:
                herwig_version = os.popen(herwig_version).read().rstrip().split('=')[2].replace(">","")
                print "* Herwig version = "+str(herwig_version)
        if "evtgen" in dn.lower():
            evtgen_version = ps_version + "/"+str(cmssw)+"/config/toolbox/"+str(scram_arch)+"/tools/selected/evtgen.xml"
            evtgen_version_file = os.path.isfile(evtgen_version)
            evtgen_version = "grep version "+evtgen_version
	    photos_version = ps_version + "/"+str(cmssw)+"/config/toolbox/"+str(scram_arch)+"/tools/selected/photospp.xml"
	    photos_version_file = os.path.isfile(photos_version)
	    photos_version =  "grep version "+photos_version
            if evtgen_version_file is True:
                evtgen_version = os.popen(evtgen_version).read().rstrip().split('=')[2].replace(">","")
                print "* EvtGen version = "+str(evtgen_version)
	    if photos_version_file is True:
		photos_version = os.popen(photos_version).read().rstrip().split('=')[2].replace(">","")
		print "* PHOTOS version = "+str(photos_version)
        print "##################################################"
        if herwig_flag != 0:
            os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_frag_lines.txt -O herwig_frag_lines.txt')
            file1 = set(line.strip().replace(",","")  for line in open('herwig_frag_lines.txt'))
            file2 = set(line.strip().replace(",","") for line in open(pi))
            herwig_check = []
            herwig_mat_err = 0
            for line in file1:
                if line not in file2:
                    herwig_check.append(line)
            if len(herwig_check) != 0 and "eec5" not in dn.lower() and "ee5c" not in dn.lower():
                herwig_count.append(herwig_check[0].count('hw_lhe_common_settings'))
                herwig_count.append(herwig_check[1].count('herwig7LHECommonSettingsBlock'))
                herwig_count.append(herwig_check[2].count('from Configuration.Generator.Herwig7Settings.Herwig7LHECommonSettings_cfi import *'))
                if all(x == 1 for x in herwig_count) and any("insert SubProcess:MatrixElements" in x for x in list(file2)):
                    herwig7_bypass_error = 1
                if herwig7_bypass_error == 0:
                    print "* [ERROR] "+ str(len(herwig_check)) + " missing fragment line(s) for herwig:"
                    print "*          lines for internal matrix element are missing in the fragment."
                    print herwig_check
                    error = error + len(herwig_check)
            if "powheg" in dn.lower():
                if int(os.popen('grep -c Herwig7LHEPowhegSettings_cfi '+pi).read()) == 0:
                    print "* [ERROR] Herwig7LHEPowhegSettings_cfi should be loaded in the fragment"
                    error += 1
                    herwig_mat_err = 1
                if int(os.popen('grep -c herwig7LHEPowhegSettingsBlock '+pi).read()) == 0:
                    print "* [ERROR] herwig7LHEPowhegSettingsBlock missing for powheg+herwig7 request"
                    error += 1
                    herwig_mat_err = 1
                if int(os.popen('grep -c hw_lhe_Powheg_settings '+pi).read()) == 0:
                    print "* [ERROR] hw_lhe_Powheg_settings missing for powheg+herwig7 request"
                    error += 1
                    herwig_mat_err = 1
            if "mcatnlo" in dn.lower():
                if int(os.popen('grep -c Herwig7LHEMG5aMCatNLOSettings_cfi '+pi).read()) == 0:
                    print "* [ERROR] Herwig7LHEMG5aMCatNLOSettings_cfi should be loaded in the fragment"
                    error += 1
                    herwig_mat_err = 1
                if int(os.popen('grep -c herwig7LHEMG5aMCatNLOSettingsBlock '+pi).read()) == 0:
                    print "* [ERROR] herwig7LHEMG5aMCatNLOSettingsBlock missing for MG5_aMC[NLO]+herwig7 request"
                    error += 1
                    herwig_mat_err = 1
                if int(os.popen('grep -c hw_lhe_MG5aMCatNLO_settings '+pi).read()) == 0:
                    print "* [ERROR] hw_lhe_MG5aMCatNLO_settings missing for MG5_aMC[NLO]+herwig7 request"
                    error += 1
                    herwig_mat_err = 1
            if herwig_mat_err == 0 and len(herwig_check) == 0:
                print "*"
                print "* [OK] Herwig7 fragment probably OK"
                print "*"
        if fsize == 0:
            print "* [WARNING] No fragment associated to this request"
            print "*           is this the hadronizer you intended to use?: "+gettest
            warning += 1
        ttxt = os.popen('grep nThreads '+pi+'_get_test').read()
        if int(os.popen('grep -c nThreads '+pi+'_get_test').read()) == 0 :
            nthreads = 1
        else :
            nthreads = int(re.search('nThreads(.*?) --',ttxt).group(1))
        if  (8*3600/timeperevent)*filter_eff < 50 and timeperevent > 0 and int(test_cs_version[1]) > 9 and ppd == 0:
            print ("* [ERROR] please try to increase the filter efficiency")
            error += 1
        if  (8*3600/timeperevent)*filter_eff < 50 and timeperevent > 0 and int(test_cs_version[1]) <= 9 and ppd == 0:
            print ("* [ERROR] please try to increase the filter efficiency")
            error += 1
        if int(test_cs_version[1]) >= 10 and int(test_cs_version[2]) >= 6 and nthreads == 8 and mem != 15900 and ppd == 0:
            print ("* [ERROR] 8 core request with memory different from 15900 GB. Please set the memory to 15900 GB")
            error += 1
        if "HIN-HINPbPbAutumn18GSHIMix" not in pi and "HINPbPbAutumn18wmLHEGSHIMix" not in pi and "HINPbPbAutumn18GS" not in pi and ppd == 0:
            if mem > 2300 and mem != 4000 and mem != 15900:
                print "* [ERROR] Memory is not <=2300, =4000 or =15900 MB"
                error += 1
            if mem <= 2300 and nthreads != 1 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 1"
                error += 1
            if mem == 4000 and nthreads == 1 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 2,4 or 8"
                error += 1
            if mem == 15900 and (nthreads != 8 and nthreads != 16) :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 8 or 16"
                error += 1
        if "HIN-HINPbPbAutumn18GSHIMix" in pi or "HINPbPbAutumn18wmLHEGSHIMix" in pi or "HINPbPbAutumn18GS" in pi and ppd == 0:
            if mem != 14700 and mem != 5900 and mem != 4000 and mem > 2300:
                print "* [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign but Memory is not 14700, 5900, 400, or <= 2300 MB"
                error += 1
            if mem == 14700 and nthreads != 8 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 8"
                error += 1
            if mem == 5900 and nthreads != 4 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 4"
                error += 1
            if mem == 4000 and nthreads != 2 :
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 2"
                error += 1
            if mem <= 2300 and nthreads != 1:
                print "* [ERROR] Memory is "+str(mem)+" MB while number of cores is "+str(nthreads)+" but not = 1"
                error += 1

        gridpack_cvmfs_path_tmp = os.popen('grep \/cvmfs '+my_path+'/'+pi+'/'+pi).read()
        if int(os.popen('grep -c grid_points '+pi).read()) != 0:
            grid_points_flag = 1
        gp_size = len(gridpack_cvmfs_path_tmp)
	if "sherpa" in dn.lower():
                        print("* [WARNING] Not checking sherpacks for now.")
                        warning += 1
			gp_size = 0
        if fsize != 0:
            if int(os.popen('grep -c eos '+pi).read()) == 1 :
                print "* [ERROR] Gridpack should have used cvmfs path instead of eos path"
                error += 1
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
            if herwig_flag != 0:
		if int(os.popen('grep -c njetsmax '+pi).read()) == 1:
		    nJetMax = os.popen('grep njetsmax '+pi).read()
		    nJetMax = re.findall('\d+',nJetMax)
                    nJetMax = int(nJetMax[0])
            if int(os.popen('grep -c nFinal '+pi).read()) == 1:
                nFinal = os.popen('grep nFinal '+pi).read()
		if grid_points_flag == 1:
		    nFinal = re.findall('nFinal = \d+',nFinal)
		    nFinal = nFinal[0]
                nFinal =  re.findall('\d+',nFinal)
                nFinal = int(nFinal[0])
                print "nFinal="+str(nFinal)
            if int(os.popen('grep -c FlatRandomEGunProducer '+pi).read()) == 1 or int(os.popen('grep -c FlatRandomPtGunProducer '+pi).read()) == 1:
                particle_gun = 1
            print "* Using CMSSW release: "+cmssw
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
                    error += 1
                    tmp_flag = 1
                if len(mb_SigmaEl) == 0:
                    print "* [ERROR] SigmaTotal:sigmaEl is missing"
                    print "*         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1"
                    print "*         SigmaTotal:sigmaEl should be added by hand and set to 21.89"
                    error += 1
                    tmp_flag = 1
                if len(mb_SigmaTot) == 0:
                    print "* [ERROR] SigmaTotal:sigmaTot is missing"
                    print "*         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1"
                    print "*         SigmaTotal:sigmaTot should be added by hand and set to 100.309"
                    error += 1
                    tmp_flag = 1
                if len(PDF_pSet_test) == 0:
                    print "* [WARNING] PDF:pSet is missing (if you want to use NNPDF3.1)"
                    print "*         For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1"
                    print "*         PDF access method should be like"
                    print "*         e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'"
                    warning += 1
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
                        warning += 1

            if gp_size != 0:
                gridpack_cvmfs_path_tmp = re.findall("/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.xz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tgz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.gz",gridpack_cvmfs_path_tmp)
                gridpack_cvmfs_path = gridpack_cvmfs_path_tmp[0]
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
                jhu_gp = os.path.isfile(my_path+'/'+pi+'/'+'JHUGen.input')
                pw_gp = os.path.isfile(my_path+'/'+pi+'/'+'powheg.input')
                mg_gp = os.path.isfile(my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat') or os.path.isfile(my_path+'/'+pi+'/'+'process/Cards/run_card.dat')
                amcnlo_gp = os.path.isfile(my_path+'/'+pi+'/'+'process/Cards/run_card.dat')
                print "powheg "+str(pw_gp)
                print "mg "+str(mg_gp)
                print "jhugen "+str(jhu_gp)
                if any(word in dn for word in tunename) or "sherpa" in dn.lower() or ("herwigpp" in dn.lower() and ("eec5" in dn.lower() or "ee5c" in dn.lower())):
                    print "* [OK] Data set name has a known tune"
                else:
                    print "* [ERROR] Dataset name does not have the tune name: "+dn
                    print "*         Please add the tune name to the dataset."
                    error += 1
                if any(word in dn.lower() for word in psname):
                    print "* [OK] Dataset name contains a parton shower code name"
                else:
                    print "* [ERROR] Dataset name does not contain a parton shower code name: "+dn
                    print "*         Please add the parton shower name to the dataset name."
                    error += 1
                if any(word in dn.lower() for word in MEname):
                    print "Data set name is regular: "+dn
                else:
                    print "* [WARNING] Dataset name is not regular:"+dn
                    print "*           Please add the Generator name to the dataset."
                    warning += 1
                    if pw_gp is True:
                        dn = dn + "-powheg"
                    if mg_gp is True:
                        dn = dn + "-madgraph"
                    if jhu_gp is True:
                        dn = dn + "-jhugen"
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
                        warning += 1
                    if len(pf[1]) !=0 or len(pf[2]) != 0:
                        print "* [WARNING] "+pf[1]
                        print "*           "+pf[2]
                        print "*           You may try to request more events per phase-space region in the gridpack."
                        warning += 1
                if mg_gp is True:
                    filename_rc = my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'
                    fname_p2 = my_path+'/'+pi+'/'+'process/Cards/run_card.dat'
                    if os.path.isfile(fname_p2) is True :
                        filename_rc = fname_p2
                    ickkw_c = os.popen('more '+filename_rc+' | tr -s \' \' | grep "= ickkw"').read()
                    matching_c = int(re.search(r'\d+',ickkw_c).group())
                    maxjetflavor = os.popen('more '+filename_rc+' | tr -s \' \' | grep "= maxjetflavor"').read()
                    maxjetflavor = int(re.search(r'\d+',maxjetflavor).group())
                    print "maxjetflavor = "+str(maxjetflavor)
                    if matching_c == 3 and pythia8_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+filename_rc).read()
                        if "PYTHIA8" not in ps_hw.upper():
                            print "* [ERROR] PYTHIA8 = parton_shower not in run_card.dat"
                            error += 1
                    if matching_c == 3 and herwig_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+filename_rc).read()
                        if ("HERWIGPP" not in ps_hw.upper()) or ("HERWIG7" not in ps_hw.upper() and herwig7_bypass_error == 1):
                            print "* [ERROR] HERWIGPP or HERWIG7 = parton_shower not in run_card.dat"
                            error += 1
                        if int(os.popen('grep -c "set FxFxHandler:MergeMode FxFx" '+pi).read()) == 0:
			    print "* [ERROR] Please add \'set FxFxHandler:MergeMode FxFx\'"
			    print "*         and set FxFxHandler:njetsmax to the number of additional partons in the proc_card"
			    error += 1
                    if matching_c == 2 and herwig_flag != 0:
			if int(os.popen('grep -c herwig7CommonMergingSettingsBlock').read()) == 0:
			    print "* [ERROR] Please load herwig7CommonMergingSettingsBlock"
			    error += 1
                if amcnlo_gp is True:
                    ickkw_c = os.popen('more '+my_path+'/'+pi+'/'+'process/Cards/run_card.dat'+' | tr -s \' \' | grep "= ickkw"').read()
                    matching_c = int(re.search(r'\d+',ickkw_c).group())
                    print ickkw_c
                    if pythia8_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+my_path+'/'+pi+'/'+'process/Cards/run_card.dat').read()
                        if "PYTHIA8" not in ps_hw.upper():
                            print "* [ERROR] PYTHIA8 = parton_shower not in run_card.dat"
                            error += 1
                    if herwig_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+my_path+'/'+pi+'/'+'process/Cards/run_card.dat').read()
			if ("HERWIGPP" not in ps_hw.upper()) or ("HERWIG7" not in ps_hw.upper() and herwig7_bypass_error == 1):
                            print "* [ERROR] HERWIGPP or HERWIG7 = parton_shower not in run_card.dat"
                            error += 1

        if "jhugen" in dn.lower():
            if gp_size == 0:
                break
	    for root, dirs, files in os.walk(os.path.join(my_path, pi, "."), topdown=False):
   		for name in files:
      		    if "JHUGen.input" in name:
			print"* Found the JHUGen input file: "+os.path.join(root, name)
			jhufilename = os.path.join(root, name)
   		for name in dirs:
      	            if "JHUGen.input" in name:
	                print"* Found the JHUGen input file: "+os.path.join(root, name)
			jhufilename = os.path.join(root, name)
            if os.path.isfile(jhufilename) is True and pw_gp is False:
                with open(jhufilename) as f:
                    jhu_in = f.read()
                    jhu_in = re.sub(r'(?m)^ *#.*\n?', '',jhu_in)
                    jhu_pdf = re.findall('LHAPDF=\S+',jhu_in)
                    jhu_pdf = jhu_pdf[0].split('=')[1].split('/')[1]
                    print "##################################################"
                    print "* The PDF set used by JHUGEN is:"+ str(jhu_pdf)
                    print "##################################################"
                    if "UL" in pi and jhu_pdf not in UL_PDFs:
                        print"* [WARNING] The gridpack uses PDF = "+str(jhu_pdf)+" but not the recommended sets for UL requests:"
                        print"*                                   "+str(UL_PDFs_N[0])+" "+str(UL_PDFs[0])
                        print"*                                or "+str(UL_PDFs_N[1])+" "+str(UL_PDFs[1])
                        warning += 1
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
                            warning += 1
                    with open(os.path.join(my_path, pi, "runcmsgrid.sh"),'r+') as f:
                        content = f.read()
                        match = re.search(r"""process=(["']?)([^"']*)\1""", content)
			warning1,error1 = xml_check_and_patch(f,content,gridpack_eos_path,my_path,pi)
		        warning += warning1
 			error += error1
			f.close()
                    if os.path.isfile(my_path+'/'+pi+'/'+'external_tarball/runcmsgrid.sh') is True:
                        with open(os.path.join(my_path, pi, "external_tarball/runcmsgrid.sh"),'r+') as f2:
                            content2 = f2.read()
                            match = re.search(r"""process=(["']?)([^"']*)\1""", content2)
			    warning1,error1 = xml_check_and_patch(f2,content2,gridpack_eos_path,my_path,pi)
                            et_flag = 1
		    for file in os.listdir(my_path+'/'+pi+'/.'):
                    	if fnmatch.fnmatch(file,'*externaltarball.dat'):
			   file_i = file
			   et_flag_external = 1
                    if et_flag_external == 1:
			with open(my_path+'/'+pi+'/'+file_i) as f_ext:
			    for line in f_ext:
                                if line.startswith("EXTERNAL_TARBALL") == True:
				    powheg_gp = line.split('\"')[1]
                                    os.system('mkdir '+my_path+'/'+pi+'_powheg_gridpack')
				    os.system('tar xf '+powheg_gp+' -C '+my_path+'/'+pi+'_powheg_gridpack')
				    powheg_input = os.path.join(my_path,pi+'_powheg_gridpack', "powheg.input")
                    if et_flag == 0 and et_flag_external == 0:
			powheg_input = os.path.join(my_path, pi, "powheg.input")
                    if et_flag == 1 and et_flag_external == 0:
		        powheg_input = os.path.join(my_path, pi, "external_tarball/powheg.input")
                    with open(powheg_input) as f:
                        for line in f:
                            if line.startswith("!") == False and line.startswith("#") == False:
                                if "bornonly" in line:
                                    bornonly = int(re.split(r'\s+',line)[1])
                                if "lhans1" in line:
                                    pw_pdf = int(re.split(r'\s+', line)[1])
                                    print "##################################################"
                                    print "* Powheg PDF used is: "+str(pw_pdf)
                                    print "##################################################"
                                    if "UL" in pi and pw_pdf not in UL_PDFs_N:
                                        print"* [WARNING] The gridpack uses PDF="+str(pw_pdf)+" but not the recommended sets for UL requests:"
                                        print"*                                             "+str(UL_PDFs_N[0])+" "+str(UL_PDFs[0])
                                        print"*                                             or "+str(UL_PDFs_N[1])+" "+str(UL_PDFs[1])
                                        warning += 1
		    if os.path.isfile(my_path+'/'+pi+'/'+'external_tarball/pwg-rwl.dat') is True:
			pwg_rwl_file = os.path.join(my_path, pi, "external_tarball/pwg-rwl.dat")
                    else:
                        pwg_rwl_file = os.path.join(my_path, pi, "pwg-rwl.dat")
                    if os.path.isfile(pwg_rwl_file):
                        with open(pwg_rwl_file) as f_pdf:
                            pdf_var_check0 = 0
                            pdf_var_check1 = 0
                            scale_var_check0 = 0
                            scale_var_check1 = 0
                            for line in f_pdf:
                                if "scale_variation" in line:
                                    scale_var_check0 += 1
                                if "renscfact" in line and "facscfact" in line:
                                    scale_var_check1 += 1
                                if "PDF_variation" in line:
                                    pdf_var_check0 += 1
                                if str(pw_pdf)[0:3] in line:
                                    pdf_var_check1 += 1
                            if scale_var_check0 == 1 and scale_var_check1 == 9:
                                print "* [OK] Most probably ME scale variations are OK."
                            else:
                                print "* [WARNING] There may be a problem with scale variations. Please check pwg-rwl.dat"
                                warning += 1
                            if pdf_var_check0 > 0 and pdf_var_check1 > 1:
                                print "* [OK] Most probably PDF variations are OK."
                            else:
                                print "* [WARNING] There may be a problem with PDF variations. Please check pwg-rwl.dat"
                                warning += 1
                    if bornonly == 1:
                        bornonly_frag_check = 0
                        if int(os.popen('grep -c "Pythia8PowhegEmissionVetoSettings" '+pi).read()) == 1:
                            bornonly_frag_check = 1
                        if int(os.popen('grep -c "SpaceShower:pTmaxMatch" '+pi).read()) == 1:
                            bornonly_frag_check = 1
                        if int(os.popen('grep -c "TimeShower:pTmaxMatch" '+pi).read()) == 1:
                            bornonly_frag_check = 1
                        if bornonly_frag_check != 0:
                            print "* [ERROR] bornonly = 1 and (Pythia8PowhegEmissionVetoSettings or SpaceShower:pTmaxMatch or  TimeShower:pTmaxMatch)"
                            error += 1
                        else:
                            print "* [WARNING] bornonly = ",bornonly
                            warning += 1
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
                            if et_flag == 0 and et_flag_external == 0:
                                with open(os.path.join(my_path, pi, "powheg.input")) as f:
                                    content = f.read()
                                    matches = dict((name, re.search(r"^"+name+" *([0-9]+)", content, flags=re.MULTILINE)) for name in desiredvalues)
                            if et_flag == 1 and et_flag_external == 0:
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
                                        warning += 1
                                else:
                                    bad = True
                                    print "* [ERROR] didn't find "+name+" in powheg.input"
                                    error += 1
                            if not bad:
                                print "* [OK] integration grid setup looks ok for gg_H_quark-mass-effects"
                    else:
                        print "* [WARNING] Didn't find powheg process in runcmsgrid.sh"
                        warning += 1

                if ind > 0 and ind < 3:
                    if gp_size == 0:
                        break
                    filename_pc = my_path+'/'+pi+'/'+'process/madevent/Cards/proc_card_mg5.dat'
                    fname_p2 = my_path+'/'+pi+'/'+'process/Cards/proc_card.dat'
                    fname_p3 = my_path+'/'+pi+'/'+'process/Cards/proc_card_mg5.dat'
                    if os.path.isfile(fname_p2) is True :
                        filename_pc = fname_p2
                    if os.path.isfile(fname_p3) is True :
                        filename_pc = fname_p3
                    if os.path.isfile(filename_pc) is True :
                        mg_nlo = int(os.popen('grep -c "\[QCD\]" '+filename_pc).read())
                        loop_flag = int(os.popen('more '+filename_pc+' | grep -c "noborn=QCD"').read())
                        gen_line = os.popen('grep generate '+filename_pc).read()
                        print(gen_line)
                        proc_line = os.popen('grep process '+filename_pc).read()
                        print(proc_line)
                        if gen_line.count('@') <= proc_line.count('@'):
                            nproc = proc_line.count('@')
                            nproc = '@'+str(nproc)
                            proc_line = proc_line.split('add process')
                            jet_line = proc_line[len(proc_line)-1]
                            jet_line_arr = jet_line.split(',')
                            for x in range(0,len(jet_line_arr)):
                                nbtomatch = jet_line_arr[x].count('b') if maxjetflavor > 4 else 0
                                nc = jet_line_arr[x].count('c') if "chi" not in jet_line_arr[x] else 0
                                if "excl" in jet_line_arr[x] and nc != 0:
                                    nc = nc -1
                                jet_count_tmp.append(jet_line_arr[x].count('j') + nbtomatch + nc)
                            jet_count = max(jet_count_tmp)
                        else :
                            jet_line = gen_line.replace('generate','')
                            jet_count = jet_line.count('j') + jet_line.count('b') + jet_line.count('c')
                        if nJetMax == jet_count:
                            print "* [OK] nJetMax(="+str(nJetMax) + ") is equal to the number of jets in the process(="+str(jet_count)+")"
                        if nJetMax != jet_count and gen_line.count('@') != 0 and matching_c !=0:
                            print "* [WARNING] nJetMax(="+str(nJetMax)+") is NOT equal to the number of jets specified in the proc card(="+str(jet_count)+")"
                            warning += 1
                        if nJetMax != jet_count and str(jet_count)+"jet" in dn.lower() and matching_c !=0:
                            print "* [WARNING] nJetMax(="+str(nJetMax)+") is not equal to the number of jets specified in the proc card(="+str(jet_count)+")."
                            print "*           Is it because this is an exclusive production with additional samples with higher multiplicity generated separately?"
                            warning += 1
                    if os.path.isfile(filename_rc) is True :
                        ickkw = os.popen('more '+filename_rc+' | tr -s \' \' | grep "= ickkw"').read()
                        bw = os.popen('more '+filename_rc+' | tr -s \' \' | grep "= bwcutoff"').read()
                        mg_pdf = os.popen('more '+filename_rc+' | tr -s \' \' | grep "= lhaid"').read()
                    else:
                        if gp_size != 0:
                            print "* [ERROR] Although the name of the dataset has ~Madgraph, the gridpack doesn't seem to be a MG5_aMC one. Please check."
                            error += 1
                            break
                    version_file = my_path+'/'+pi+'/'+'mgbasedir/VERSION'
                    if os.path.isfile(version_file) is True:
                        mgversion_tmp = os.popen('grep version '+version_file).read()
                        mgversion = mgversion_tmp.split()
                        mgversion = mgversion[2].split(".")
                        mgversion_tmp = mgversion_tmp.split("\n")
                        mg5_aMC_version = int(mgversion[0])*100 + int(mgversion[1])*10 + int(mgversion[2])
                        print "###################################################################"
                        print "* The gridpack is made with mg5_aMC version:"+str(mg5_aMC_version)
                        print "###################################################################"
                        if "UL" in pi and mg5_aMC_version < 261:
                            if "PPD" in pi:
                                print"* [WARNING] You're using MG5_aMC "+str(mg5_aMC_version)+" in an Ultra Legacy Campaign. You should use MG5_aMCv2.6.1+"
                                warning += 1
                            else:
                                print"* [ERROR] You're using MG5_aMC "+str(mg5_aMC_version)+" in an Ultra Legacy Campaign. You should use MG5_aMCv2.6.1+"
                                error += 1
                    test_bw = bw.split()
                    if float(test_bw[0]) > 15.:
                        print " [WARNING] bwcutoff set to "+str(test_bw[0])+". Note that large bwcutoff values can cause problems in production."
                        warning += 1
                    mg_pdf = mg_pdf.split()
                    print "##################################################"
                    print "* The MG5_aMC PDF set is:"+str(mg_pdf[0])
                    print "##################################################"
                    if "UL" in pi and int(mg_pdf[0]) != UL_PDFs_N[0] and int(mg_pdf[0]) != UL_PDFs_N[1]:
                        print"* [WARNING] The gridpack uses PDF="+str(mg_pdf[0])+" but not the recommended sets for UL requests:"
                        print"*                                             "+str(UL_PDFs_N[0])+" "+str(UL_PDFs[0])
                        print"*                                             or "+str(UL_PDFs_N[1])+" "+str(UL_PDFs[1])
                        warning += 1
                    if mg_gp is True:
                        runcmsgrid_file = os.path.join(my_path, pi, "runcmsgrid.sh")
                        with open(runcmsgrid_file) as fmg:
                            fmg_f = fmg.read()
                            fmg_f = re.sub(r'(?m)^ *#.*\n?', '',fmg_f)
                            mg_me_pdf_list = re.findall('pdfsets=\S+',fmg_f)
                            if mg5_aMC_version >= 260:
                                mg_lo = int(os.popen('grep "systematics" '+str(runcmsgrid_file)+' | grep -c madevent').read())
                                mg_nlo = int(os.popen('grep "systematics" '+str(runcmsgrid_file)+' | grep -c aMCatNLO').read())
                            if mg5_aMC_version < 260:
                                mg_lo = int(os.popen('grep -c syscalc '+str(runcmsgrid_file)).read())
                                if mg_nlo > 0:
                                    if mg5_aMC_version < 242:
                                        print "* [WARNING] No automated PDF check for this version."
                                        warning += 1
                                        continue
                                    r_scale = os.popen('more '+filename_rc+' | tr -s \' \' | grep "reweight_scale"').read()
                                    r_scale = r_scale.split()[0]#.split('.')[1]
                                    if "." in r_scale:
                                        r_scale = r_scale.split('.')[1]
                                    if len(r_scale) == 0 or "true" not in str(r_scale).lower():
                                        print "* [ERROR] For NLO MG5_aMC version < 260, one should have .true. = reweight_scale"
                                        error += 1
                                    dir_path = os.path.join(my_path, pi, "InputCards")
                                    input_cards_run_card = find_file(dir_path,"run_card.dat")
                                    r_pdf = os.popen('more '+str(input_cards_run_card)+' | tr -s \' \' | grep "reweight_PDF"').read()
                                    r_pdf = r_pdf.split()[0]
                                    if len(r_pdf) == 0 or "$DEFAULT_PDF_MEMBERS" not in r_pdf:
                                        print "* [ERROR] For NLO MG5_aMC version < 260, one should have $DEFAULT_PDF_MEMBERS = reweight_PDF"
                                        error += 1
                            print "##################################################"
                            if mg_lo > 0 and mg_nlo > 0:
                                "* [ERROR] something's wrong - LO and NLO configs together."
                                error += 1
                            if mg_lo > 0:
                                print "* The MG5_aMC ME is running at LO"
                            if mg_nlo > 0:
                                print "* The MG5_aMC ME is running at NLO"
                            print "##################################################"
                            if mg_nlo > 0 and mg5_aMC_version >= 260:
                                if os.path.isfile(filename_rc) is True :
                                    store_rwgt_info = os.popen('more '+filename_rc+' | tr -s \' \' | grep "store_rwgt_info"').read()
                                if len(store_rwgt_info) != 0:
                                    store_rwgt_info_a = store_rwgt_info.split('=')
                                    if "false" in store_rwgt_info_a[0].lower():
                                        print "* [ERROR] store_rwgt_info set to"+ str(store_rwgt_info_a[0]) +" for MG5_aMC >= 260."
                                        error += 1
                                if len(store_rwgt_info) == 0:
                                    print "* [ERROR] No store_rwgt_info set for MG5_aMC >= 260."
                                    error += 1
                            if mg_lo > 0 and mg5_aMC_version >= 260:
                                if os.path.isfile(filename_rc) is True :
                                    use_syst = os.popen('more '+filename_rc+' | tr -s \' \' | grep "use_syst"').read()
                                if len(use_syst) != 0:
                                    use_syst_a = use_syst.split('=')
                                    if "false" in use_syst_a[0].lower():
                                        print "* [ERROR] use_syst set to"+ str(use_syst_a[0]) +" for MG5_aMC >= 260."
                                        error += 1
                                if len(use_syst) == 0:
                                    print "* [ERROR] No use_syst set for MG5_aMC >= 260."
                                    error += 1

                            if mg5_aMC_version < 260:
                                continue
                            mg_me_pdf_list = mg_me_pdf_list[0].split('=')[1].split('\"')[1].split(',')
                            var_count = [s for s in mg_me_pdf_list if "@0" in s]
                            if len(var_count) < 1:
                                print"* [WARNING] There will be no PDF variations! Please check the runcmsgrid file in the gridpack."
                                warning += 1
                            else:
                                print"* [OK] There are some PDF variations."
                            if "UL" in pi and mg_me_pdf_list.count(str(UL_PDFs_N[0])) != 1 and mg_me_pdf_list.count(str(UL_PDFs_N[1])) != 1:
                                if mg_me_pdf_list.count(str(UL_PDFs_N[0])) > 1 or mg_me_pdf_list.count(str(UL_PDFs_N[1])) > 1:
                                    print " [WARNING] At least one of the default PDF sets ("+UL_PDFs_N+") appear as variation as well or listed more than once."
                                    warning += 1
                                else:
                                    print"* [WARNING] pdfsets in runcmsgrid file does not contain one of the recommended sets:"
                                    print"*                                             "+str(UL_PDFs_N[0])+"("+str(UL_PDFs[0])+")"
                                    print"*                                             or "+str(UL_PDFs_N[1])+"("+str(UL_PDFs[1])+")"
                                    warning += 1
                                print"* Your runcmsgrid file contains these sets:"
                                print(mg_me_pdf_list)
                            if (mg_me_pdf_list.count(str(UL_PDFs_N[0])) > 0 and mg_me_pdf_list.count(str(UL_PDFs_N[0])+"@0") != 0) or (mg_me_pdf_list.count(str(UL_PDFs_N[1])) > 0 and mg_me_pdf_list.count(str(UL_PDFs_N[1])+"@0") != 0):
                                print"* [WARNING] Main pdf recommended set ("+str(UL_PDFs_N[0])+" or "+str(UL_PDFs_N[1])+") is listed in runcmsgrid file but it is also included as a variation??"
                                warning += 1
                    matching = int(re.search(r'\d+',ickkw).group())
                    ickkw = str(ickkw)
                    if matching == 1 or matching == 2:
                        if match_eff == 1:
                            print "* [WARNING] Matched sample but matching efficiency is 1!"
                            warning += 1
                    if ind < 2 and mg_nlo != 1:
                        MGpatch.append(int(os.popen('more '+my_path+'/'+pi+'/'+'runcmsgrid.sh | grep -c "FORCE IT TO"').read()))
                        MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'mgbasedir/Template/LO/SubProcesses/refine.sh').read()))
                        MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'process/madevent/SubProcesses/refine.sh').read()))
                        if MGpatch[0] == 1 and MGpatch[1] == 1 and MGpatch[2] == 1:
                            print "*"
                            print "* [OK] MG5_aMC@NLO leading order patches OK in gridpack"
                        if MGpatch[0] != 1:
                            print "* [ERROR] MG5_aMC@NLO multi-run patch missing in gridpack - please re-create a gridpack"
                            print "*            using updated genproductions area"
                            error += 1
                        if MGpatch[1] == 0 or MGpatch[2] == 0:
                            if '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw :
                                print "* [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                                print "* And the request is using a version "+str(cmssw)+" that does not contain the patch."
                                print "* Please use >= 7_1_32_patch1 or CMSSW_9_3_9_patch1 or 10_2_0_pre2"
                                error += 1
                            elif '7_1' in cmssw:
                                test_version = cmssw.split('_')
                                if (len(test_version) == 4 and int(test_version[3]) < 33) or (len(test_version) == 5 and (int(test_version[3]) < 32 or (int(test_version[3]) == 32 and "patch1" not in cmssw))):
                                    print "* [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                                    print "* And the request is using a version "+str(cmssw)+" that does not contain the patch."
                                    print "* In this release, please at least use CMSSW_7_1_32_patch1"
                                    error += 1
                            elif '9_3' in cmssw:
                                test_version = cmssw.split('_')
                                if (len(test_version) == 4 and int(test_version[3]) < 10) or (len(test_version) == 5 and (int(test_version[3]) < 9 or (int(test_version[3]) == 9 and "patch1" not in cmssw))):
                                    print "* [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                                    print "* And the request is using a version "+str(cmssw)+" that does not contain the patch."
                                    print "* In this release, please at least use CMSSW_9_3_9_patch1"
                                    error += 1
                            elif '10_2' in cmssw:
                                test_version = cmssw.split('_')
                                if len(test_version) == 4 and int(test_version[3]) < 1:
                                    print "* [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing."
                                    print "* And the request is using a version "+str(cmssw)+" that does not contain the patch."
                                    print "* In this release, please at least use CMSSW_10_2_0_pre2"
                                    error += 1
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
                                error += 1
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
                        warning += 1
                elif matching == 1 and check[0] == 0 and check[1] == 0 and check[2] == 0 :
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    if matching_c != 0:
                        mg_lo_w_matching = 1
                        print "* [WARNING] To check manually - This is a matched MadGraph LO sample. Please check 'JetMatching:nJetMax' ="+str(nJetMax)+" is OK and"
                        print "*            correctly set as number of partons in born matrix element for highest multiplicity."
                        warning += 1
                elif matching == 0 and word == "madgraph" and check[0] == 0 and check[1] == 0 and check[2] == 0 :
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                elif matching == 0 and word == "mcatnlo" and check[0] == 2 and check[1] == 1 and check[2] == 1 and loop_flag != 1:
                    print "* [OK] no known inconsistency in the fragment w.r.t. the name of the dataset "+word
                    if matching_c == 0:
                        print "* [WARNING] This a MadGraph NLO sample without matching. Please check 'TimeShower:nPartonsInBorn'"
                        print "*                                                   is set correctly as number of coloured particles"
                        print "*                                                  (before resonance decays) in born matrix element."
                        warning += 1
                else:
                    if word != "powheg" and (word == "madgraph" and mg_nlo != 1):
                        print "* [ERROR] Fragment may be wrong: check "+word+" settings in the fragment"
                        error += 1
                    if matching <= 1 and word == "madgraph" and mg_nlo != 1:
                        print "* [ERROR] You run MG5_aMC@NLO at LO but you have  Pythia8aMCatNLOSettings_cfi in fragment"
                        print "*           --> please remove it from the fragment"
                        error += 1
        if knd == 1 :
             powhegcheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
             if powhegcheck[0] > 0 :
                 print "* [ERROR] Please remove POWHEG settings for MG requests."
                 error += 1
        if knd == -1 :
             purepythiacheck.append(int(os.popen('grep -c -i Pythia8aMCatNLOSettings '+pi).read()))
             purepythiacheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
             if purepythiacheck[0] > 0 or purepythiacheck[1] >0 :
                 print "* [WARNING] Please remove aMCatNLO or POWHEG settings if this is a pure Pythia request."
                 print "*           If it's not a pure request, in the future, please include madgraph/powheg or amcatnlo"
                 print "*           in the name of the dataset"
                 warning += 1
        if loop_flag == 1:
            if mcatnlo_flag == 1:
                print "* [ERROR] You are using a loop induced process, [noborn=QCD]."
                print "*         Please remove all occurances of Pythia8aMCatNLOSettings from the fragment"
                error += 1
            if nPartonsInBorn_flag == 1:
                print "* [ERROR] You are using a loop induced process, [noborn=QCD]."
                print "*         Please remove all TimeShower:nPartonsInBorn from the fragment"
                error += 1
        for kk in range (0, 8):
            tunecheck.append(int(os.popen('grep -v "#" '+pi+' | grep -v "annotation" | grep -v "name" | grep -c -i '+tune[kk]).read()))
        if tunecheck[6] == 3 or tunecheck[7] == 3:
            if tunecheck[0] != 3:
                print "* [WARNING] Check if there is some extra tune setting"
                warning += 1
        if 'sherpa' in dn.lower():
            print "* [WARNING] No automated check of Sherpa ps/tune parameters yet"
            warning += 1
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
                warning += 1
        if 3 not in tunecheck and 'sherpa' not in dn.lower() and fsize != 0 and n_ext_par == 0 and herwig_flag == 0:
            if  any(tunecheck[0]<3 and it!=0 for it in tunecheck) :
              	print tunecheck
              	print "* [ERROR] Tune configuration may be wrong in the fragment"
 	    	print "*         or pythia8CUEP8M1Settings are overwritten by some other parameters as in CUETP8M2T4"
            	error += 1
	    else :
		print "* [WARNING] None standard tune - please check the fragment carefully."
		warning += 1
        elif 3 in tunecheck:
            print "* [OK] Tune configuration probably OK in the fragment"
            if tunecheck[0] > 2 :
                if 'Summer19UL' not in pi and 'Fall18' not in pi and 'Fall17' not in pi and 'Run3' not in pi:
                    print "* [WARNING] Do you really want to have tune "+tune[0] +" in this campaign?"
                    warning += 1
        if 'Fall18' in pi and 'UL' in pi and fsize != 0 and herwig_flag == 0:
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) != 1 :
                print "* [WARNING] No parton shower weights configuration in the fragment. In the Fall18 campaign, we recommend to include Parton Shower weights"
                warning += 1
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) == 1 :
                cmssw_version    = int(re.search("_[0-9]?[0-9]_[0-9]?[0-9]_[0-9]?[0-9]",cmssw).group().replace('_',''))
                if cmssw_version < int('10_2_3'.replace('_','')) :
                    print "* [ERROR] PS weights in config but CMSSW version is < 10_2_3 - please check!"
                    error += 1
                psweightscheck.append(int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettingsBlock," '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettings" '+pi).read()))
                if psweightscheck[0] == 1 and psweightscheck[1] == 1 and psweightscheck[2] == 2 :
                    print "* [OK] Parton shower weight configuration probably OK in the fragment"
                else:
                    print "* [ERROR] Parton shower weight configuration not OK in the fragment"
                    error += 1
        if evtgen_flag == 1 and fsize != 0:
            if int(os.popen('grep -c -i EvtGen '+pi).read()) == 0:
                print "* [ERROR] evtgen flag defined in dataset name but not set within the fragment"
                error += 1
            if int(os.popen('grep -c -i EvtGen '+pi).read()) != 0 and int(os.popen('grep -c -i "from GeneratorInterface.EvtGenInterface.EvtGenSetting_cff import *" '+pi).read()) == 0:
                print "* [ERROR] EvtGenInterface not definied within the fragment"
                error += 1
            if int(os.popen('grep -c -i EvtGen '+pi).read()) != 0 and int(os.popen('grep -c -i "convertPythiaCodes = cms.untracked.bool(False)" '+pi).read()) == 0:
                print "* [ERROR] Turn off the PythiaCodes conversion when using EvtGen"
                error += 1
	    if int(os.popen('grep -c -i evt_2014.pdl '+pi).read()) == 0:
                print "* [WARNING] Are you sure you do not want the 2014 evt pdl table?"
                warning += 1
            if int(os.popen('grep -c -i DECAY_2014 '+pi).read()) == 0:
                print "* [WARNING] Are you sure you do not want the 2014 decay dec table?"
                warning += 1
            if int(os.popen('grep -c -i EvtGen '+pi).read()) != 0 and int(os.popen('grep -c -i "generator.PythiaParameters.processParameters.extend(EvtGenExtraParticles)" '+pi).read()) == 0:
                print "* [WARNING] Are you sure EvtGenExtraParticles extension is not needed?"
                warning += 1
        if evtgen_flag == 0 and fsize != 0 and int(os.popen('grep -c -i EvtGen '+pi).read()) != 0:
            print "* [ERROR] EvtGen settings within fragment but no evtgen flag at dataset name"
            error += 1
        if int(os.popen('grep -c -i filter '+pi).read()) > 3 and filter_eff == 1:
            print "* [WARNING] Filters in the fragment but filter efficiency = 1"
            warning += 1
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
