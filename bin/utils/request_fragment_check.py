#!/bin/env python3
import os
import sys
import re
import argparse
import textwrap
import fnmatch
import os.path
import string
import glob
from datetime import datetime
###########Needed to check for ultra-legacy sample consistency check############################################
os.system('env -i KRB5CCNAME="$KRB5CCNAME" cern-get-sso-cookie -u https://cms-pdmv.cern.ch/mcm/ -o cookiefile.txt --krb --reprocess')
################################################################################################################
sys.path.append('/afs/cern.ch/cms/PPD/PdmV/tools/McM/')
from rest3 import McM

parser = argparse.ArgumentParser(
    formatter_class=argparse.RawDescriptionHelpFormatter,
    description=textwrap.dedent('''\
            ------------------------------------------------
               This script checks for possible WARNINGs and ERRORs
               and does a patch for the MG5_aMC LO nthreads problem if needed.
                                                                                    '''))
parser.add_argument('--prepid', type=str, help="check mcm requests using prepids", nargs='+')
parser.add_argument('--ticket', type=str, help="check mcm requests using ticket number", nargs=1)
parser.add_argument('--bypass_status', help="don't check request status in mcm", action='store_false')
parser.add_argument('--bypass_validation', help="proceed to next prepid even if there are errors", action='store_true')
parser.add_argument('--apply_many_threads_patch', help="apply the many threads MG5_aMC@NLO LO patch if necessary", action='store_true')
parser.add_argument('--dev', help="Run on DEV instance of McM", action='store_true')
parser.add_argument('--debug', help="Print debugging information", action='store_true')
parser.add_argument('--develop', help="Option to make modifications of the script", action='store_true')
args = parser.parse_args()

if args.prepid is not None:
    if len(args.prepid) == 1: print("---> "+str(len(args.prepid))+" request will be checked:")
    if len(args.prepid) != 1: print("---> "+str(len(args.prepid))+" requests will be checked:")
    prepid = args.prepid
print(" ")

if args.develop is False:
   os.popen('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/request_fragment_check.py -O req_script_master.py').read()
   scr_master_check = []
   scr_file1 = open('req_script_master.py',"r")
   scr_file2 = open('request_fragment_check.py',"r")
   for line in scr_file1:
       if line not in scr_file2: scr_master_check.append(line)
   os.popen('rm req_script_master.py').read()
   if len(scr_master_check)!= 0:
       print("Please use the github master version of the script. Exiting")
       sys.exit()
	
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

def check_replace(runcmsgridfile):
    error_check_replace = []
    replace_mccont = os.popen('grep "_REPLACE" '+str(runcmsgridfile)).read()
    if len(replace_mccont):
        error_check_replace.append(" Incomplete gridpack. Replace _REPLACE strings in runcmsgrid.sh:")
    return error_check_replace 

def slha_gp(gridpack_cvmfs_path,slha_flag):
    if slha_flag == 1:
        if "%i" in gridpack_cvmfs_path:
            gridpack_cvmfs_path = gridpack_cvmfs_path.replace("%i","*")
        elif "%s" in gridpack_cvmfs_path:
            gridpack_cvmfs_path = gridpack_cvmfs_path.replace("%s","*")
        elif "%d" in gridpack_cvmfs_path:
            gridpack_cvmfs_path = gridpack_cvmfs_path.replace("%d","*")
        else:
            slha_flag = 0
        if slha_flag == 1:
            slha_all_path = os.path.dirname(gridpack_eos_path)
            print("Directory: "+slha_all_path)
            list_gridpack_cvmfs_path = os.listdir(slha_all_path)[0]
            print(list_gridpack_cvmfs_path)
            gridpack_cvmfs_path = slha_all_path+'/'+list_gridpack_cvmfs_path
            print("SLHA request - checking single gridpack:")
            print(gridpack_cvmfs_path)
        return gridpack_cvmfs_path, slha_all_path, slha_flag


def tunes_settings_check(dn,fragment,pi,sherpa_flag):
    error_tunes_check = []
    if "Summer22" in pi and "FlatRandomEGunProducer" not in fragment and "FlatRandomPtGunProducer" not in fragment and "Pythia8EGun" not in fragment and "Pythia8PtGun" not in fragment and "FlatRandomPtAndDxyGunProducer" not in fragment and sherpa_flag == 0:
        if "Configuration.Generator.MCTunesRun3ECM13p6TeV" not in fragment or "from Configuration.Generator.MCTunes2017" in fragment:
            error_tunes_check.append(" For Summer22 samples, please use from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import * in your fragment instead of from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *")
    if "Run3" in pi and (dn.startswith("DYto") or dn.startswith("Wto")):
        if "ktdard" in fragment and "0.248" not in fragment:
            error_tunes_check.append(" 'kthard = 0.248' not in fragment for DY or Wjets MG5_aMC request for Run3. Please fix.")
    return error_tunes_check                
 
def concurrency_check(fragment,pi,cmssw_version,mg_gp):
    conc_check = 0
    conc_check_lhe = 0
    error_conc = []
    fragment = re.sub(r'(?m)^ *#.*\n?', '',fragment) # remove lines starting with #
    fragment = fragment.replace(" ","").replace("\"","'")
    if cmssw_version >= int('10_60_28'.replace('_','')) and int(str(cmssw_version)[:2]) != 11:
        if "generateConcurrently=cms.untracked.bool(False)" in fragment and "Pythia8Concurrent" in fragment and mg_gp is False:
            error_conc.append("Concurrent parameters used with generateConcurrently=cms.untracked.bool(False) in fragment.")
        if "generateConcurrently=cms.untracked.bool(True)" in fragment and mg_gp is True:
            error_conc.append("For MG5_aMC requests, currently the concurrent mode for LHE production is not supported due to heavy I/O. So, please set generateConcurrently = cms.untracked.bool(False) in ExternalLHEProducer.")
        if "ExternalLHEProducer" in fragment and "generateConcurrently=cms.untracked.bool(True)" in fragment: 
            # first check if the code has correctly implemented concurrent features. Mark conc_check_lhe (LHE step) or conc_check (GEN step) as True if features are found
            if "Herwig7GeneratorFilter" not in fragment: 
                conc_check_lhe = 1
            else:
                if "postGenerationCommand=cms.untracked.vstring('mergeLHE.py','-i','thread*/cmsgrid_final.lhe','-o','cmsgrid_final.lhe')" in fragment: 
                    conc_check_lhe = 1    
        elif "ExternalLHEProducer" not in fragment:
            conc_check_lhe = 1
        if "ExternalDecays" not in fragment and "Pythia8ConcurrentHadronizerFilter" in fragment: 
            conc_check = 1
        if "Pythia8ConcurrentGeneratorFilter" in fragment and "ExternalDecays" not in fragment and "RandomizedParameters" not in fragment: 
            conc_check = 1
        if "ExternalLHEProducer" not in fragment and "_generator=cms.EDFilter" in fragment and "fromGeneratorInterface.Core.ExternalGeneratorFilterimportExternalGeneratorFilter" in fragment and "generator=ExternalGeneratorFilter(_generator" in fragment:
            if "Pythia8GeneratorFilter" in fragment and "tauola" not in fragment.lower(): 
                conc_check = 1
            if "Pythia8GeneratorFilter" in fragment and "tauola" in fragment.lower() and "_external_process_components_=cms.vstring('HepPDTESSource')" in fragment:
                conc_check = 1
            if "AMPTGeneratorFilter" in fragment or "HydjetGeneratorFilter" in fragment or "PyquenGeneratorFilter" in fragment or "Pythia6GeneratorFilter": 
                conc_check = 1
            if "ReggeGribovPartonMCGeneratorFilter" in fragment or "SherpaGeneratorFilter" in fragment: 
                conc_check = 1
            if "Herwig7GeneratorFilter" in fragment and "wmlhegen" not in pi.lower() and "plhegen" not in pi.lower(): 
                conc_check = 1 
        print("Concurrency check LHE = ",conc_check_lhe,"  Concurrency check GEN = ",conc_check)
        if conc_check_lhe and conc_check:
            print("\n The request will be generated concurrently\n")
            if "randomizedparameters" in fragment.lower():
                error_conc.append("Concurrent generation parameters used along with RandomizedParameter scan.")
        else:
            # then if not both the LHE and GEN step turns on concurrent features, we check if for some cases it is ok not to have concurrency
            if "Pythia8HadronizerFilter" in fragment and ("evtgen" in fragment.lower() or "tauola" in fragment.lower() or "photos" in fragment.lower()):
                print("\n Pythia8HadronizerFilter with EvtGen, Tauola, or Photos can not be made concurrently.\n")
            elif "Herwig7GeneratorFilter" in fragment and ("wmlhegen" in pi.lower() or "plhegen" in pi.lower()): 
                print("Herwig7GeneratorFilter in the wmLHEGEN or pLHEGEN campaign cannot run concurrently.")
            elif "Pythia8GeneratorFilter" in fragment and "randomizedparameters" in fragment.lower():
                print("Pythia8GeneratorFilter with RandomizedParameter scan cannot run concurrently")
            elif mg_gp is True:
                print("For MG5_aMC requests, currently the concurrent mode for LHE production is not supported due to heavy I/O.")
            # for other cases, it is either concurrent generation parameters are missing or wrong        
            else:
                error_conc.append("Concurrent generation parameters missing or wrong. Please see https://twiki.cern.ch/twiki/bin/view/CMSPublic/WorkBookGenMultithread")
                    
    else:
        if "concurrent" in fragment.lower():
            error_conc.append("Concurrent generation is not supported for versions < CMSSW_10_6_28 and CMSSW_11_X_X series")
    return conc_check_lhe and conc_check, error_conc
   
def ul_consistency(dn,pi,jhu_gp):
    pi_prime = "NULL"
    prime_tmp = []
    warning_ul = []
    error_ul = []
    if any(pi in s for s in ["Summer20UL18", "Summer20UL17", "Summer20UL16wmLHEGENAPV", "APV", "Summer20UL16"]) and "GEN" in pi and "lowpu" not in dn.lower():
        prime = get_requests_from_datasetname(dn)

        if not prime:
            if "Summer20UL16" not in pi:
                error_ul.append("No corresponing Summer20UL16 request to compare to for consistency.  Please first create the corresponding Summer20UL16 requests.")
            else:
                warning_ul.append("No corresponing Summer19UL16 request to compare to for consistency. Please check CAREFULLY!")
        if len(prime) != 0:
            print("Related UL16 requests:")
            for rr in prime:
                if "Summer20UL16" in rr['prepid'] and "GEN" in rr['prepid'] and "APV" not in rr['prepid'] and ("Summer20UL18" in pi or "Summer20UL17" in pi or "APV" in pi):
                    print((rr['prepid'],rr['extension'],ext))
                    pi_prime = rr['prepid']
                    cmssw_prime = rr['cmssw_release']
                if "Summer20UL16" in pi and "APV" not in pi and "GEN" in rr['prepid'] and ext == rr['extension'] and "Summer19UL17" in rr['prepid']:
                    pi_prime = rr['prepid']
                    cmssw_prime = rr['cmssw_release']
        print(("pi_prime=",pi_prime))
        if "NULL" in pi_prime and ("APV" in pi or "Summer20UL18" in pi or "Summer20UL17" in pi):
            error_ul.append("No corresponing Summer20UL16 request to compare to for consistency. Please first create the corresponding Summer20UL16 requests.")
        if "NULL" in pi_prime and "APV" not in pi:
            warning_ul.append("No corresponing Summer19UL17 request to compare to for consistency. LEVEL2 Conveners - please check the request VERY CAREFULLY!")
        if "NULL" not in pi_prime: #
            if "APV" in pi or "Summer20UL18" in pi or "Summer20UL17" in pi: print("This is a Summer20UL16APV, UL17 or UL18 request so GEN settings will be compared to the corresponding Summer20UL16 request: "+pi_prime)
            if "APV" not in pi and "Summer20UL16" in pi: print("This is a Summer20UL16 requests so GEN setting will be compared to the corresponding Summer19UL17 request: "+pi_prime)
            os.popen('wget -q '+mcm_link+'public/restapi/requests/get_fragment/'+pi_prime+' -O '+pi_prime).read()
            f1_prime = open(pi_prime,"r")
            f2_prime = open(pi_prime+"_tmp","w")
            data_f1_prime = f1_prime.read()
            data_f2_prime = re.sub(r'(?m)^ *#.*\n?', '',data_f1_prime)
            excep = 0
            os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/exceptions.txt -O exceptions.txt')
            with open('exceptions.txt') as file_ex:
                for line in file_ex:
                    if pi in line: excep = 1 
            if jhu_gp or excep:
                data_f2_jhu = re.sub(r'\s+', ' ', data_f2).strip()
                data_f2_jhu = exception_for_ul_check(data_f2_jhu,cross_section_fragment)
                data_f2_jhu_prime = re.sub(r'\s+', ' ',data_f2_prime).strip()
                data_f2_jhu_prime = exception_for_ul_check(data_f2_jhu_prime,cross_section_fragment)
                ### skip gp comparison to be able to have WriteFailedEvents = 2  JHUGen.input
                data_f2_jhu = re.sub("args=cms.vstring\(.*?\)","args=cms.vstring()",data_f2_jhu)
                data_f2_jhu_prime = re.sub("args=cms.vstring\(.*?\)","args=cms.vstring()",data_f2_jhu_prime)
                ########
                if (data_f2_jhu == data_f2_jhu_prime) == True:
                    warning_ul.append("Two requests have the same fragment (except may be the gridpack)")
                else:
                    error_ul.append("Two requests don't have the same fragment (note that gridpacks haven't been compared to be able to have WriteFailedEvents = 2  JHUGen.input)")
            else:
                data_f2_strip = re.sub(r'\s+', ' ', data_f2).strip()
                data_f2_strip = exception_for_ul_check(data_f2_strip,cross_section_fragment)
                data_f2_prime_strip = re.sub(r'\s+', ' ',data_f2_prime).strip()
                data_f2_prime_strip = exception_for_ul_check(data_f2_prime_strip,cross_section_fragment)
                if (data_f2_strip == data_f2_prime_strip) == True:
                    print("[OK] Two requests have the same fragment.")
                else: 
                    if "Summer20UL16" not in pi:
                        error_ul.append("Fragment of "+pi+" is different than its base UL request: "+pi_prime+"Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime)
                    if "Summer20UL16" in pi and "APV" in pi:
                        error_ul.append("Fragment of "+pi+" is different than its base Summer20UL16 request: "+pi_prime+"Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime)
                    if "Summer20UL16" in pi and "APV" not in pi:
                        warning_ul.append("Fragment of "+pi+" is different than its base Summer19UL17 request: "+pi_prime+"Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime)
            if (cmssw == cmssw_prime) == True:
                print("[OK] Two requests have the same CMSSW version.")
            elif "Summer20UL16wmLHEGENAPV" in pi or "Summer20UL16GENAPV" in pi or "Summer20UL18" in pi or "Summer20UL17" in pi:
                warning_ul.append("CMSSW version of "+pi+" is different than its base UL17 request: "+pi_prime+"Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime)
            f1_prime.close()
            f2_prime.write(data_f2_prime)
            f2_prime.close()
    if not error_ul: print("UL consistency check is OK.")
    return warning_ul,error_ul

def xml_check_and_patch(f,cont,gridpack_eos_path,my_path,pi):
    xml = str(re.findall('xmllint.*',cont))
    cur_dir = os.getcwd()
    warning_xml = []
    error_xml = []
    if "stream" not in xml or len(xml) < 3:
        targz_flag = 0
        if "stream" not in xml and len(xml) > 3:
          warning_xml.append(" --stream option is missing in XMLLINT, will update runcmsgrid.")
        if len(xml) < 3:
          warning_xml.append("[WARNING] XMLLINT does not exist in runcmsgrid, will update it.")
        if ".tar.gz" in gridpack_eos_path:
          targz_flag = 1
          gridpack_eos_path_backup = gridpack_eos_path.replace('.tar.gz','_original.tar.gz')
        if ".tgz" in gridpack_eos_path: gridpack_eos_path_backup = gridpack_eos_path.replace('.tgz','_original.tgz')
        if ".tar.xz" in gridpack_eos_path:
          gridpack_eos_path_backup = gridpack_eos_path.replace('.tar.xz','_original.tar.xz')
          targz_flag = 2
        if not os.path.exists(gridpack_eos_path_backup):
          print("Backup gridpack does not exist.")
          print("Copying "+gridpack_eos_path+" to "+gridpack_eos_path_backup+" before patching runcms.grid")
          os.system('cp -n -p '+gridpack_eos_path+' '+gridpack_eos_path_backup)
          md5_1 = os.popen('md5sum'+' '+gridpack_eos_path).read().split(' ')[0]
          md5_2 = os.popen('md5sum'+' '+gridpack_eos_path_backup).read().split(' ')[0]
          if md5_1 == md5_2:
            print("Backup and original file checksums are equal.")
          else:
            error_xml.append("backup gridpack has a problem.")
        print("Updating XMLLINT line in runcmsgrid.")
        os.chdir(my_path+'/'+pi)
        if "stream" not in xml and len(xml) > 3: cont = re.sub("xmllint","xmllint --stream",cont)
        if len(xml) < 3:
          newlinetoadd = 'xmllint --stream --noout ${file}_final.lhe > /dev/null 2>&1; test $? -eq 0 || fail_exit "xmllint --stream integrity check failed on pwgevents.lhe" \ncp ${file}_final.lhe ${WORKDIR}/.'
          string_orig = "cp \$\{file\}\_final.lhe \$\{WORKDIR\}\/\."
          cont = re.sub(string_orig,newlinetoadd,cont)
        f.seek(0)
        f.write(cont)
        f.truncate()
        if targz_flag == 0: gridpackname = "gridpack.tgz"
        if targz_flag == 1: gridpackname = "gridpack.tar.gz"
        if targz_flag == 2: gridpackname = "gridpack.tar.xz"
        os.chdir(my_path+'/'+pi)
        os.system('tar cfJ '+gridpackname+' ./* --exclude='+gridpackname+' --exclude='+pi)
        os.system('cp '+gridpackname+' '+gridpack_eos_path)
        md5_1 = os.popen('md5sum '+gridpackname).read().split(' ')[0]
        md5_2 = os.popen('md5sum'+' '+gridpack_eos_path).read().split(' ')[0]
        if md5_1 == md5_2:
          print("Updated gridpack copied succesfully.")
        else:
          error_xml.append("There was a problem copying in the updated gridpack to eos.")
        os.chdir(cur_dir)
    return warning_xml,error_xml

def evtgen_check(fragment):
    err = []
    warn = []
    fragment = fragment.replace(" ","")
    if "evtgen" not in fragment.lower():
        err.append("evtgen flag defined in dataset name but not set within the fragment")
    else:
        if "fromGeneratorInterface.EvtGenInterface.EvtGenSetting_cffimport*" not in fragment:
            err.append("EvtGenInterface not definied within the fragment")
        if "convertPythiaCodes=cms.untracked.bool(False)" not in fragment:
            err.append("Turn off the PythiaCodes conversion when using EvtGen, i.e. Add convertPythiaCodes=cms.untracked.bool(False) in the fragment.")
        if "generator.PythiaParameters.processParameters.extend(EvtGenExtraParticles)" not in fragment:
            warn.append("Are you sure EvtGenExtraParticles extension is not needed?")
    if "evt_2014.pdl" not in fragment:
        warn.append("Are you sure you do not want the 2014 evt pdl table?")
    if "DECAY_2014" not in fragment:
        warn.append("Are you sure you do not want the 2014 decay dec table?")
    return warn, err

def run3_checks(fragment,dn,pi):
    err = []
    fragment = fragment.replace(" ","")
    print("======> Run3 Fragment and dataset name checks:")
    if "comEnergy" in fragment:
        comline = re.findall('comEnergy=\S+',fragment)
        if ("run3winter22" in pi.lower() or "summer2" in pi.lower()) and "13600" not in comline[0]:
            err.append("The c.o.m. energy is not specified as 13600 GeV in the fragment."+comline[0])
        if "run3winter21" in pi.lower() and "14000" not in comline[0]: 
            err.append("The c.o.m. energy is not specified as 14000 GeV in the fragment"+comline[0])
    if ("run3winter22" in pi.lower() or "summer2" in pi.lower()) and ("FlatRandomEGunProducer" not in fragment and "FlatRandomPtGunProducer" not in fragment and "Pythia8EGun" not in fragment and "13p6TeV" not in dn):
        err.append("The data set name does not contain 13p6TeV for this Run3 request")
    if "run3winter21" in pi.lower() and ("FlatRandomEGunProducer" not in fragment and "FlatRandomPtGunProducer" not in fragment and "Pythia8EGun" not in fragment and "14TeV" not in dn):
        err.append("The data set name does not contain 14TeV for this Run3 request")
    return err

def run3_run_card_check(filename_mggpc,pi):
    err = []
    beamenergy1 = os.popen('grep ebeam1 '+filename_mggpc).read()
    beamenergy2 = os.popen('grep ebeam2 '+filename_mggpc).read()
    print("======> Run3 run_card check for MG5aMC") 
    print(beamenergy1,beamenergy2)
    if ("run3winter22" in pi.lower() or "summer2" in pi.lower()) and ("6800" not in beamenergy1 or "6800" not in beamenergy2):
        err.append("The beam energy is not specified as 6800 GeV in the run_card")
    if "run3winter21" in pi.lower() and ("7000" not in beamenergy1 or "7000" not in beamenergy2):
        err.append("The beam energy is not specified as 7000 GeV in the run_card")
    return err 

def exception_for_ul_check(datatobereplaced,cross_section_fragment):
    new_data = datatobereplaced.replace(" ","")
    new_data = new_data.replace(",generateConcurrently=cms.untracked.bool(True)","")
    new_data = new_data.replace("Concurrent","")
    new_data = new_data.replace(",postGenerationCommand=cms.untracked.vstring('mergeLHE.py','-i','thread*/cmsgrid_final.lhe','-o','cmsgrid_final.lhe')","")
    new_data = new_data.replace("Pythia8ConcurrentHadronizerFilter","Pythia8HadronizerFilter")
    new_data = new_data.replace('_generator=cms.EDFilter("Pythia8GeneratorFilter"','')
    new_data = new_data.replace('_generator=cms.EDFilter("AMPTGeneratorFilter"','')
    new_data = new_data.replace('_generator=cms.EDFilter("HydjetGeneratorFilter"','')
    new_data = new_data.replace('_generator=cms.EDFilter("PyquenGeneratorFilter"','')
    new_data = new_data.replace('_generator=cms.EDFilter("Pythia6GeneratorFilter"','')
    new_data = new_data.replace('_generator=cms.EDFilter("ReggeGribovPartonMCGeneratorFilter"','')
    new_data = new_data.replace('_generator=cms.EDFilter("SherpaGeneratorFilter"','generator=cms.EDFilter("SherpaGeneratorFilter"')
    new_data = new_data.replace('_generator=cms.EDFilter("Herwig7GeneratorFilter"','')
    new_data = new_data.replace('fromGeneratorInterface.Core.ExternalGeneratorFilterimportExternalGeneratorFilter','')
    new_data = new_data.replace('generator=ExternalGeneratorFilter(_generator)','')
    if str(cross_section_fragment).isdigit() is True and (float(cross_section_fragment) == 0 or float(cross_section_fragment) == 1 or float(cross_section_fragment) == -1):
        new_data = new_data.replace('crossSection=cms.untracked.double(0)','')
        new_data = new_data.replace('crossSection=cms.untracked.double(1)','')
        new_data = new_data.replace('crossSection=cms.untracked.double(-1)','')
    return new_data

def vbf_dipole_recoil_check(vbf_lo,vbf_nlo,data_f2,pw_gp,dn):
    dipole_recoil_flag = 0
    dipole_recoil = re.findall('SpaceShower:dipoleRecoil.*?\S+\S+',data_f2)
    warning_dipole = []
    error_dipole = []
    if "vbf" not in dn.lower():
        warning_dipole.append("VBF not in dataset name.")
    if len(dipole_recoil):
        dipole_recoil = dipole_recoil[0].split("=")[1].replace('"', '').replace('\'', '')
        if "on" in dipole_recoil:
            dipole_recoil_flag = 1 
    if pw_gp is False:
        if vbf_lo and dipole_recoil_flag == 0:
            warning_dipole.append("LO VBF with global recoil --> SpaceShower:dipoleRecoil = 0 SMP/HIG groups are moving to local recoil but currently using global recoil. See https://arxiv.org/pdf/1803.07943.pdf")
        if vbf_lo and dipole_recoil_flag:
            print("[OK] LO VBF with local recoil. --> SpaceShower:dipoleRecoil = 1")
        if vbf_nlo and dipole_recoil_flag == 0:
            print("[OK] NLO VBF with global recoil --> SpaceShower:dipoleRecoil = 0")
        if vbf_nlo and dipole_recoil_flag:
            error_dipole.append("NLO VBF with local recoil. --> SpaceShower:dipoleRecoil = 1 aMC@NLO should not be used with local recoil. See https://arxiv.org/pdf/1803.07943.pdf")
    else:
        if "vbf" in dn.lower() and dipole_recoil_flag == 0:
            warning_dipole.append("VBF POWHEG with global recoil --> SpaceShower:dipoleRecoil = 0. See https://arxiv.org/pdf/1803.07943.pdf")
        if "vbf" in dn.lower() and dipole_recoil_flag == 1:
            print("[OK] VBF POWHEG with local recoil --> SpaceShower:dipoleRecoil = 1.")  
    return warning_dipole, error_dipole  


if args.dev:
    print("Running on McM DEV!\n")


def root_requests_from_ticket(ticket_prepid, include_docs=False):
    """
    Return list of all root (first ones in the chain) requests of a ticket.
    By default function returns list of prepids.
    If include_docs is set to True, function will return whole documents
    """
    mccm = get_ticket(ticket_prepid)
    query = ''
    for root_request in mccm.get('requests',[]):
       if isinstance(root_request,str):
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
    print("------------------------------------")
    print("--> Ticket = "+ticket)
    print("------------------------------------")
    prepid = []
    for rr in root_requests_from_ticket(ticket):
        if 'GS' in rr or 'wmLHE' in rr or 'pLHE' in rr or 'FS' in rr: prepid.append(rr)

prepid = list(set(prepid)) #to avoid requests appearing x times if x chains have the same request
print("Current date and time: %s" % (datetime.now().strftime('%Y-%m-%d %H:%M:%S')))
print("Prepid(s):")
for x in prepid:
    print(x)

for num in range(0,len(prepid)):
    res = get_request(prepid[num])
    if len(res) == 0 :
        print("***************************************************************************************")
        print("Something's wrong - can not get the request parameters - check that the request exists.")
        print("***************************************************************************************")
        continue

    my_path =  '/tmp/'+os.environ['USER']+'/gridpacks/'
    print("***********************************************************************************")

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
        total_eff = filter_eff*match_eff 
        cross_section = r['generator_parameters'][-1]['cross_section']
        ext = r['extension']
        print("Extension or not: "+str(ext))
        print(pi+"    Status= "+r['status'])
        print(dn)
        if args.bypass_status and r['status'] != "defined":
            print("--> Skipping since the request is not in defined state")
            print("--> Use --bypass_status option to look at all requests irrespective of state")
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
        mg_nlo = 0
        mcatnlo_flag = 0
        loop_flag = 0
        vbf_lo = 0
        vbf_nlo = 0
        knd =  -1
        slha_flag = 0
        slha_all_path = 'none'
        grid_points_flag = 0
        nPartonsInBorn_flag = 0
        filename_mggpc = 'del'
        ickkw = 'del' # ickkw = matching parameter in madgraph
        ickkw_c = 100
        alt_ickkw_c = 0
        maxjetflavor = 0
        nJetMax = 100
        particle_gun = 0
        randomizedparameters = 0
        tunparmark = 0
        jet_count_tmp = []
        nFinal = 100
        jet_count = 0
        bw = -1
        error = 0
        errors = []
        warning = 0
        warnings = []
        et_flag = 0
        et_flag_external = 0
        bornonly = 0
        herwig_flag = 0
        herwig_count = []
        herwig7_bypass_error = 0
        pythia8_flag = 0
        evtgen_flag = 0
        concornot = 0 
        pf = []
        ppd = 0
        store_rwgt_info_exception = 0 
        if "ppd" in pi.lower(): ppd = 1
        req_type = "dummy"
        if "gen" in pi.lower(): req_type = "genonly"
        if "gs" in pi.lower(): req_type = "gs"
        if "plhe" in pi.lower(): req_type = "plhe"
        if "herwig" in dn.lower(): herwig_flag = 1
        if "evtgen" in dn.lower(): evtgen_flag = 1
        if "comphep" in dn.lower() or "calchep" in dn.lower():
            warnings.append("comphep or calchep request. Please check manually")
            continue
        for item in te:
            timeperevent = float(item)
            print("time per event (sec/event) = "+str(timeperevent))
        if timeperevent > 150.0 :
            warnings.append("Large time/event (> 150 sec)="+str(timeperevent)+" - please check")
        version_not_ok = 0
        if '8_0' in cmssw and "Summer16FSPremix" not in pi: version_not_ok = 1
        if '9_4' in cmssw and "Fall17FSPremix" not in pi: version_not_ok = 1
        if '10_6' not in cmssw and '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw and version_not_ok == 1:
            warnings.append("Are you sure you want to use "+cmssw+" release which is not standard which may not have all the necessary GEN code.")
        if totalevents >= 100000000 :
            warnings.append("Is "+str(totalevents)+" events what you really wanted - please check!")
        os.popen('wget -q '+mcm_link+'public/restapi/requests/get_fragment/'+pi+' -O '+pi).read()

        fsize = os.path.getsize(pi)
        f1 = open(pi,"r")
        f2 = open(pi+"_tmp","w")
        data_f1 = f1.read()

        if int(os.popen('grep -c FlatRandomEGunProducer '+pi).read()) == 1 or int(os.popen('grep -c FlatRandomPtGunProducer '+pi).read()) == 1 or int(os.popen('grep -c Pythia8EGun '+pi).read()) == 1 or int(os.popen('grep -c Pythia8PtGun '+pi).read()) ==1 or int(os.popen('grep -c FlatRandomPtAndDxyGunProducer '+pi).read()): 
            particle_gun = 1
        if int(os.popen('grep -c -i randomizedparameters '+pi).read()) > 0:
            randomizedparameters = 1
#        cmssw_version    = int(re.search("_[0-9]?[0-9]_[0-9]?[0-9]_[0-9]?[0-9]",cmssw).group().replace('_',''))
        cmssw_version    = re.search("_[0-9]?[0-9]_[0-9]?[0-9]_[0-9]?[0-9]",cmssw).group().split("_")
        if len(cmssw_version[1]) != 2 and int(cmssw_version[1]) > 9:
           cmssw_version[1] += "0"
        if len(cmssw_version[2]) != 2:
           cmssw_version[2] += "0"
        if len(cmssw_version[3]) != 2:
           cmssw_version[3] += "0"
        cmssw_version=int(cmssw_version[1]+cmssw_version[2]+cmssw_version[3])
        data_f2 = re.sub(r'(?m)^ *#.*\n?', '',data_f1)
        concurrency_check_exception_list = ["HIG-RunIISummer20UL16GENAPV-00063",
                                            "HIG-RunIISummer20UL16GEN-00072",
                                            "HIG-RunIISummer20UL17GEN-00007",
                                            "HIG-RunIISummer20UL17GEN-00008",
                                            "HIG-RunIISummer20UL17GEN-00009",
                                            "HIG-RunIISummer20UL17GEN-00010",
                                            "HIG-RunIISummer20UL18GEN-00007", 
                                            "HIG-RunIISummer20UL18GEN-00008", 
                                            "HIG-RunIISummer20UL18GEN-00009", 
                                            "HIG-RunIISummer20UL18GEN-00010" 
                                           ]
#        if "SnowmassWinter21GEN" not in pi and "SnowmassWinter21wmLHEGEN" not in pi and particle_gun == 0 and pi not in concurrency_check_exception_list and "matchbox" not in data_f1.lower():
#            conc_check_result, tmp_err = concurrency_check(data_f1,pi,cmssw_version)
#            errors.extend(tmp_err)
#        else:
#            warnings.append("Skipping the concurrency check since these are (wmLHE)GEN-only campaigns or a particle gun or a Sherpa Diphoton sample or an herwig7 request with Matchbox.")

      
        cross_section_fragment = re.findall('crossSection.*?\S+\S+',data_f2)
        if (cross_section_fragment):
            cross_section_fragment=cross_section_fragment[0]
            cross_section_fragment = re.findall('\((.*?)\)',cross_section_fragment)[0]
        filter_eff_fragment = re.findall('filterEfficiency.*?\S+\S+',data_f2)    
        if (filter_eff_fragment):    
            filter_eff_fragment=filter_eff_fragment[0]
            filter_eff_fragment = re.findall('\((.*?)\)',filter_eff_fragment)[0]
        print("Filter efficiency in the fragment ="+ str(filter_eff_fragment))
        print("Cross section in the fragment =" + str(cross_section_fragment) +" pb")
        print("Cross section from generator parameters field = "+str(cross_section)+" pb")
        if not str(cross_section_fragment).isdigit():
            warnings.append("Skipping the cross section consistency check in generator parameters field and the fragment. This is most probably because the cross section is defined through a variable")
        if str(cross_section_fragment).isdigit() is True and cross_section_fragment and cross_section and int(ext) == 0 and float(cross_section_fragment) != float(cross_section):
            errors.append("Cross section in the generator parameters field and the one in the fragment do not match!")
        print("")
        print("Filter efficiency in fragment =" + str(filter_eff_fragment))
        print("Filter efficiency from generator parameters field = "+str(filter_eff))
        # see https://github.com/cms-sw/genproductions/issues/3269
        if len(filter_eff_fragment) > 0 and float(filter_eff_fragment) < 1.0:
            if filter_eff_fragment and filter_eff and int(ext) == 0 and float(filter_eff_fragment) != float(filter_eff):
                errors.append("In general, filter efficiency in the fragment is not taken into accout. Please make sure that the filter efficiency in the generator parameters field is correct!")
	
        # Extension compatibility
        if int(ext) > 0:
           clone_entries = [i for i in r['history'] if i['action'] == 'clone']
           if clone_entries:
               pi_clone_entries = clone_entries[0]['step']
               print(("Request cloned from = ",pi_clone_entries))
               os.popen('wget -q '+mcm_link+'public/restapi/requests/get_fragment/'+pi_clone_entries+' -O '+pi_clone_entries).read()
               f1_clone = open(pi_clone_entries,"r")
               f2_clone = open(pi_clone_entries+"_tmp","w")
               data_f1_clone = f1_clone.read()
               data_f2_clone = re.sub(r'(?m)^ *#.*\n?', '',data_f1_clone)
               data_f2_strip=re.sub(r'\s+', ' ', data_f2).strip()
               data_f2_strip=exception_for_ul_check(data_f2_strip,cross_section_fragment)
               data_f2_clone_strip=re.sub(r'\s+', ' ', data_f2_clone).strip()
               data_f2_clone_strip=exception_for_ul_check(data_f2_clone_strip,cross_section_fragment)
               if (data_f2_strip == data_f2_clone_strip) == True:
                   print("[OK] The base request and the cloned request used for the extension have the same fragment.")
               else:
                   errors.append("The base request "+pi+" and the cloned request "+pi_clone_entries+" used for the extension don't have the same fragment! Here is the diff of the base and and the cloned request: \n"+(os.popen('diff '+pi+' '+pi_clone_entries).read()))
        f1.close()
        f2.write(data_f2)
        f2.close()
        os.system('mkdir -p '+my_path+'/'+pi)
        os.system('mkdir -p '+my_path+'/eos/'+pi)
        os.system('mv '+pi+'_tmp '+pi)
        os.system('cp '+pi+' '+my_path+'/'+pi+'/.')
        os.system('wget -q '+mcm_link+'public/restapi/requests/get_test/'+pi+' -O '+pi+'_get_test')
        gettest = os.popen('grep cff '+pi+'_get_test'+' | grep curl').read()
        if os.path.getsize(pi+'_get_test') == 0:
            print("public/restapi/requests/get_test/ is not acessible for this request. Exiting! Please contact geovanny.gonzalez@cern.ch")
            sys.exit()
        scram_arch = os.popen('grep SCRAM_ARCH '+pi+'_get_test').read()
        scram_arch = scram_arch.split('=')[1].rstrip()
        print("CMSSW release for the request: "+str(cmssw))
        print("scram_arch = "+str(scram_arch))
        ps_version = "/cvmfs/cms.cern.ch/"+str(scram_arch)+"/cms/cmssw"
        if "patch" in cmssw: ps_version = ps_version + "-patch"
        if "pythia8" in dn.lower():
            pythia8_flag = 1
            pythia8_version = ps_version + "/"+str(cmssw)+"/config/toolbox/"+str(scram_arch)+"/tools/selected/pythia8.xml"
            pythia8_version_file = os.path.isfile(pythia8_version)
            pythia8_version = "grep version "+pythia8_version
            if pythia8_version_file is True:
                pythia8_version = os.popen(pythia8_version).read().rstrip().split('=')[2].replace(">","")
                print("PYTHIA8 version = "+str(pythia8_version))
        if "herwig" in dn.lower():
            herwig_version = ps_version + "/"+str(cmssw)+"/config/toolbox/"+str(scram_arch)+"/tools/selected/herwigpp.xml"
            herwig_version_file = os.path.isfile(herwig_version)
            herwig_version = "grep version "+herwig_version
            if herwig_version_file is True:
                herwig_version = os.popen(herwig_version).read().rstrip().split('=')[2].replace(">","")
                print("Herwig version = "+str(herwig_version))
        if "evtgen" in dn.lower():
            evtgen_version = ps_version + "/"+str(cmssw)+"/config/toolbox/"+str(scram_arch)+"/tools/selected/evtgen.xml"
            evtgen_version_file = os.path.isfile(evtgen_version)
            evtgen_version = "grep version "+evtgen_version
            photos_version = ps_version + "/"+str(cmssw)+"/config/toolbox/"+str(scram_arch)+"/tools/selected/photospp.xml"
            photos_version_file = os.path.isfile(photos_version)
            photos_version =  "grep version "+photos_version
            if evtgen_version_file is True:
                evtgen_version = os.popen(evtgen_version).read().rstrip().split('=')[2].replace(">","")
                print("EvtGen version = "+str(evtgen_version))
            if photos_version_file is True:
                photos_version = os.popen(photos_version).read().rstrip().split('=')[2].replace(">","")
                print("PHOTOS version = "+str(photos_version))
        gridpack_cvmfs_path_tmp = os.popen('grep \/cvmfs '+my_path+'/'+pi+'/'+pi).read()
        if int(os.popen('grep -c grid_points '+pi).read()) != 0: grid_points_flag = 1
        gp_size = len(gridpack_cvmfs_path_tmp)

        pw_gp = False
        madloop_in_gp = False
        minlo = False
        minnlo = False
        amcnlo_gp = False
        mg_gp = False
        jhu_gp = False
        sherpa_gp = False
        sherpa_flag = False
        openloops_flag = False
        pw_mg = 0

        gp_full_path = True

        if "sherpacklocation" in gridpack_cvmfs_path_tmp.lower():
            sherpa_flag = True
            tmpsher = re.findall("'.*'",gridpack_cvmfs_path_tmp)[0].replace("'","")
            sherpack_gp = os.path.join(tmpsher,os.listdir(tmpsher)[0])
            print("Sherpack Location = ",sherpack_gp)  
            sp_list = sherpack_gp.split("/")
            scram_sherpa = [s for s in sp_list if "gcc" in s]
            if len(scram_sherpa):
                scram_sherpa = scram_sherpa[0]
                print ("scram_arch for Sherpa = ",scram_sherpa) 
                if scram_sherpa == scram_arch:
                    print ("[OK] scram_arch for Sherpa and CMSSW are the same")
                else:
                    errors.append("scram_arch for Sherpa and CMSSW are NOT the same. But note that this check is done based on folder names except the one for CMSSW")  
            else:
                warnings.append("scram_arch for Sherpa unidentifiable")
            sv_tmp = re.findall("sherpa/.*/",gridpack_cvmfs_path_tmp)[0].split("/")[1].split(".")
            if "v" in sv_tmp[0].lower(): sv_tmp[0] = sv_tmp[0].replace("v","").replace("V","")
            sherpa_version = int(sv_tmp[0])*1000 + int(sv_tmp[1])*100 + int(sv_tmp[2])
            print ("Sherpa Version = ", sherpa_version)
            if sherpa_version < 2211:
                warnings.append("Sherpa older than version 2.2.11")
        if "openloops" in gridpack_cvmfs_path_tmp.lower():
            openloops_flag = True
            OL_list = os.popen('grep openloops '+pi).read().split("/")
            scram_OL = [s for s in OL_list if "gcc" in s]
            if len(scram_OL):
                scram_OL = scram_OL[0]
                print ("scram_arch for OpenLoops = ",scram_OL)
                if scram_OL == scram_sherpa:
                    print ("[OK] scram_arch for Sherpa and OpenLoops are the same")
                else:
                    errors.append("scram_arch for Sherpa and OpenLoops are NOT the same. But note that this check is done based on folder names except the one for CMSSW")

        errors.extend(tunes_settings_check(dn,data_f1,pi,sherpa_flag))

        if gp_size and sherpa_flag == 0:
            gridpack_cvmfs_path_tmp = re.findall("/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.xz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tgz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.gz",gridpack_cvmfs_path_tmp)
            if not gridpack_cvmfs_path_tmp:
                errors.append("Gridpack should be in cvmfs in the dedicated folder location with the full path to the file given. ")
                gp_full_path = False
        if gp_size and gp_full_path and sherpa_flag == 0:
            gridpack_cvmfs_path = gridpack_cvmfs_path_tmp[0]
            gridpack_eos_path = gridpack_cvmfs_path.replace("/cvmfs/cms.cern.ch/phys_generator","/eos/cms/store/group/phys_generator/cvmfs")
            if int(os.popen('grep -c slha '+pi).read()) != 0 or int(os.popen('grep -c \%i '+pi).read()) != 0 or int(os.popen('grep -c \%s '+pi).read()) != 0: slha_flag = 1
            if slha_flag == 1: gridpack_cvmfs_path, slha_all_path, slha_flag = slha_gp(gridpack_cvmfs_path,slha_flag)
            print("-----------------------------------")
            print("Gridpack location in cvmfs and eos:")
            print(gridpack_cvmfs_path)
            print(gridpack_eos_path)
            print("Gridpack size in MBs: "+str(round(os.path.getsize(gridpack_cvmfs_path)/(1024*1024),3))+ " M")
            if os.path.isfile(gridpack_cvmfs_path) is True:
                os.system('tar xf '+gridpack_cvmfs_path+' -C '+my_path+'/'+pi)
                size_after_untar = os.popen("du -h -d 0 "+my_path+'/'+pi).read().split("\t")[0]
                print ("Gridpack folder size after untarring: "+size_after_untar)
                folder = glob.glob(my_path+'/'+pi+'/*')
                folder_and_subfolder = sum([len(files) for r, d, files in os.walk(my_path+'/'+pi)])
                print("Number of files and folders in the gridpack excluding the files in subfolders = "+str(len(folder)))
                print("Number of files and folders in the gridpack including the files in subfolders = "+str(folder_and_subfolder))
                print("-----------------------------------")
            else:
                errors.append("Gridpack ",gridpack_cvmfs_path," does not exist! ..... exiting ....")
                sys.exit()
            jhu_gp = os.path.isfile(my_path+'/'+pi+'/'+'JHUGen.input')
            pw_gp = os.path.isfile(my_path+'/'+pi+'/'+'powheg.input')
            madloop_in_gp = os.path.isfile(my_path+'/'+pi+'/'+'MadLoopParams.dat')
            mg_f1 = my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'
            mg_f2 = my_path+'/'+pi+'/'+'process/Cards/run_card.dat'
            amcnlo_gp = os.path.isfile(my_path+'/'+pi+'/'+'process/Cards/run_card.dat')
            mg_gp = os.path.isfile(mg_f1) or os.path.isfile(mg_f2)
            print("path powheg "+str(pw_gp))
            print("path madloop "+str(madloop_in_gp))
            print("path mg "+str(mg_gp))
            print("path amcnlo "+str(amcnlo_gp))
            print("path jhugen "+str(jhu_gp))           
            if pw_gp is True:
                direc_list = os.listdir(my_path+'/'+pi+'/')
                pw_mg = len([x for x in direc_list if "mg5" in x.lower()])
                print("MG5_aMC + POWHEG sample.")
            if mg_gp is False and "madgraph" in dn.lower():
                errors.append("Although the name of the dataset has ~Madgraph, the gridpack doesn't seem to be a MG5_aMC one.")
            if mg_gp is True:
                errors.extend(tunes_settings_check(dn,data_f1,pi,sherpa_flag))
                filename_mggpc = my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'
                fname_p2 = my_path+'/'+pi+'/'+'process/Cards/run_card.dat'
                if os.path.isfile(fname_p2) is True :
                    filename_mggpc = fname_p2
                #file_run_card = open(filename_mggpc,"r")
                if "Run3" in pi and "PbPb" not in pi:
                    err_tmp = run3_run_card_check(filename_mggpc,pi)
                    errors.extend(err_tmp)
                grep_txt_tmp = 'more '+filename_mggpc+' | tr -s \' \' | grep -c "= ickkw"'
                if int(os.popen(grep_txt_tmp).read()) == 1:
                    alt_ickkw_c = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "= ickkw"').read()
                    alt_ickkw_c = int(re.search(r'\d+',alt_ickkw_c).group())
                    print("MG5 matching/merging: "+str(alt_ickkw_c))
                maxjetflavor = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "= maxjetflavor"').read()
                if len(maxjetflavor) != 0:
                        maxjetflavor = int(re.search(r'\d+',maxjetflavor).group())
                else:
                    warnings.append("maxjetflavor not defined in run_card.dat")
                print("maxjetflavor = "+str(maxjetflavor))
                if alt_ickkw_c == 3:
                    qCutME = os.popen('grep "qCutME" '+pi).read()
                    if len(qCutME) == 0:
                        errors.append("For FxFx setups qCutME should be specified in the fragment.")
                    else:
                        qCutME = qCutME.replace(" ","")
                        qCutME = re.findall('qCutME=\d+',qCutME)[0].split("=")[1]
                        print("qCutME = ",qCutME)
                        ptj_runcard = os.popen('grep "ptj" '+filename_mggpc).read()
                        ptj_runcard = ptj_runcard.replace(" ","")
                        ptj_runcard = re.findall('\d*\.?\d+',ptj_runcard)[0].split("=")[0]
                        print("ptj_runcard =", ptj_runcard)
                        if float(qCutME) != float(ptj_runcard):
                            errors.append("qCutME in PS settings and ptj in run_card in gridpack do not match.")
                    if int(os.popen('grep -c nQmatch '+pi).read()) == 1:
                        nQmatch = os.popen('grep "nQmatch" '+pi).read()
                        nQmatch = nQmatch.replace(" ","")
                        nQmatch = re.findall('nQmatch=\d+',nQmatch)[0].split("=")[1]
                        print("nQmatch = ",nQmatch)
                        if int(nQmatch) != int(maxjetflavor):
                            errors.append("nQmatch in PS settings and maxjetflavor in run_card in gridpack do not match.")
                    else:
                        warnings.append("nQmatch in PS settings is not specified. Please check.") 
#        if herwig_flag == 0 and pw_gp is True:
#            warn_tmp , err_tmp = vbf_dipole_recoil_check(vbf_lo,vbf_nlo,data_f2,pw_gp,dn)
#            warnings.extend(warn_tmp)
#            errors.extend(err_tmp)
        if herwig_flag != 0:
            os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_common.txt -O herwig_common.txt') 
            file1 = set(line.strip().replace(",","") for line in open('herwig_common.txt'))
            for line in file1:
                if line not in data_f1 and ("matchbox" in data_f1.lower() and "hw_7p1SettingsFor7p2" not in line):
                    errors.append("Missing herwig setting in fragment: "+line)
            if pw_gp is True:
               os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_powheg.txt -O herwig_powheg.txt')	
               file_me = set(line.strip().replace(",","") for line in open('herwig_powheg.txt'))
               for line in file_me:
                   if line not in data_f1:
                       errors.append("Missing herwig powheg specific setting in fragment: "+line)
            if mg_gp is True:
               os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_mg.txt -O herwig_mg.txt') 
               file_me = set(line.strip().replace(",","") for line in open('herwig_mg.txt'))
               os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_mg_wo_merging.txt -O herwig_mg_wo_merging.txt')
               file_me_wo_merg = set(line.strip().replace(",","") for line in open('herwig_mg_wo_merging.txt'))
               if alt_ickkw_c != 0:
                   for line in file_me:                
                       if line not in data_f1:
                           errors.append("Missing herwig mg5_amc specific setting in fragment: "+line)
                       if "set FxFxHandler:njetsmax" not in data_f1:
                           errors.append("Missing set FxFxHandler:njetsmax MAX_N_ADDITIONAL_JETS in the user settings block")
               else:
                   for line in file_me_wo_merg:
                           errors.append("Missing herwig mg5_amc specific setting in fragment: "+line)
               if alt_ickkw_c == 3:#fxfx
                   if "'set FxFxHandler:MergeMode FxFx'" not in data_f1:
                       errors.append("Missing set FxFxHandler:MergeMode FxFx in the user settings block")
               if alt_ickkw_c == 1:#mlm
                   if "'set FxFxHandler:MergeMode TreeMG5'" not in data_f1:
                       errors.append("Missing set FxFxHandler:MergeMode TreeMG5 in the user settings block")
            if amcnlo_gp is True or alt_ickkw_c == 0:
               os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_mcnlo.txt -O herwig_mcnlo.txt')
               file_me = set(line.strip().replace(",","") for line in open('herwig_mcnlo.txt'))
               if "Matchbox" in data_f1:
                   n_matchbox = data_f1.lower().count("matchbox") 
                   if "InterfaceMatchboxTest" in data_f1 and n_matchbox != 1:
                       if "hw_lhe_MG5aMCatNLO_settings" in data_f1 or "hw_lhe_common_settings" in data_f1 or "'herwig7LHEMG5aMCatNLOSettingsBlock" in data_f1:
                           errors.append("Extra blocks: 'hw_lhe_MG5aMCatNLO_settings' or 'hw_lhe_common_settings' or 'herwig7LHEMG5aMCatNLOSettingsBlock')")
               for line in file_me:
                   if line not in data_f1 and "Matchbox" not in data_f1 and amcnlo_gp:
                       errors.append("Missing herwig MG with 0 jets or mc@nlo specific setting in fragment: "+line)
            if "9_3" not in str(cmssw) and "7_1" not in str(cmssw) and pw_gp != 0 and mg_gp !=0 and amcnlo_qg !=0:
                os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_frag_lines.txt -O herwig_frag_lines.txt')
                file1 = set(line.strip().replace(",","")  for line in open('herwig_frag_lines.txt'))
                herwig_check = []
                herwig_psweight_tag = 0
                for line in data_f1: print(line)
                print("-----")	 
                for line in file1:
                    print(line)
                    if line not in data_f1:
                        herwig_check.append(line)	
                if len(herwig_check) != 0 and "eec5" not in dn.lower() and "ee5c" not in dn.lower():
                    herwig_count.append(herwig_check[0].count('hw_lhe_common_settings'))
                    herwig_count.append(herwig_check[1].count('herwig7LHECommonSettingsBlock'))
                    herwig_count.append(herwig_check[2].count('from Configuration.Generator.Herwig7Settings.Herwig7LHECommonSettings_cfi import *'))
                    if all(x == 1 for x in herwig_count) and any("insert SubProcess:MatrixElements" in x for x in list(data_f1)):
                        herwig7_bypass_error = 1
                    if "PSWeights" not in herwig_check:
                        herwig_psweight_tag = 1
                        warnings.append("Parton shower weights are missing in the Herwig7 fragment.")
                    if herwig7_bypass_error == 0 and herwig_psweight_tag == 0:
                        errors.append(str(len(herwig_check)) + " missing fragment line(s) for herwig: lines for internal matrix element are missing in the fragment."+herwig_check)
        if fsize == 0:
            warnings.append("No fragment associated to this request. Is this the hadronizer you intended to use?: "+gettest)
        ttxt = os.popen('grep nThreads '+pi+'_get_test').read()
        ntread_new = 1
        if not ttxt:
            ttxt = os.popen('grep "# Threads for each sequence" '+pi+'_get_test').read()	
            print(ttxt)
            nthreads = int(re.search(r'\d+',ttxt).group())
            if not nthreads: ntread_new = 0
        if ntread_new == 0:
            if int(os.popen('grep -c nThreads '+pi+'_get_test').read()) == 0 :
                nthreads = 1
            else :
                nthreads = int(re.search('nThreads(.*?) --',ttxt).group(1))

        if "SnowmassWinter21GEN" not in pi and "SnowmassWinter21wmLHEGEN" not in pi and particle_gun == 0 and pi not in concurrency_check_exception_list and "matchbox" not in data_f1.lower():
            conc_check_result, tmp_err = concurrency_check(data_f1,pi,cmssw_version,mg_gp)
            errors.extend(tmp_err)
        else:
            warnings.append("Skipping the concurrency check since these are (wmLHE)GEN-only campaigns or a particle gun or a Sherpa Diphoton sample or an herwig7 request with Matchbox.")
                
        nevts = 100.
        if timeperevent > 0:   
            nevts = (8*3600/timeperevent)*total_eff
            print("Expected number of events = "+str(nevts))
        if any(word in dn for word in MEname) and gp_size == 0 and "plhe" not in pi.lower():
            errors.append("gridpack path is not properly specified - most probable reason is that it is not a cvmfs path.")
        if "sherpa" in dn.lower():
            warnings.append("Not checking sherpacks in too much detail for now. Please do independent tests.")
        if fsize != 0:
            if int(os.popen('grep -c nPartonsInBorn '+pi).read()) == 1:
                nPartonsInBorn_flag = 1
                print((os.popen('grep nPartonsInBorn '+pi).read()))
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
                if int(os.popen('grep -c hw_PSWeights_settings '+pi).read()) == 1:
                    warnings.append("hw_PSWeights_settings in fragment. These are currently affected by an issue of a very large variance in weights (https://indico.cern.ch/event/1282424/contributions/5392893/attachments/2642930/4574147/presentation.pdf), so we do not recommend including them unless you want to do some particular studies.")
            if int(os.popen('grep -c nFinal '+pi).read()) == 1:
                nFinal = os.popen('grep nFinal '+pi).read()
                if grid_points_flag == 1:
                    nFinal = re.findall('nFinal = \d+',nFinal)
                    nFinal = nFinal[0]
                nFinal =  re.findall('\d+',nFinal)
                nFinal = int(nFinal[0])
                print("nFinal="+str(nFinal))
            if int(test_cs_version[2]) == 6 and ('CMSSW_10_6_0' not in cmssw or 'CMSSW_10_6_0_patch1' not in cmssw): tunparmark = 1
            if int(test_cs_version[1]) == 10 and int(test_cs_version[2]) >= 5 and int(test_cs_version[2]) <= 6 and int(test_cs_version[3]) >= 0 and '10_5_0_pre1' not in cmssw and particle_gun == 0 and tunparmark == 0 and herwig_flag == 0:
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
                    errors.append("SigmaTotal:mode is missing. For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1. SigmaTotal:mode shoud be added by hand and set to 0")
                    tmp_flag = 1
                if len(mb_SigmaEl) == 0:
                    errors.append("SigmaTotal:sigmaEl is missing.  For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1. SigmaTotal:sigmaEl should be added by hand and set to 21.89")
                    tmp_flag = 1
                if len(mb_SigmaTot) == 0:
                    errors.append("SigmaTotal:sigmaTot is missing. For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1. SigmaTotal:sigmaTot should be added by hand and set to 100.309")
                    tmp_flag = 1
                if len(PDF_pSet_test) == 0:
                    warnings.append("PDF:pSet is missing (if you want to use NNPDF3.1). For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1.  PDF access method should be like. e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'")
                    tmp_flag = 1
                if tmp_flag == 0:
                    if int(mb_mode[0]) != 0:
                        errors.append("SigmaTotal:mode should have been set to 0")
                    if abs(float(mb_SigmaEl[0])-21.88) > 0.1:
                        errors.append("SigmaTotal:sigmaEl should have been set to 21.89")
                    if abs(float(mb_SigmaTot[0])-100.308) > 0.01:
                        errors.append("SigmaTotal:sigmaTot should have been set to 100.309")
                    if int(PDF_pSet[0]) != 1:
                        warnings.append("PDF access method is wrong (if you want to use NNPDF3.1). Please correct: e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'")

            if gp_size != 0:
                if "ppd" not in pi.lower() and "Summer20UL17pp5TeV" not in pi:
                    w_temp, e_temp = ul_consistency(dn,pi,jhu_gp)
                    warnings.extend(w_temp)
                    errors.extend(e_temp)
                if "fall18" not in pi.lower() and "fall17" not in pi.lower() and "winter15" not in pi.lower() and "summer15" not in pi.lower() and not (any(word in dn for word in tunename) or "sherpa" in dn.lower() or ("herwigpp" in dn.lower() and ("eec5" in dn.lower() or "ee5c" in dn.lower()))):
                    errors.append("Dataset name does not have the tune name: "+dn)
                if "fall18" not in pi.lower() and "fall17" not in pi.lower() and "winter15" not in pi.lower() and "summer15" not in pi.lower() and not any(word in dn.lower() for word in psname):
                    errors.append("Dataset name does not contain a parton shower code name: "+dn)
                if not any(word in dn.lower() for word in MEname):
                    warnings.append("Dataset name is not regular:"+dn+"          Please add the Generator name to the dataset.")
                    if pw_gp is True: dn = dn + "-powheg"
                    if mg_gp is True: dn = dn + "-madgraph"
                    if jhu_gp is True: dn = dn + "-jhugen"
                    if amcnlo_gp is True:
                        if alt_ickkw_c == 0: dn = dn + "-amcatnlo"
                        if alt_ickkw_c == 3: dn = dn + "-amcatnloFXFX"
                gp_log_loc = my_path+'/'+pi+'/gridpack_generation.log'
                if os.path.isfile(gp_log_loc) is False and jhu_gp is False and sherpa_flag is False:
                    warnings.append("No gridpack generation.log")
                elif (mg_gp is True or amcnlo_gp is True) and os.path.isfile(gp_log_loc) is True:
                    pf.append(os.popen('grep \"saving rejects to\" '+gp_log_loc).read())
                    pf.append(os.popen('grep \"INFO: fail to reach target\" '+gp_log_loc).read())
                    pf.append(os.popen('grep \"INFO: Not enough events for at least one production mode\" '+gp_log_loc).read())
                    if len(pf[0]) != 0:
                        warnings.append(pf[0]+"Gridpack PATCH problem! Please use the master branch of genproductions!")
                    if len(pf[1]) !=0 or len(pf[2]) != 0:
                        warnings.append(pf[1]+"          "+pf[2]+"          You may try to request more events per phase-space region in the gridpack.")
                if os.path.isfile(gp_log_loc) is True and ('madgraph' in dn.lower() or 'amcatnlo' in dn.lower()):
                    print("------------------------------------------------------------------------------------")
                    print("Summary for madgraph for experts fron gridpack log (cross section BEFORE matching (if there is matching/merging)):")
                    print(os.popen('grep Summary '+gp_log_loc+' -A 5 -B 1').read())
                    print("------------------------------------------------------------------------------------")
                if mg_gp is True:
                    dir_path = os.path.join(my_path,pi,"InputCards")
                    if os.path.isdir(dir_path):
                        input_cards_customize_card = find_file(dir_path,"customizecards.dat")
                        input_patch = find_file(dir_path,"patch")
                        if input_patch:
                            print("Checking running Yukawa coupling:")
                            print("input patch file: "+input_patch)
                            with open(input_patch, 'r+') as f_patch:
                                for line in f_patch.readlines():
                                    if "runfac" in line and "integer,parameter" in line:
                                        store_rwgt_info_exception = line.split("::")[1].split("=")[1]   
                                        store_rwgt_info_exception = re.sub(r'(?m)^ *#.*\n?', '',store_rwgt_info_exception)
                                        store_rwgt_info_exception = int(store_rwgt_info_exception)
                                        if store_rwgt_info_exception:
                                            print("store_rwgt_info_exception="+str(store_rwgt_info_exception)+"--> See https://cms-talk.web.cern.ch/t/validate-requests-with-store-rwgt-info-false-in-gridpacks/12417\n")
                        if input_cards_customize_card:
                            c_w_line = []
                            s_line = []
                            run_card_line = []
                            with open(input_cards_customize_card, 'r+') as f_cust:
                                for num, lc in enumerate(f_cust, 0):
                                    if "compute_widths " in lc.lower(): c_w_line.append(num)
                                    if "set " in lc.lower(): s_line.append(num)
                                    if "run_card" in lc.lower(): run_card_line.append(lc.rstrip())
                                if (run_card_line):
                                    print("-------")
                                    print("User settings in customize card for run_card:")
                                    for lll in run_card_line:
                                        print(lll)
                                    print("-------")
                            customize_widths_flag = 0
                            if len(c_w_line) > 0 and len(s_line) > 0:
                                for x in c_w_line:
                                    for y in s_line:
                                        if int(x) < int(y): customize_widths_flag = 1
                            if customize_widths_flag > 0:
                                errors.append("COMPUTE_WIDTHS followed by SET command(s) should not be used in customizecards. Instead use \"set width X auto\" to compute the widths for X and change the parameter card settings.")  
                        input_cards_reweight_card = find_file(dir_path,"reweight_card.dat")
                        if input_cards_reweight_card:
                            reweights = os.popen('more '+input_cards_reweight_card+' | tr -s \' \' | grep "rwgt_name"').read()
                            reweights = re.sub("launch --rwgt_name",'',reweights)
                            reweights = re.sub("=",'',reweights)
                            #There is an xml check in the gridpack generation for this. 
                            #In principle we could drop the tests for the characters below if we assume all gridpacks start from master. 
                            #However, this might not always be true, so the check stays but if needed it can be made looser. 
                            chars_to_check = set('@#$%^&*()+-[]{}\ ')
                            chars_to_check_warning = set('.')
                            if any((chars in chars_to_check) for chars in reweights):
                                errors.append("Please remove problematic characters (at least one of @#$%^&*()+-[]{}.\) from rwgt_names. See https://github.com/cms-sw/genproductions/blob/master/bin/MadGraph5_aMCatNLO/gridpack_generation.sh#L102. This causes the header in mg5 to be corrupted and nano-aod will not work.") 
                            if any((chars in chars_to_check_warning) for chars in reweights):
                                warnings.append('The existence of a "." in reweight_card will result in the name of the weight not to be shown in the header. Please make sure if this is a problem for your analysis, if not, please remove the dot')
                if mg_gp is True:
                    if alt_ickkw_c == 3 and pythia8_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+filename_mggpc).read()
                        if "PYTHIA8" not in ps_hw.upper():
                            errors.append("PYTHIA8 = parton_shower not in run_card.dat")
                    if alt_ickkw_c == 3 and herwig_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+filename_mggpc).read()
                        if ("HERWIGPP" not in ps_hw.upper()) or ("HERWIG7" not in ps_hw.upper() and herwig7_bypass_error == 1):
                            errors.append("HERWIGPP or HERWIG7 = parton_shower not in run_card.dat")
                        if int(os.popen('grep -c "set FxFxHandler:MergeMode FxFx" '+pi).read()) == 0:
                            errors.append("Please add \'set FxFxHandler:MergeMode FxFx\' and set FxFxHandler:njetsmax to the number of additional partons in the proc_card")
                    if alt_ickkw_c == 2 and herwig_flag != 0:
                        if int(os.popen('grep -c herwig7CommonMergingSettingsBlock').read()) == 0:
                            errors.append("Please load herwig7CommonMergingSettingsBlock")
                if amcnlo_gp is True:
                    if pythia8_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+my_path+'/'+pi+'/'+'process/Cards/run_card.dat').read()
                        if "PYTHIA8" not in ps_hw.upper():
                            errors.append("PYTHIA8 = parton_shower not in run_card.dat")
                    if herwig_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+my_path+'/'+pi+'/'+'process/Cards/run_card.dat').read()
                        if ("HERWIGPP" not in ps_hw.upper()) or ("HERWIG7" not in ps_hw.upper() and herwig7_bypass_error == 1):
                            errors.append("HERWIGPP or HERWIG7 = parton_shower not in run_card.dat")
        
        if "jhugen" in dn.lower() and gp_size != 0 and "plhe" not in pi.lower():
            jhufilename = "NULL"
            for root, dirs, files in os.walk(os.path.join(my_path, pi, "."), topdown=False):
                for name in files:
                    if "JHUGen.input" in name:
                        print("Found the JHUGen input file: "+os.path.join(root, name))
                        jhufilename = os.path.join(root, name)
                for name in dirs:
                    if "JHUGen.input" in name:
                        print("Found the JHUGen input file: "+os.path.join(root, name))
                        jhufilename = os.path.join(root, name)
            if os.path.isfile(jhufilename) is True and pw_gp is False:
                with open(jhufilename) as f:
                    jhu_in = f.read()
                    jhu_in = re.sub(r'(?m)^ *#.*\n?', '',jhu_in)
                    jhu_pdf = re.findall('LHAPDF=\S+',jhu_in)
                    if jhu_pdf: jhu_pdf = jhu_pdf[0].split('=')[1].split('/')[1]
                    print("The PDF set used by JHUGEN is:"+ str(jhu_pdf))
                    if "UL" in pi and jhu_pdf not in UL_PDFs:
                        warnings.append("The gridpack uses PDF = "+str(jhu_pdf)+" but not the recommended sets for UL requests:     "+str(UL_PDFs_N[0])+" "+str(UL_PDFs[0])+"                               or "+str(UL_PDFs_N[1])+" "+str(UL_PDFs[1]))
            if os.path.isfile(jhufilename) is True and pw_gp is True:
                with open(jhufilename) as f:
                    jhu_in = f.read()
                    jhu_in = re.sub(r'(?m)^ *#.*\n?', '',jhu_in)
                    jhu_wfe = str(re.findall(r'(WriteFailedEvents.*(?=))',jhu_in))
                    if (not jhu_wfe or jhu_wfe.isspace()) or (jhu_wfe and not jhu_wfe.isspace() and "2" not in jhu_wfe): 
                        errors.append("WriteFailedEvents should be set to 2 in JHUGen.input in jhugen+powheg samples.")
                    else:
                        WriteFailedEvents_flag = 1
                        print("[OK] "+str(jhu_wfe)+" for this jhugen+powheg sample.")

        if pw_gp is True or mg_gp is True or amcnlo_gp is True:
            if pw_gp is True:
                word = "PowhegEmissionVeto"
            else:
                word = "aMCatNLO"
            check.append(int(os.popen('grep -c pythia8'+word+'Settings '+pi).read()))
            check.append(int(os.popen('grep -c "from Configuration.Generator.Pythia8'+word+'Settings_cfi import *" '+pi).read()))
            check.append(int(os.popen('grep -c "pythia8'+word+'SettingsBlock," '+pi).read()))
            if check[2] == 1: mcatnlo_flag = 1
        if pw_gp is True:
            split_dp_gpf = 'del'
            file_pwg_check =  my_path+'/'+pi+'/'+'pwhg_checklimits'
            print(file_pwg_check)
            split_dp = gridpack_cvmfs_path.split("/")
            for i in split_dp:
                if ("slc" and "CMSSW") in i: split_dp_gpf = i
            if ((split_dp_gpf.startswith("Z") or split_dp_gpf.startswith("gg_H")) and nFinal != 1) or ((split_dp_gpf.startswith("HJJ") or split_dp_gpf.startswith("ttH") or split_dp_gpf.startswith("HZJ") or split_dp_gpf.startswith("HWJ")) and nFinal!= 3) or (split_dp_gpf.startswith("ggHZ") and nFinal!=2):
                warnings.append("nFinal="+str(nFinal) + " may not be equal to the number of final state particles before decays)")
            pw_processes = 'dy','ggh','glugluh','tth','hzj','hwj','ggzh'
            if not any(i in dn.lower() for i in pw_processes):
                warnings.append("Please check manually if nFinal="+str(nFinal) + " for this process is OK, i.e. equal to the number of final state particles before decays) ")
#           if os.path.isfile(file_pwg_check) is True :
#                print("grep from powheg pwhg_checklimits files")
#                nemit = os.popen('grep emitter '+file_pwg_check+' | grep process | head -n 1').read().replace('process','').replace('\n','').split(',')
#                nemitsplit = nemit[1].split()
#                nemitsplit_pr = nemitsplit[2:]
#                nemitsplit = [x for x in nemitsplit_pr if x!=nemitsplit[0] and x!=nemitsplit[1]]
#                nemitsplit = [100 if x == "***" else x for x in nemitsplit]
#                nemitsplit_wo_leptons = [int(x) for x in nemitsplit]
#                nemitsplit_wo_leptons = [abs(x) for x in nemitsplit_wo_leptons]
#                nemitsplit_wo_leptons = [x for x in nemitsplit_wo_leptons if x < 11 or x > 18]
#                nfinstatpar = len(nemitsplit_wo_leptons)-nemitsplit_wo_leptons.count(0)                
#                if nfinstatpar == nFinal : print("[OK] nFinal(="+str(nFinal) + ") is equal to the number of final state particles before decays (="+str(nfinstatpar)+")")
#                if nfinstatpar != nFinal :
#                    warnings.append("nFinal(="+str(nFinal) + ") may not be equal to the number of final state particles before decays (="+str(nfinstatpar)+")")
            if os.path.isfile(my_path+'/'+pi+'/'+'runcmsgrid.sh') is True: 
                runcmsgrid_file = my_path+'/'+pi+'/'+'runcmsgrid.sh'
                with open(runcmsgrid_file,'r+') as f:
                    content = f.read()
                    errors.extend(check_replace(runcmsgrid_file))
                    match = re.search(r"""process=(["']?)([^"']*)\1""", content)
                    print(match.group(0))
                    warning1,error1 = xml_check_and_patch(f,content,gridpack_eos_path,my_path,pi)
                    warnings.extend(warning1)
                    errors.extend(error1)
                    
                    f.close()
            else:
                errors.append(my_path+'/'+pi+'/'+'runcmsgrid.sh does not exists')
            if os.path.isfile(my_path+'/'+pi+'/'+'external_tarball/runcmsgrid.sh') is True:
                runcmsgrid_file = my_path+'/'+pi+'/'+'external_tarball/runcmsgrid.sh'
                with open(runcmsgrid_file,'r+') as f2:
                    content2 = f2.read()
                    errors.extend(check_replace(runcmsgrid_file))
                    match = re.search(r"""process=(["']?)([^"']*)\1""", content2)
                    warning1,error1 = xml_check_and_patch(f2,content2,gridpack_eos_path,my_path,pi)
                    errors.extend(error1)
                    warnings.extend(warning1) 
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
            if et_flag == 0 and et_flag_external == 0: powheg_input = os.path.join(my_path, pi, "powheg.input")
            if et_flag == 1 and et_flag_external == 0: powheg_input = os.path.join(my_path, pi, "external_tarball/powheg.input")
            if os.path.isfile(powheg_input) is True:
                pw_pdf = 0
                with open(powheg_input) as f:
                    for line in f:
                        if line.startswith("!") == False and line.startswith("#") == False:
                            if "bornonly" in line: bornonly = int(re.split(r'\s+',line)[1])
                            if "lhans1" in line:
                                pw_pdf = int(re.split(r'\s+', line)[1])
                                print("Powheg PDF used is: "+str(pw_pdf))
                                if "UL" in pi and pw_pdf not in UL_PDFs_N:
                                    warnings.append("The gridpack uses PDF="+str(pw_pdf)+" but not the recommended sets for UL requests:  "+str(UL_PDFs_N[0])+" "+str(UL_PDFs[0])+"   or "+str(UL_PDFs_N[1])+" "+str(UL_PDFs[1]))
                            if "minlo" in line and "modlog_p" not in line:
                                minlo = int(re.split(r'\s+', line)[1])
                                print("MINLO = "+str(minlo))
                            if "minnlo" in line and "modlog_p" not in line:
                                minnlo = int(re.split(r'\s+', line)[1])
                                print("MINNLO = "+str(minnlo))
            if os.path.isfile(my_path+'/'+pi+'/'+'external_tarball/pwg-stst.dat') is True:
                pwg_stat_file = os.path.join(my_path, pi, "external_tarball/pwg-stat.dat")
            else:
                pwg_stat_file = os.path.join(my_path, pi, "pwg-stat.dat")
            if os.path.isfile(pwg_stat_file):
                with open(pwg_stat_file) as f_pwg_stat: 
                    s_pwg_stat = f_pwg_stat.read()
                    print("-----------------------------------------------------------------")
                    print("Summary from pwg-stat.dat from Powheg firdpack (for experts only):")
                    print("-----------------------------------------------------------------")
                    print(s_pwg_stat)
                    print("-----------------------------------------------------------------")
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
                        if "scale_variation" in line: scale_var_check0 += 1
                        if "renscfact" in line and "facscfact" in line: scale_var_check1 += 1
                        if "PDF_variation" in line: pdf_var_check0 += 1
                        if str(pw_pdf+1) in line: pdf_var_check1 += 1
                    if not (scale_var_check0 == 1 and scale_var_check1 == 9):
                        warnings.append("There may be a problem with scale variations. Please check pwg-rwl.dat")
                    if not (pdf_var_check0 > 0 and pdf_var_check1 >= 1):
                        errors.append("There may be a problem with PDF variations. Please check pwg-rwl.dat")
            if bornonly == 1:
                bornonly_frag_check = 0
                if int(os.popen('grep -c "Pythia8PowhegEmissionVetoSettings" '+pi).read()) == 1: bornonly_frag_check = 1
                if int(os.popen('grep -c "SpaceShower:pTmaxMatch" '+pi).read()) == 1: bornonly_frag_check = 1
                if int(os.popen('grep -c "TimeShower:pTmaxMatch" '+pi).read()) == 1: bornonly_frag_check = 1
                if bornonly_frag_check != 0:
                    errors.append("bornonly = 1 and (Pythia8PowhegEmissionVetoSettings or SpaceShower:pTmaxMatch or  TimeShower:pTmaxMatch)")
                else:
                    warnings.append("bornonly = ",bornonly)
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
                    for name, match in matches.items():
                        if match:
                            actualvalue = int(match.group(1))
                            if actualvalue < desiredvalues[name]:
                                bad = True
                                warnings.append("{0} = {1}, should be at least {2} (may be ok if hmass < 150 GeV, please check!)".format(name, actualvalue, desiredvalues[name]))
                        else:
                            bad = True
                            errors.append("didn't find "+name+" in powheg.input")
                    if not bad: print("[OK] integration grid setup looks ok for gg_H_quark-mass-effects")
            else:
                warnings.append("Didn't find powheg process in runcmsgrid.sh")

        if mg_gp is True or amcnlo_gp is True:
            if gp_size == 0: break
            bbmark = 0
            filename_pc = my_path+'/'+pi+'/'+'process/madevent/Cards/proc_card_mg5.dat'
            fname_p2 = my_path+'/'+pi+'/'+'process/Cards/proc_card.dat'
            fname_p3 = my_path+'/'+pi+'/'+'process/Cards/proc_card_mg5.dat'
            if os.path.isfile(fname_p2) is True : filename_pc = fname_p2
            if os.path.isfile(fname_p3) is True : filename_pc = fname_p3
            if os.path.isfile(filename_pc) is True :
                mg_nlo = int(os.popen('grep -c "\[QCD\]" '+filename_pc).read())
                loop_flag = int(os.popen('more '+filename_pc+' | grep -c "noborn=QCD"').read())
                gen_line = os.popen('grep generate '+filename_pc).read()
                bbmark = int(os.popen('grep -c "define bb = b b~" '+filename_pc).read())
                print("Process lines from the proc card:")
                print(gen_line)
                proc_line = os.popen('grep process '+filename_pc+' | grep -v set').read()
                print(proc_line)
                proc_line = gen_line.replace('generate','') + "\n" + proc_line 
                print("Simplified process lines:")
                if (gen_line.count('@') > 0 and gen_line.count('@') <= proc_line.count('@')) or (proc_line.count('add') > 0):
                    proc_line = proc_line.split('add process')
                    print(proc_line)
                    bmark = 0
                    for y in range(0,len(proc_line)):
                        if "define bb = b b~" in proc_line[y]: bmark = 1  
                        if proc_line[y].startswith("set"): continue
                        zz = proc_line[y] 
                        if "," in proc_line[y]: zz = proc_line[y].split(',')[0]
                        zz = zz.translate(str.maketrans('','',string.punctuation))
                        nbtomatch = zz.count('b') if maxjetflavor > 4 else 0
                        nc = zz.count('c') if "chi" not in zz else 0
                        if "excl" in zz and nc != 0: nc = nc -1
                        if bbmark == 1 and nbtomatch > 1: nbtomatch = nbtomatch - 1
                        jet_count_tmp.append(zz.count('j') + nbtomatch + nc)
                    jet_count = max(jet_count_tmp)
                else:
                    jet_line = gen_line.replace('generate','')
                    jet_count = jet_line.count('j') + jet_line.count('b') + jet_line.count('c')
                if nJetMax == jet_count: print("[OK] nJetMax(="+str(nJetMax) + ") is equal to the number of jets in the process(="+str(jet_count)+")")
                if nJetMax != jet_count and gen_line.count('@') != 0 and alt_ickkw_c !=0:
                    warnings.append("nJetMax(="+str(nJetMax)+") is NOT equal to the number of jets specified in the proc card(="+str(jet_count)+")")
                if nJetMax != jet_count and jet_count > 0 and alt_ickkw_c !=0:
                    warnings.append("nJetMax(="+str(nJetMax)+") is NOT equal to the number of jets specified in the proc card(="+str(jet_count)+")")
                if nJetMax != jet_count and str(jet_count)+"jet" in dn.lower() and alt_ickkw_c !=0:
                    warnings.append("nJetMax(="+str(nJetMax)+") is not equal to the number of jets specified in the proc card(="+str(jet_count)+"). Is it because this is an exclusive production with additional samples with higher multiplicity generated separately?")
                print("Jet Count = "+str(jet_count))
                if jet_count >= 2 and "dy" not in dn.lower():
                    if alt_ickkw_c == 0:
                        if mg_nlo:
                            vbf_nlo = 1
                            print("VBF process at NLO")
                    else:
                        vbf_lo = 1   
                        print("VBF process at LO")
                    warn_tmp , err_tmp = vbf_dipole_recoil_check(vbf_lo,vbf_nlo,data_f2,pw_gp,dn)
                    warnings.extend(warn_tmp)
                    errors.extend(err_tmp)
            if os.path.isfile(filename_mggpc) is True :
                ickkw = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "= ickkw"').read()
                bw = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "= bwcutoff"').read()
                mg_pdf = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "lhaid"').read()
                mg_pdf = mg_pdf.split("=")[0].split()[0]
                test_bw = bw.split()
                if float(test_bw[0]) > 15.:
                    warnings.append("bwcutoff set to "+str(test_bw[0])+". Note that large bwcutoff values can cause problems in production.")
                print("The MG5_aMC PDF set is:"+str(mg_pdf))
                if "UL" in pi and int(mg_pdf) != UL_PDFs_N[0] and int(mg_pdf) != UL_PDFs_N[1]:
                    warnings.append("The gridpack uses PDF="+str(mg_pdf)+" but not the recommended sets for UL requests:       "+str(UL_PDFs_N[0])+" "+str(UL_PDFs[0])+"     or "+str(UL_PDFs_N[1])+" "+str(UL_PDFs[1]))
            version_file = my_path+'/'+pi+'/'+'mgbasedir/VERSION'
            if os.path.isfile(version_file) is True:
                mgversion_tmp = os.popen('grep version '+version_file).read()
                mgversion = mgversion_tmp.split()
                mgversion = mgversion[2].split(".")
                mgversion_tmp = mgversion_tmp.split("\n")
                print("The gridpack is made with mg5_aMC version:"+str(mgversion_tmp[0]))   
                mg5_aMC_version = float(mgversion[0])*100/float(pow(10,len(str(int(mgversion[0])))-1)) + float(mgversion[1])*10/float(pow(10,len(str(int(mgversion[1])))-1)) + float(mgversion[2])/float(pow(10,len(str(int(mgversion[2])))-1))
                if "UL" in pi and mg5_aMC_version < 261:
                    if "PPD" in pi:
                        warnings.append("You're using MG5_aMC "+str(mg5_aMC_version)+" in an Ultra Legacy Campaign. You should use MG5_aMCv2.6.1+")
                    else:
                        errors.append("You're using MG5_aMC "+str(mg5_aMC_version)+" in an Ultra Legacy Campaign. You should use MG5_aMCv2.6.1+")

            if herwig_flag == 0 and pw_gp is True:
                warn_tmp , err_tmp = vbf_dipole_recoil_check(vbf_lo,vbf_nlo,data_f2,pw_gp,dn)
                warnings.extend(warn_tmp)
                errors.extend(err_tmp)

            if mg_gp is True:
                runcmsgrid_file = os.path.join(my_path, pi, "runcmsgrid.sh")
                with open(runcmsgrid_file) as fmg:
                    fmg_f = fmg.read()
                    errors.extend(check_replace(runcmsgrid_file))
                    fmg_f = re.sub(r'(?m)^ *#.*\n?', '',fmg_f)
                    mg_me_pdf_list = re.findall('pdfsets=\S+',fmg_f)
                    if mg5_aMC_version >= 260:
                        mg_lo = int(os.popen('grep "systematics" '+str(runcmsgrid_file)+' | grep -c madevent').read())
                        mg_nlo = int(os.popen('grep "systematics" '+str(runcmsgrid_file)+' | grep -c aMCatNLO').read())
                    if mg5_aMC_version < 260:
                        mg_lo = int(os.popen('grep -c syscalc '+str(runcmsgrid_file)).read())
                        if mg_nlo > 0:
                            if mg5_aMC_version < 242:
                                warnings.append("No automated PDF check for this version.")
                                continue
                            r_scale = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "reweight_scale"').read()
                            r_scale = r_scale.split()[0]#.split('.')[1]
                            if "." in r_scale:
                                r_scale = r_scale.split('.')[1]
                            if len(r_scale) == 0 or "true" not in str(r_scale).lower():
                                errors.append("For NLO MG5_aMC version < 260, one should have .true. = reweight_scale")
                            dir_path = os.path.join(my_path, pi, "InputCards")
                            input_cards_run_card = find_file(dir_path,"run_card.dat")
                            r_pdf = os.popen('more '+str(input_cards_run_card)+' | tr -s \' \' | grep "reweight_PDF"').read()
                            r_pdf = r_pdf.split()[0]
                            if len(r_pdf) == 0 or "$DEFAULT_PDF_MEMBERS" not in r_pdf:
                                errors.append("For NLO MG5_aMC version < 260, one should have $DEFAULT_PDF_MEMBERS = reweight_PDF")
                    if mg_lo > 0 and mg_nlo > 0:
                        errors.append("something's wrong - LO and NLO configs together.")
                    if mg_lo > 0: print("The MG5_aMC ME is running at LO")
                    if mg_nlo > 0: print("The MG5_aMC ME is running at NLO")
                    if mg_nlo > 0 and mg5_aMC_version >= 260:
                        if os.path.isfile(filename_mggpc) is True : store_rwgt_info = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "store_rwgt_info"').read()
                        print("store_rwgt_info_exception ="+str(store_rwgt_info_exception))
                        if len(store_rwgt_info) != 0:
                            store_rwgt_info_a = store_rwgt_info.split('=')
                            if store_rwgt_info_exception == 1:
                                print("Running Yukawa coupling: Skipping store_rwgt_info check")
                            elif "false" in store_rwgt_info_a[0].lower():
                                errors.append("store_rwgt_info set to"+ str(store_rwgt_info_a[0]) +" for MG5_aMC >= 260. This is needed to evaluate systematics. See eg. https://hypernews.cern.ch/HyperNews/CMS/get/generators/4513/1/1/1/1/1/2.html")
                        if len(store_rwgt_info) == 0:
                            errors.append("No store_rwgt_info set for MG5_aMC >= 260. This is needed to evaluate systematics. See eg. https://hypernews.cern.ch/HyperNews/CMS/get/generators/4513/1/1/1/1/1/2.html")
                    if mg_lo > 0 and mg5_aMC_version >= 260:
                        if os.path.isfile(filename_mggpc) is True : use_syst = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "use_syst"').read()
                        if len(use_syst) != 0:
                            use_syst_a = use_syst.split('=')
                            if "false" in use_syst_a[0].lower():
                                errors.append("use_syst set to"+ str(use_syst_a[0]) +" for MG5_aMC >= 260.")
                        if len(use_syst) == 0:
                            errors.append("No use_syst set for MG5_aMC >= 260.")

                    if mg5_aMC_version < 260: continue
                    mg_me_pdf_list = mg_me_pdf_list[0].split('=')[1].split('\"')[1].split(',')
                    var_count = [s for s in mg_me_pdf_list if "@0" in s]
                    if len(var_count) < 1:
                        warnings.append("There will be no PDF variations! Please check the runcmsgrid file in the gridpack.")
                    if "UL" in pi and mg_me_pdf_list.count(str(UL_PDFs_N[0])) != 1 and mg_me_pdf_list.count(str(UL_PDFs_N[1])) != 1:
                        if mg_me_pdf_list.count(str(UL_PDFs_N[0])) > 1 or mg_me_pdf_list.count(str(UL_PDFs_N[1])) > 1:
                            warnings.append("At least one of the default PDF sets ("+UL_PDFs_N+") appear as variation as well or listed more than once.")
                        else:
                            warnings.append("pdfsets in runcmsgrid file does not contain one of the recommended sets:"+str(UL_PDFs_N[0])+"("+str(UL_PDFs[0])+")    or "+str(UL_PDFs_N[1])+"("+str(UL_PDFs[1])+")")
                        print("Your runcmsgrid file contains these sets:")
                        print(mg_me_pdf_list)
                    if (mg_me_pdf_list.count(str(UL_PDFs_N[0])) > 0 and mg_me_pdf_list.count(str(UL_PDFs_N[0])+"@0") != 0) or (mg_me_pdf_list.count(str(UL_PDFs_N[1])) > 0 and mg_me_pdf_list.count(str(UL_PDFs_N[1])+"@0") != 0):
                        warnings.append("Main pdf recommended set ("+str(UL_PDFs_N[0])+" or "+str(UL_PDFs_N[1])+") is listed in runcmsgrid file but it is also included as a variation??")
            if alt_ickkw_c > 0:
                if match_eff == 1:
                    warnings.append("Matched sample but matching efficiency is 1!")

            if (pw_gp or mg_gp) and mg_nlo != 1 and mcatnlo_flag != 1:
                MGpatch.append(int(os.popen('more '+my_path+'/'+pi+'/'+'runcmsgrid.sh | grep -c "FORCE IT TO"').read()))
                MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'mgbasedir/Template/LO/SubProcesses/refine.sh').read()))
                MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'process/madevent/SubProcesses/refine.sh').read()))
                if MGpatch[0] == 1 and MGpatch[1] == 1 and MGpatch[2] == 1: print("[OK] MG5_aMC@NLO leading order patches OK in gridpack")
                if MGpatch[0] != 1:
                    errors.append("MG5_aMC@NLO multi-run patch missing in gridpack - please re-create a gridpack using updated genproductions area")
                if MGpatch[1] == 0 or MGpatch[2] == 0:
                    if '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw :
                        errors.append("At least one of the MG5_aMC@NLO tmpdir patches is missing. And the request is using a version "+str(cmssw)+" that does not contain the patch. Please use >= 7_1_32_patch1 or CMSSW_9_3_9_patch1 or 10_2_0_pre2")
                    elif '7_1' in cmssw:
                        test_version = cmssw.split('_')
                        if (len(test_version) == 4 and int(test_version[3]) < 33) or (len(test_version) == 5 and (int(test_version[3]) < 32 or (int(test_version[3]) == 32 and "patch1" not in cmssw))):
                            errors.append("At least one of the MG5_aMC@NLO tmpdir patches is missing. And the request is using a version "+str(cmssw)+" that does not contain the patch. In this release, please at least use CMSSW_7_1_32_patch1")
                    elif '9_3' in cmssw:
                        test_version = cmssw.split('_')
                        if (len(test_version) == 4 and int(test_version[3]) < 10) or (len(test_version) == 5 and (int(test_version[3]) < 9 or (int(test_version[3]) == 9 and "patch1" not in cmssw))):
                            errors.append("At least one of the MG5_aMC@NLO tmpdir patches is missing. And the request is using a version "+str(cmssw)+" that does not contain the patch. In this release, please at least use CMSSW_9_3_9_patch1")
                    elif '10_2' in cmssw:
                        test_version = cmssw.split('_')
                        if len(test_version) == 4 and int(test_version[3]) < 1:
                            errors.append("At least one of the MG5_aMC@NLO tmpdir patches is missing. And the request is using a version "+str(cmssw)+" that does not contain the patch. In this release, please at least use CMSSW_10_2_0_pre2")
                print("-------------------------MG5_aMC LO/MLM Many Threads Patch Check --------------------------------------")
                ppp_ind_range = 0
                if slha_flag == 1:
                    slha_file_list =  os.listdir(slha_all_path)
                    print(slha_file_list)
                    ppp_ind_range = len(slha_file_list)
                if slha_flag == 0:
                    ppp_ind_range = 1
                #slha_flag = 0
                for ppp in range(0,ppp_ind_range):
                    if gp_size == 0: break
                    del MGpatch2[:]
                    if slha_flag == 1:
                        gridpack_cvmfs_path_tmp = slha_all_path+'/'+slha_file_list[ppp]
                        if "runmode0_TEST" in gridpack_cvmfs_path_tmp: continue
                        gridpack_cvmfs_path = gridpack_cvmfs_path_tmp
                        gridpack_eos_path = gridpack_cvmfs_path_tmp.replace("/cvmfs/cms.cern.ch/phys_generator","/eos/cms/store/group/phys_generator/cvmfs")
                    print(gridpack_eos_path)
                    os.system('tar xf '+gridpack_eos_path+' -C '+my_path+'/eos/'+pi)
                    MGpatch2.append(int(os.popen('more '+my_path+'/'+pi+'/'+'runcmsgrid.sh | grep -c "To overcome problem of taking toomanythreads"').read()))
                    MGpatch2.append(int(os.popen('more '+my_path+'/eos/'+pi+'/'+'runcmsgrid.sh | grep -c "To overcome problem of taking toomanythreads"').read()))
                    if MGpatch2[1] == 1: print("[OK] MG5_aMC@NLO LO nthreads patch OK in EOS")
                    if MGpatch2[0] == 1: print("[OK] MG5_aMC@NLO LO nthreads patch OK in CVMFS")
                    if MGpatch2[0] == 0 and MGpatch2[1] == 1: print("[OK] MG5_aMC@NLO LO nthreads patch not made in CVMFS but done in EOS waiting for CVMFS-EOS synch")
                    if MGpatch2[1] == 0:
                        errors.append("MG5_aMC@NLO LO nthreads patch not made in EOS")
                        if args.apply_many_threads_patch:
                            print("Patching for nthreads problem... please be patient.")
                            if slha_flag == 0:
                                os.system('python2 ../../Utilities/scripts/update_gridpacks_mg242_thread.py --prepid '+pi)
                            if slha_flag == 1:
                                os.system('python2 ../../Utilities/scripts/update_gridpacks_mg242_thread.py --gridpack '+gridpack_cvmfs_path)
                    print("-------------------------EOF MG5_aMC LO/MLM Many Threads Patch Check ----------------------------------")
                    print("*")
        if  mg_gp or amcnlo_gp:
            if alt_ickkw_c >= 2 and check[0] == 2 and check[1] == 1 and check[2] == 1 :
                if alt_ickkw_c > 3 and os.path.isfile(file_pwg_check) is False :
                    warnings.append("To check manually - This is a Powheg NLO sample. Please check 'nFinal' is  set correctly as number of final state particles (BEFORE THE DECAYS) in the LHE other than emitted extra parton.")
            if alt_ickkw_c == 1 and check[0] == 0 and check[1] == 0 and check[2] == 0 and mg_lo > 0:
                warnings.append("To check manually - This is a matched MadGraph LO sample. Please check 'JetMatching:nJetMax' ="+str(nJetMax)+" is OK and correctly set as number of partons in born matrix element for highest multiplicity.")
            if alt_ickkw_c == 0 and word == "mcatnlo" and mg_nlo > 0 and check[0] == 2 and check[1] == 1 and check[2] == 1 and loop_flag != 1:
                warnings.append("This a MadGraph NLO sample without matching. Please check 'TimeShower:nPartonsInBorn' is set correctly as number of coloured particles (before resonance decays) in born matrix element.")
            if alt_ickkw_c <= 1 and word == "madgraph" and mg_nlo != 1 and amcnlo_gp is False and (check[0] != 0 or check[1] != 0 or check[2] != 0):
                errors.append("You run MG5_aMC@NLO at LO but you have  Pythia8aMCatNLOSettings_cfi in fragment")

        if mg_gp is True or amcnlo_gp is True:
            powhegcheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
            if powhegcheck[0] > 0 and pw_mg == 0:
                errors.append("Please remove POWHEG settings for MG requests.")
        if pw_mg is False and mg_gp is False and amcnlo_gp is False and jhu_gp is False and sherpa_gp is False:
             purepythiacheck.append(int(os.popen('grep -c -i Pythia8aMCatNLOSettings '+pi).read()))
             purepythiacheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
             if purepythiacheck[0] > 0 or purepythiacheck[1] >0 :
                 warnings.append("Please remove aMCatNLO or POWHEG settings if this is a pure Pythia request. If it's not a pure request, in the future, please include madgraph/powheg or amcatnlo in the name of the dataset")
        if loop_flag == 1:
            if mcatnlo_flag == 1:
                errors.append("You are using a loop induced process, [noborn=QCD]. Please remove all occurances of Pythia8aMCatNLOSettings from the fragment")
            if nPartonsInBorn_flag == 1:
                errors.append("You are using a loop induced process, [noborn=QCD]. Please remove all TimeShower:nPartonsInBorn from the fragment")
        for kk in range (0, 8):
            tunecheck.append(int(os.popen('grep -v "#" '+pi+' | grep -v "annotation" | grep -v "name" | grep -c -i '+tune[kk]).read()))
        if tunecheck[6] == 3 or tunecheck[7] == 3:
            if tunecheck[0] != 3:
                warnings.append("Check if there is some extra tune setting")
        if 'sherpa' in dn.lower():
            warnings.append("No automated check of Sherpa ps/tune parameters yet")
        if 3 not in tunecheck and herwig_flag == 0 and sherpa_flag == 0:
            with open(pi) as f:
                tot = f.read()
                n_ext_par += tot.count('MultipartonInteractions')
                n_ext_par += tot.count('ColourReconnection')
                n_ext_par += tot.count('SpaceShower')
                n_ext_par += tot.count('TimeShower')
                n_ext_par += tot.count('reweightGenEmp')
                warnings.append("Number of extra or replaced tune parameters is at least "+str(n_ext_par)+"Please check tune configuration carefully (e.g. are the non-replaced parameters the ones you want)")
        if 3 not in tunecheck and fsize != 0 and n_ext_par == 0 and herwig_flag == 0 and sherpa_flag == 0:
            if  any(tunecheck[0]<3 and it!=0 for it in tunecheck):
                errors.append("Tune configuration may be wrong in the fragment or pythia8CUEP8M1Settings are overwritten by some other parameters as in CUETP8M2T4. "+str(tunecheck))
            else:
                warnings.append("None standard tune - please check the fragment carefully.")
        if fsize != 0 and herwig_flag == 0 and sherpa_flag == 0:
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) != 1:
                warnings.append("No parton shower weights configuration in the fragment. Since the Fall18 campaign, we recommend to include Parton Shower weights")
            else:
                if (int(str(cmssw_version)[:1]) == 9 and cmssw_version < 93019) or (int(str(cmssw_version)[:1]) > 9 and cmssw_version < 102030) or (int(str(cmssw_version)[:1]) == 7 and cmssw_version < 71047):
                    errors.append("PS weights in config but CMSSW version is < 10_2_3 for CMSSW version >= 10_X_X or is < 9_3_10 for CMSSW version <= 9_X_X - please check!")
                psweightscheck.append(int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettingsBlock," '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettings" '+pi).read()))
                if not (psweightscheck[0] == 1 and psweightscheck[1] == 1 and psweightscheck[2] == 2):
                    errors.append("Parton shower weight configuration not OK in the fragment")
        if evtgen_flag == 1 and fsize != 0:
            w_tmp, err_tmp = evtgen_check(data_f1)
            warnings.extend(w_tmp)
            errors.extend(err_tmp)               
        if evtgen_flag == 0 and fsize != 0 and int(os.popen('grep -c -i EvtGen '+pi).read()) != 0:
            errors.append("EvtGen settings within fragment but no evtgen flag at dataset name")
        if int(os.popen('grep -c -i filter '+pi).read()) > 3 and filter_eff == 1:
            warnings.append("Filters in the fragment but filter efficiency = 1")
        if "Run3" in pi and "PbPb" not in pi and "Run3Summer21" not in pi:
            err_tmp = run3_checks(data_f1,dn,pi)
            errors.extend(err_tmp)
        if args.develop is False:
            os.popen("rm -rf "+my_path+pi).read()
            os.popen("rm -rf "+my_path+'eos/'+pi).read()
        print("***********************************************************************************")
        print("Number of warnings = "+ str(len(warnings)))
        if len(warnings) > 0:
            print("WARNINGS:")
            print("----------")
            indexelem = 0
            for elem in warnings:
                print (indexelem+1, ": ",elem,"\n")
                indexelem += 1
            print("----------")
        
        print("Number of errors = "+ str(len(errors)))
        if len(errors) > 0:
            print("ERRORS:")
            print("----------")
            indexelem = 0
            for elem in errors:
                print (indexelem+1, ": ",elem,"\n")
                indexelem += 1
            print("----------")        
            print("There is at least 1 error. Request won't proceed to VALIDATION")

        error = len(errors)
# Valid range for exit codes is 0-255
        if error > 255 or error < 0:
            error = 255

# Exit with code, 0 - good, not 0 is bad
        if args.ticket is None:
            if args.bypass_validation:
                continue
            else:
                sys.exit(error)
