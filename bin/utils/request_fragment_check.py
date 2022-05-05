#!/bin/env python3
import os
import sys
import re
import argparse
import textwrap
import fnmatch
import os.path
import string
#import json
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
    error_check_replace = 0
    replace_mccont = os.popen('grep "_REPLACE" '+str(runcmsgridfile)).read()
    if len(replace_mccont):
        print("[ERROR] Incomplete gridpack. Replace _REPLACE strings in runcmsgrid.sh:")
        print (replace_mccont)
        error_check_replace += 1
    return error_check_replace 

def concurrency_check(fragment,pi,cmssw_version):
    conc_check = 0
    conc_check_lhe = 0
    error_conc = 0
    fragment = re.sub(r'(?m)^ *#.*\n?', '',fragment) # remove lines starting with #
    fragment = fragment.replace(" ","").replace("\"","'")#
    if cmssw_version >= int('10_60_28'.replace('_','')) and int(str(cmssw_version)[:2]) != 11:
        if "ExternalLHEProducer" in fragment and "generateConcurrently=cms.untracked.bool(True)" in fragment:
            # first check if the code has correctly implemented concurrent features. Mark conc_check_lhe (LHE step) or conc_check (GEN step) as True if features are found
            if "Herwig7GeneratorFilter" not in fragment: 
                conc_check_lhe = 1
            else:
                if "postGenerationCommand=cms.untracked.vstring('mergeLHE.py','-i','thread*/cmsgrid_final.lhe','-o','cmsgrid_final.lhe')" in fragment: 
                    conc_check_lhe = 1# 
        elif "ExternalLHEProducer" not in fragment:#
            conc_check_lhe = 1#
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
                print("[ERROR] Concurrent generation parameters used along with RandomizedParameter scan.")
                error_conc = 1
        else:
            # then if not both the LHE and GEN step turns on concurrent features, we check if for some cases it is ok not to have concurrency
            if "Pythia8HadronizerFilter" in fragment and ("evtgen" in fragment.lower() or "tauola" in fragment.lower() or "photos" in fragment.lower()):
                print("\n Pythia8HadronizerFilter with EvtGen, Tauola, or Photos can not be made concurrently.\n")
            elif "Herwig7GeneratorFilter" in fragment and ("wmlhegen" in pi.lower() or "plhegen" in pi.lower()): 
                print("Herwig7GeneratorFilter in the wmLHEGEN or pLHEGEN campaign cannot run concurrently.")
            elif "Pythia8GeneratorFilter" in fragment and "randomizedparameters" in fragment.lower():
                print("Pythia8GeneratorFilter with RandomizedParameter scan cannot run concurrently")
            # for other cases, it is either concurrent generation parameters are missing or wrong
            else:
                print("[ERROR] Concurrent generation parameters missing or wrong. Please see https://twiki.cern.ch/twiki/bin/view/CMSPublic/WorkBookGenMultithread")
                error_conc = 1
    else:
        if "concurrent" in fragment.lower():
            print("[ERROR] Concurrent generation is not supported for versions < CMSSW_10_6_28 and CMSSW_11_X_X series")
            error_conc = 1
    return conc_check_lhe and conc_check, error_conc
   
def ul_consistency(dn,pi,jhu_gp):
    pi_prime = "NULL"
    prime_tmp = []
    warning_ul = 0
    error_ul = 0
    if "Summer20UL18" in pi or "Summer20UL17" in pi or "Summer20UL16wmLHEGENAPV" in pi or "APV" in pi or "Summer20UL16" in pi and "GEN" in pi:
        prime = get_requests_from_datasetname(dn)
        if len(prime) == 0:
            if "Summer20UL16" not in pi:
                print("[ERROR] No corresponing Summer20UL16 request to compare to for consistency.")
                print("         Please first create the corresponding Summer20UL16 requests.")
                error_ul += 1
            else:
                print("[WARNING] No corresponing Summer19UL16 request to compare to for consistency.")
                print("          Please check CAREFULLY!")
                warning_ul += 1
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
            print("[ERROR] No corresponing Summer20UL16 request to compare to for consistency.")
            print("        Please first create the corresponding Summer20UL16 requests.")
            error_ul += 1
        if "NULL" in pi_prime and "APV" not in pi:
            print("[WARNING] No corresponing Summer19UL17 request to compare to for consistency.")
            print(" LEVEL2 Conveners - please check the request VERY CAREFULLY!")
            warning_ul += 1
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
                    print("[WARNING] Two requests have the same fragment (except may be the gridpack)")
                    warning_ul += 1
                else:
                    print("[ERROR] Two requests don't have the same fragment (note that gridpacks haven't been compared to be able to have WriteFailedEvents = 2  JHUGen.input)")
                    error_ul += 1
            else:
                data_f2_strip = re.sub(r'\s+', ' ', data_f2).strip()
                data_f2_strip = exception_for_ul_check(data_f2_strip,cross_section_fragment)
                data_f2_prime_strip = re.sub(r'\s+', ' ',data_f2_prime).strip()
                data_f2_prime_strip = exception_for_ul_check(data_f2_prime_strip,cross_section_fragment)
                if (data_f2_strip == data_f2_prime_strip) == True:
                    print("[OK] Two requests have the same fragment.")
                else: 
                    if "Summer20UL16" not in pi:
                        print("[ERROR] Fragment of "+pi+" is different than its base UL request: "+pi_prime)
                        print("        Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime)
                        error_ul += 1
                    if "Summer20UL16" in pi and "APV" in pi:
                        print("[ERROR] Fragment of "+pi+" is different than its base Summer20UL16 request: "+pi_prime)
                        print("        Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime)
                        error_ul += 1
                    if "Summer20UL16" in pi and "APV" not in pi:
                        print("[WARNING] Fragment of "+pi+" is different than its base Summer19UL17 request: "+pi_prime)
                        print("        Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime)
                        warning_ul += 1           
            if (cmssw == cmssw_prime) == True:
                print("[OK] Two requests have the same CMSSW version.")
            elif "Summer20UL16wmLHEGENAPV" in pi or "Summer20UL16GENAPV" in pi or "Summer20UL18" in pi or "Summer20UL17" in pi:
                print("[WARNING] CMSSW version of "+pi+" is different than its base UL17 request: "+pi_prime)
                print("        Please make sure that "+pi+" has _exactly_ the same settings as "+pi_prime)
                warning_ul += 1
            f1_prime.close()
            f2_prime.write(data_f2_prime)
            f2_prime.close()
    if not error_ul: print("UL consistency check is OK.")
    return warning_ul,error_ul

def xml_check_and_patch(f,cont,gridpack_eos_path,my_path,pi):
    xml = str(re.findall('xmllint.*',cont))
    cur_dir = os.getcwd()
    warning_xml = 0
    error_xml = 0
    if "stream" not in xml or len(xml) < 3:
        targz_flag = 0
        if "stream" not in xml and len(xml) > 3:
          print("[WARNING] --stream option is missing in XMLLINT, will update runcmsgrid.")
          warning_xml += 1
        if len(xml) < 3:
          print("[WARNING] XMLLINT does not exist in runcmsgrid, will update it.")
          warning_xml += 1
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
            print("[ERROR] backup gridpack has a problem.")
            error_xml += 1
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
          print("[ERROR] there was a problem copying in the updated gridpack to eos.")
          error_xml += 1
        os.chdir(cur_dir)
    return warning_xml,error_xml

def evtgen_check(fragment):
    err = 0
    warn = 0
    fragment = fragment.replace(" ","")
    if "evtgen" not in fragment.lower():
        print("[ERROR] evtgen flag defined in dataset name but not set within the fragment")
        err = 1
    else:
        if "fromGeneratorInterface.EvtGenInterface.EvtGenSetting_cffimport*" not in fragment:
            print("[ERROR] EvtGenInterface not definied within the fragment")
            err = 1
        if "convertPythiaCodes=cms.untracked.bool(False)" not in fragment:
            print("[ERROR] Turn off the PythiaCodes conversion when using EvtGen")
            err = 1
        if "generator.PythiaParameters.processParameters.extend(EvtGenExtraParticles)" not in fragment:
            print("[WARNING] Are you sure EvtGenExtraParticles extension is not needed?")
            warn = 1      
    if "evt_2014.pdl" not in fragment:
        print("[WARNING] Are you sure you do not want the 2014 evt pdl table?")
        warn = 1   
    if "DECAY_2014" not in fragment:
        print("[WARNING] Are you sure you do not want the 2014 decay dec table?")
        warn = 1
    return warn, err

def run3_checks(fragment,dn,pi):
    err = 0
    warn = 0
    fragment = fragment.replace(" ","")
    print("======> Run3 Fragment and dataset name checks:")
    if "comEnergy" in fragment:
        comline = re.findall('comEnergy=\S+',fragment)
        if "run3winter22" in pi.lower() and "13600" not in comline[0]:
            print(comline[0])
            print("[ERROR] The c.o.m. energy is not specified as 13600 GeV in the fragment")
            err += 1
        if "run3winter21" in pi.lower() and "14000" not in comline[0]: 
            print(comline[0])
            print("[ERROR] The c.o.m. energy is not specified as 14000 GeV in the fragment")
            err += 1 
    if "run3winter22" in pi.lower() and ("FlatRandomEGunProducer" not in fragment and "FlatRandomPtGunProducer" not in fragment and "Pythia8EGun" not in fragment and "13p6TeV" not in dn):
        print("[ERROR] The data set name does not contain 13p6TeV for this Run3 request")
        err += 1
    if "run3winter21" in pi.lower() and ("FlatRandomEGunProducer" not in fragment and "FlatRandomPtGunProducer" not in fragment and "Pythia8EGun" not in fragment and "14TeV" not in dn):
        print("[ERROR] The data set name does not contain 14TeV for this Run3 request")
        err += 1 
    return err

def run3_run_card_check(filename_mggpc,pi):
    err = 0
    beamenergy1 = os.popen('grep ebeam1 '+filename_mggpc).read()
    beamenergy2 = os.popen('grep ebeam2 '+filename_mggpc).read()
    print("======> Run3 run_card check for MG5aMC") 
    print(beamenergy1,beamenergy2)
    if "run3winter22" in pi.lower() and ("6800" not in beamenergy1 or "6800" not in beamenergy2):
        print("[ERROR] The beam energy is not specified as 6800 GeV in the run_card")
        err = 1
    if "run3winter21" in pi.lower() and ("7000" not in beamenergy1 or "7000" not in beamenergy2):
        print("[ERROR] The beam energy is not specified as 7000 GeV in the run_card")
        err = 1
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
    warning_dipole = 0
    error_dipole = 0
    if "vbf" not in dn.lower():
        warning_dipole += 1
        print("[WARNING] VBF not in dataset name.")
    if len(dipole_recoil):
        dipole_recoil = dipole_recoil[0].split("=")[1].replace('"', '').replace('\'', '')
        if "on" in dipole_recoil:
            dipole_recoil_flag = 1 
    if pw_gp is False:
        if vbf_lo and dipole_recoil_flag == 0:
            warning_dipole = 1
            print("[WARNING] LO VBF with global recoil --> SpaceShower:dipoleRecoil = 0 SMP/HIG groups are moving to local recoil but currently using global recoil. See https://arxiv.org/pdf/1803.07943.pdf")
        if vbf_lo and dipole_recoil_flag:
            print("[OK] LO VBF with local recoil. --> SpaceShower:dipoleRecoil = 1")
        if vbf_nlo and dipole_recoil_flag == 0:
            print("[OK] NLO VBF with global recoil --> SpaceShower:dipoleRecoil = 0")
        if vbf_nlo and dipole_recoil_flag:
            error_dipole = 1
            print("[ERROR] NLO VBF with local recoil. --> SpaceShower:dipoleRecoil = 1 aMC@NLO should not be used with local recoil. See https://arxiv.org/pdf/1803.07943.pdf")
    else:
        if "vbf" in dn.lower() and dipole_recoil_flag == 0:
            warning_dipole = 1
            print("[WARNING] VBF POWHEG with global recoil --> SpaceShower:dipoleRecoil = 0. See https://arxiv.org/pdf/1803.07943.pdf")
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
#    print "JSON Dump:"
#    print "----------"
#    print(json.dumps(res,indent = 2))
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
        grid_points_flag = 0
        nPartonsInBorn_flag = 0
        filename_mggpc = 'del'
        ickkw = 'del' # ickkw = matching parameter in madgraph
        ickkw_c = 100
        alt_ickkw_c = 100
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
        warning = 0
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
        if "ppd" in pi.lower(): ppd = 1
        req_type = "dummy"
        if "gen" in pi.lower(): req_type = "genonly"
        if "gs" in pi.lower(): req_type = "gs"
        if "plhe" in pi.lower(): req_type = "plhe"
        if "herwig" in dn.lower(): herwig_flag = 1
        if "evtgen" in dn.lower(): evtgen_flag = 1
        if "comphep" in dn.lower() or "calchep" in dn.lower():
            print("[WARNING] comphep or calchep request. Please check manually")
            warning += 1
            continue
        for item in te:
            timeperevent = float(item)
            print("time per event (sec/event) = "+str(timeperevent))
        if timeperevent > 150.0 :
            print("[WARNING] Large time/event (> 150 sec)="+str(timeperevent)+" - please check")
            warning += 1
        version_not_ok = 0
        if '8_0' in cmssw and "Summer16FSPremix" not in pi: version_not_ok = 1
        if '9_4' in cmssw and "Fall17FSPremix" not in pi: version_not_ok = 1
        if '10_6' not in cmssw and '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw and version_not_ok == 1:
            print("[WARNING] Are you sure you want to use "+cmssw+" release which is not standard")
            print("          which may not have all the necessary GEN code.")
            warning += 1
        if totalevents >= 100000000 :
            print("[WARNING] Is "+str(totalevents)+" events what you really wanted - please check!")
            warning += 1
        os.popen('wget -q '+mcm_link+'public/restapi/requests/get_fragment/'+pi+' -O '+pi).read()

        fsize = os.path.getsize(pi)
        f1 = open(pi,"r")
        f2 = open(pi+"_tmp","w")
        data_f1 = f1.read()

        if int(os.popen('grep -c FlatRandomEGunProducer '+pi).read()) == 1 or int(os.popen('grep -c FlatRandomPtGunProducer '+pi).read()) == 1 or int(os.popen('grep -c Pythia8EGun '+pi).read()) == 1: 
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
        if "SnowmassWinter21GEN" not in pi and "SnowmassWinter21wmLHEGEN" not in pi and particle_gun == 0 and pi not in concurrency_check_exception_list:
            conc_check_result, tmp_err = concurrency_check(data_f1,pi,cmssw_version)
            error += tmp_err
        else:
            print("[WARNING] Skipping the concurrency check since these are (wmLHE)GEN-only campaigns or a particle gun or a Sherpa Diphoton sample.")
            warning += 1
#        data_f2 = re.sub(r'(?m)^ *#.*\n?', '',data_f1)

        cross_section_fragment = re.findall('crossSection.*?\S+\S+',data_f2)
        if (cross_section_fragment):
            cross_section_fragment=cross_section_fragment[0]
            cross_section_fragment = re.findall('\((.*?)\)',cross_section_fragment)[0]
        filter_eff_fragment = re.findall('filterEfficiency.*?\S+\S+',data_f2)    
        if (filter_eff_fragment):    
            filter_eff_fragment=filter_eff_fragment[0]
            filter_eff_fragment = re.findall('\((.*?)\)',filter_eff_fragment)[0]
        print("Cross section in the fragment =" + str(cross_section_fragment) +" pb")
        print("Cross section from generator parameters field = "+str(cross_section)+" pb")
        if str(cross_section_fragment).isdigit() is False:
            print("[WARNING] Skipping the cross section consistency check in generator parameters field and the fragment")
            print("          This is most probably because the cross section is defined through a variable") 
        if str(cross_section_fragment).isdigit() is True and cross_section_fragment and cross_section and int(ext) == 0 and float(cross_section_fragment) != float(cross_section):
            print("[ERROR] Cross section in the generator parameters field and the one in the fragment do not match!")
            error += 1
        print("")
        print("Filter efficiency in fragment =" + str(filter_eff_fragment))
        print("Filter efficiency from generator parameters field = "+str(filter_eff))
        if filter_eff_fragment and filter_eff and int(ext) == 0 and float(filter_eff_fragment) != float(filter_eff):
            print("[ERROR] Filter efficiency in the generator parameters field and the one in the fragment do not match!")
            error += 1    
	
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
                   print("[ERROR] The base request "+pi+" and the cloned request "+pi_clone_entries+" used for the extension don't have the same fragment!")
                   print("Below is the diff of the base and and the cloned request:")
                   print("---------------------------------------------------------------------------------")
                   print((os.popen('diff '+pi+' '+pi_clone_entries).read()))
                   print("---------------------------------------------------------------------------------") 
                   error += 1		
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
        amcnlo_gp = False
        mg_gp = False
        jhu_gp = False
        sherpa_gp = False
        sherpa_flag = False
        openloops_flag = False

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
                    print ("[ERROR] scram_arch for Sherpa and CMSSW are NOT the same")
                    error += 1
                print("      but note that this check is done based on folder names except the one for CMSSW")  
            else:
                print ("[WARNING] scram_arch for Sherpa unidentifiable")
                warning += 1
            sv_tmp = re.findall("sherpa/.*/",gridpack_cvmfs_path_tmp)[0].split("/")[1].split(".")
            if "v" in sv_tmp[0].lower(): sv_tmp[0] = sv_tmp[0].replace("v","").replace("V","")
            sherpa_version = int(sv_tmp[0])*1000 + int(sv_tmp[1])*100 + int(sv_tmp[2])
            print ("Sherpa Version = ", sherpa_version)
            if sherpa_version < 2211:
                print("[WARNING] Sherpa older than version 2.2.11")
                warning += 1
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
                    print ("[ERROR] scram_arch for Sherpa and OpenLoops are NOT the same")
                    error += 1
                print("      but note that this check is done based on folder names except the one for CMSSW")

        if gp_size and sherpa_flag == 0:
            gridpack_cvmfs_path_tmp = re.findall("/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.xz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tgz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.gz",gridpack_cvmfs_path_tmp)
            if not gridpack_cvmfs_path_tmp:
                print("[ERROR] Gridpack should be in cvmfs in the dedicated folder location with the full path to the file given. ")
                error += 1
                gp_full_path = False
        if gp_size and gp_full_path and sherpa_flag == 0:
            gridpack_cvmfs_path = gridpack_cvmfs_path_tmp[0]
            gridpack_eos_path = gridpack_cvmfs_path.replace("/cvmfs/cms.cern.ch/phys_generator","/eos/cms/store/group/phys_generator/cvmfs")
            print("Gridpack location in cvmfs and eos:")
            print(gridpack_cvmfs_path)
            print(gridpack_eos_path)
            if int(os.popen('grep -c slha '+pi).read()) != 0 or int(os.popen('grep -c \%i '+pi).read()) != 0 or int(os.popen('grep -c \%s '+pi).read()) != 0: slha_flag = 1
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
            if os.path.isfile(gridpack_cvmfs_path) is True:
                os.system('tar xf '+gridpack_cvmfs_path+' -C '+my_path+'/'+pi)
            else:
                error += 1
                print ("[ERROR] Gridpack ",gridpack_cvmfs_path," does not exist!") 
                print ("    ..... exiting ....")
                sys.exit()
            jhu_gp = os.path.isfile(my_path+'/'+pi+'/'+'JHUGen.input')
            pw_gp = os.path.isfile(my_path+'/'+pi+'/'+'powheg.input')
            mg_f1 = my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'
            mg_f2 = my_path+'/'+pi+'/'+'process/Cards/run_card.dat'
            amcnlo_gp = os.path.isfile(my_path+'/'+pi+'/'+'process/Cards/run_card.dat')
            mg_gp = os.path.isfile(mg_f1) or os.path.isfile(mg_f2)
            print("path powheg "+str(pw_gp))
            print("path mg "+str(mg_gp))
            print("path amcnlo "+str(amcnlo_gp))
            print("path jhugen "+str(jhu_gp))
            if mg_gp is False and "madgraph" in dn.lower():
                print("[ERROR] Although the name of the dataset has ~Madgraph, the gridpack doesn't seem to be a MG5_aMC one.")
                error += 1
            if mg_gp is True:
                filename_mggpc = my_path+'/'+pi+'/'+'process/madevent/Cards/run_card.dat'
                fname_p2 = my_path+'/'+pi+'/'+'process/Cards/run_card.dat'
                if os.path.isfile(fname_p2) is True :
                    filename_mggpc = fname_p2
                #file_run_card = open(filename_mggpc,"r")
                if "Run3" in pi and "PbPb" not in pi:
                    err_tmp = run3_run_card_check(filename_mggpc,pi)
                    error += err_tmp
                alt_ickkw_c = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "= ickkw"').read()
                alt_ickkw_c = int(re.search(r'\d+',alt_ickkw_c).group())
                print("MG5 matching/merging: "+str(alt_ickkw_c))
                maxjetflavor = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "= maxjetflavor"').read()
                if len(maxjetflavor) != 0:
                        maxjetflavor = int(re.search(r'\d+',maxjetflavor).group())
                else:
                    print("[WARNING] maxjetflavor not defined in run_card.dat")
                    warning += 1
                print("maxjetflavor = "+str(maxjetflavor))
                if alt_ickkw_c == 3:
                    qCutME = os.popen('grep "qCutME" '+pi).read()
                    qCutME = qCutME.replace(" ","")
                    qCutME = re.findall('qCutME=\d+',qCutME)[0].split("=")[1]
                    print("qCutME = ",qCutME)
                    ptj_runcard = os.popen('grep "ptj" '+filename_mggpc).read()
                    ptj_runcard = ptj_runcard.replace(" ","")
                    ptj_runcard = re.findall('\d*\.?\d+',ptj_runcard)[0].split("=")[0]
                    print("ptj_runcard =", ptj_runcard)
                    if float(qCutME) != float(ptj_runcard):
                        error += 1
                        print("[ERROR] qCutME in PS settings and ptj in run_card in gridpack do not match.")
                    nQmatch = os.popen('grep "nQmatch" '+pi).read()
                    nQmatch = nQmatch.replace(" ","")
                    nQmatch = re.findall('nQmatch=\d+',nQmatch)[0].split("=")[1]
                    print("nQmatch = ",nQmatch)
                    if int(nQmatch) != int(maxjetflavor):
                        error += 1
                        print("[ERROR] nQmatch in PS settings and maxjetflavor in run_card in gridpack do not match.")
        if herwig_flag == 0 and pw_gp is True:
            warn_tmp , err_tmp = vbf_dipole_recoil_check(vbf_lo,vbf_nlo,data_f2,pw_gp,dn)
            warning += warn_tmp
            error += err_tmp
        if herwig_flag != 0:
            os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_common.txt -O herwig_common.txt') 
            file2 = set(line.strip().replace(",","") for line in open(pi))
            file1 = set(line.strip().replace(",","") for line in open('herwig_common.txt'))
            for line in file1:                
                if line not in file2:
                    print("[ERROR] Missing herwig setting in fragment: "+line)
                    error += 1
            if pw_gp is True:
               os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_powheg.txt -O herwig_powheg.txt')	
               file_me = set(line.strip().replace(",","") for line in open('herwig_powheg.txt'))
               for line in file_me:
                   if line not in file2:
                       print("[ERROR] Missing herwig powheg specific setting in fragment: "+line)
                       error += 1
            if mg_gp is True:
               os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_mg.txt -O herwig_mg.txt') 
               file_me = set(line.strip().replace(",","") for line in open('herwig_mg.txt'))
               for line in file_me:
                   if line not in file2:
                       print("[ERROR] Missing herwig mg5_amc specific setting in fragment: "+line)
                       error += 1 
               if alt_ickkw_c == 3:#fxfx
                   if "'set FxFxHandler:MergeMode FxFx'" not in file2:
                       print("[ERROR] Missing set FxFxHandler:MergeMode FxFx in the user settings block")
                       error += 1
                   if "'set FxFxHandler:njetsmax'" not in file2:
                       print("[ERROR] Missing set FxFxHandler:njetsmax MAX_N_ADDITIONAL_JETS in the user settings block")
                       error += 1
               if alt_ickkw_c == 1:#mlm
                   if "'set FxFxHandler:MergeMode TreeMG5'" not in file2:
                       print("[ERROR] Missing set FxFxHandler:MergeMode TreeMG5 in the user settings block")
                       error += 1 
            if amcnlo_gp is True or alt_ickkw_c == 0:
               os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_mcnlo.txt -O herwig_mcnlo.txt')
               file_me = set(line.strip().replace(",","") for line in open('herwig_mcnlo.txt'))
               for line in file_me:
                   if line not in file2:
                       print("[ERROR] Missing herwig MG with 0 jets or mc@nlo specific setting in fragment: "+line)
                       error += 1 
            if "9_3" not in str(cmssw) and "7_1" not in str(cmssw) and pw_gp != 0 and mg_gp !=0 and amcnlo_qg !=0:
                os.system('wget -q https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/utils/herwig_frag_lines.txt -O herwig_frag_lines.txt')
                file1 = set(line.strip().replace(",","")  for line in open('herwig_frag_lines.txt'))
                herwig_check = []
                herwig_psweight_tag = 0
                for line in file2: print(line)
                print("-----")	 
                for line in file1:
                    print(line)
                    if line not in file2:
                        herwig_check.append(line)	
                if len(herwig_check) != 0 and "eec5" not in dn.lower() and "ee5c" not in dn.lower():
                    herwig_count.append(herwig_check[0].count('hw_lhe_common_settings'))
                    herwig_count.append(herwig_check[1].count('herwig7LHECommonSettingsBlock'))
                    herwig_count.append(herwig_check[2].count('from Configuration.Generator.Herwig7Settings.Herwig7LHECommonSettings_cfi import *'))
                    if all(x == 1 for x in herwig_count) and any("insert SubProcess:MatrixElements" in x for x in list(file2)):
                        herwig7_bypass_error = 1
                    if "PSWeights" not in herwig_check:
                        herwig_psweight_tag = 1
                        print("[WARNING] Parton shower weights are missing in the Herwig7 fragment.")
                        warning = warning + 1
                    if herwig7_bypass_error == 0 and herwig_psweight_tag == 0:
                        print("[ERROR] "+ str(len(herwig_check)) + " missing fragment line(s) for herwig:")
                        print("         lines for internal matrix element are missing in the fragment.")
                        print(herwig_check)
                        error = error + len(herwig_check)
        if fsize == 0:
            print("[WARNING] No fragment associated to this request")
            print("          is this the hadronizer you intended to use?: "+gettest)
            warning += 1
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

        nevts = 100.
        if timeperevent > 0:   
            nevts = (8*3600/timeperevent)*total_eff
            print("Expected number of events = "+str(nevts))
#        if  nevts < 50. and ppd == 0:
#            print("[ERROR] The expected number of events is too small (<50): "+str(nevts))
#            print("        Either the timeperevent value is too large or the filter or matching efficiency is too small. ")
#            print("        Note that total_efficiency = filter_efficiency x matching_efficiency.") 
#            print("        Please check or improve:") 
#            print("            time per event = "+str(timeperevent))
#            print("            filter efficiency = "+str(filter_eff))
#            print("            matching efficiency = "+str(match_eff))
#            error += 1

        if any(word in dn for word in MEname) and gp_size == 0 and "plhe" not in pi.lower():
            print("[ERROR] gridpack path is not properly specified - most probable reason is that it is not a cvmfs path.")
            error += 1
        if "sherpa" in dn.lower():
            print("[WARNING] Not checking sherpacks in too much detail for now. Please do independent tests.")
            warning += 1
#            gp_size = 0
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
            if int(os.popen('grep -c nFinal '+pi).read()) == 1:
                nFinal = os.popen('grep nFinal '+pi).read()
                if grid_points_flag == 1:
                    nFinal = re.findall('nFinal = \d+',nFinal)
                    nFinal = nFinal[0]
                nFinal =  re.findall('\d+',nFinal)
                nFinal = int(nFinal[0])
                print("nFinal="+str(nFinal))
            if int(test_cs_version[2]) == 6 and ('CMSSW_10_6_0' not in cmssw or 'CMSSW_10_6_0_patch1' not in cmssw): tunparmark = 1
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
                    print("[ERROR] SigmaTotal:mode is missing")
                    print("        For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1")
                    print("        SigmaTotal:mode shoud be added by hand and set to 0")
                    error += 1
                    tmp_flag = 1
                if len(mb_SigmaEl) == 0:
                    print("[ERROR] SigmaTotal:sigmaEl is missing")
                    print("        For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1")
                    print("        SigmaTotal:sigmaEl should be added by hand and set to 21.89")
                    error += 1
                    tmp_flag = 1
                if len(mb_SigmaTot) == 0:
                    print("[ERROR] SigmaTotal:sigmaTot is missing")
                    print("        For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1")
                    print("        SigmaTotal:sigmaTot should be added by hand and set to 100.309")
                    error += 1
                    tmp_flag = 1
                if len(PDF_pSet_test) == 0:
                    print("[WARNING] PDF:pSet is missing (if you want to use NNPDF3.1)")
                    print("        For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1")
                    print("        PDF access method should be like")
                    print("        e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'")
                    warning += 1
                    tmp_flag = 1
                if tmp_flag == 0:
                    if int(mb_mode[0]) != 0:
                        print("[ERROR] SigmaTotal:mode should have been set to 0")
                        error = error+1
                    if abs(float(mb_SigmaEl[0])-21.88) > 0.1:
                        print("[ERROR] SigmaTotal:sigmaEl should have been set to 21.89")
                        error = error+1
                    if abs(float(mb_SigmaTot[0])-100.308) > 0.01:
                        print("[ERROR] SigmaTotal:sigmaTot should have been set to 100.309")
                        error = error+1
                    if int(PDF_pSet[0]) != 1:
                        print("[WARNING] PDF access method is wrong (if you want to use NNPDF3.1). Please correct:")
                        print("        e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'")
                        warning += 1 

            if gp_size != 0:
                if "ppd" not in pi.lower() and "Summer20UL17pp5TeV" not in pi:
                    w_temp, e_temp = ul_consistency(dn,pi,jhu_gp)
                    warning += w_temp
                    error += e_temp
                if "fall18" not in pi.lower() and not (any(word in dn for word in tunename) or "sherpa" in dn.lower() or ("herwigpp" in dn.lower() and ("eec5" in dn.lower() or "ee5c" in dn.lower()))):
                    print("[ERROR] Dataset name does not have the tune name: "+dn)
                    error += 1
                if "fall18" not in pi.lower() and not any(word in dn.lower() for word in psname):
                    print("[ERROR] Dataset name does not contain a parton shower code name: "+dn)
                    error += 1
                if not any(word in dn.lower() for word in MEname):
                    print("[WARNING] Dataset name is not regular:"+dn)
                    print("          Please add the Generator name to the dataset.")
                    warning += 1
                    if pw_gp is True: dn = dn + "-powheg"
                    if mg_gp is True: dn = dn + "-madgraph"
                    if jhu_gp is True: dn = dn + "-jhugen"
                    if amcnlo_gp is True:
                        if alt_ickkw_c == 0: dn = dn + "-amcatnlo"
                        if alt_ickkw_c == 3: dn = dn + "-amcatnloFXFX"
                gp_log_loc = my_path+'/'+pi+'/gridpack_generation.log'
                if os.path.isfile(gp_log_loc) is False and jhu_gp is False and sherpa_flag is False:
                    print("[WARNING] No gridpack generation.log")
                    warning += 1			 
                elif (mg_gp is True or amcnlo_gp is True) and os.path.isfile(gp_log_loc) is True:
                    pf.append(os.popen('grep \"saving rejects to\" '+gp_log_loc).read())
                    pf.append(os.popen('grep \"INFO: fail to reach target\" '+gp_log_loc).read())
                    pf.append(os.popen('grep \"INFO: Not enough events for at least one production mode\" '+gp_log_loc).read())
                    if len(pf[0]) != 0:
                        print("#################################################################")
                        print("[WARNING] "+pf[0])
                        print("            Gridpack PATCH problem!")
                        print("            Please use the master branch of genproductions!")
                        print("#################################################################")
                        warning += 1
                    if len(pf[1]) !=0 or len(pf[2]) != 0:
                        print("[WARNING] "+pf[1])
                        print("          "+pf[2])
                        print("          You may try to request more events per phase-space region in the gridpack.")
                        warning += 1
                if mg_gp is True:
                    dir_path = os.path.join(my_path,pi,"InputCards")
                    if os.path.isdir(dir_path):
                        input_cards_customize_card = find_file(dir_path,"customizecards.dat")
                        if input_cards_customize_card:
                            c_w_line = []
                            s_line = []
                            with open(input_cards_customize_card, 'r+') as f_cust:
                                for num, lc in enumerate(f_cust, 0):
                                    if "compute_widths " in lc.lower(): c_w_line.append(num)
                                    if "set " in lc.lower(): s_line.append(num)
                            customize_widths_flag = 0
                            if len(c_w_line) > 0 and len(s_line) > 0:
                                for x in c_w_line:
                                    for y in s_line:
                                        if int(x) < int(y): customize_widths_flag = 1
                            if customize_widths_flag > 0:
                                print("[ERROR] COMPUTE_WIDTHS followed by SET command(s) should not be used in customizecards.")
                                print("        Instead use \"set width X auto\" to compute the widths for X and change the parameter card settings.")  
                                error += 1                 
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
                                print("[ERROR] Please remove problematic characters (at least one of @#$%^&*()+-[]{}.\) from rwgt_names.")
                                print("        See https://github.com/cms-sw/genproductions/blob/master/bin/MadGraph5_aMCatNLO/gridpack_generation.sh#L102")
                                print("        This causes the header in mg5 to be corrupted and nano-aod will not work.") 
                                error += 1
                            if any((chars in chars_to_check_warning) for chars in reweights):
                                print('[WARNING] The existence of a "." in reweight_card will result in the name of the weight not to be shown in the header.')
                                print("          Please make sure if this is a problem for your analysis, if not, please remove the dot.")
                                warning += 1
                if mg_gp is True:
                    if alt_ickkw_c == 3 and pythia8_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+filename_mggpc).read()
                        if "PYTHIA8" not in ps_hw.upper():
                            print("[ERROR] PYTHIA8 = parton_shower not in run_card.dat")
                            error += 1
                    if alt_ickkw_c == 3 and herwig_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+filename_mggpc).read()
                        if ("HERWIGPP" not in ps_hw.upper()) or ("HERWIG7" not in ps_hw.upper() and herwig7_bypass_error == 1):
                            print("[ERROR] HERWIGPP or HERWIG7 = parton_shower not in run_card.dat")
                            error += 1
                        if int(os.popen('grep -c "set FxFxHandler:MergeMode FxFx" '+pi).read()) == 0:
                            print("[ERROR] Please add \'set FxFxHandler:MergeMode FxFx\'")
                            print("        and set FxFxHandler:njetsmax to the number of additional partons in the proc_card")
                            error += 1
                    if alt_ickkw_c == 2 and herwig_flag != 0:
                        if int(os.popen('grep -c herwig7CommonMergingSettingsBlock').read()) == 0:
                            print("[ERROR] Please load herwig7CommonMergingSettingsBlock")
                            error += 1
                if amcnlo_gp is True:
                    if pythia8_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+my_path+'/'+pi+'/'+'process/Cards/run_card.dat').read()
                        if "PYTHIA8" not in ps_hw.upper():
                            print("[ERROR] PYTHIA8 = parton_shower not in run_card.dat")
                            error += 1
                    if herwig_flag != 0:
                        ps_hw = os.popen('grep parton_shower '+my_path+'/'+pi+'/'+'process/Cards/run_card.dat').read()
                        if ("HERWIGPP" not in ps_hw.upper()) or ("HERWIG7" not in ps_hw.upper() and herwig7_bypass_error == 1):
                            print("[ERROR] HERWIGPP or HERWIG7 = parton_shower not in run_card.dat")
                            error += 1

        if "jhugen" in dn.lower():
            if gp_size == 0: break
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
                        print("[WARNING] The gridpack uses PDF = "+str(jhu_pdf)+" but not the recommended sets for UL requests:")
                        print("                                  "+str(UL_PDFs_N[0])+" "+str(UL_PDFs[0]))
                        print("                               or "+str(UL_PDFs_N[1])+" "+str(UL_PDFs[1]))
                        warning += 1
            if os.path.isfile(jhufilename) is True and pw_gp is True:
                with open(jhufilename) as f:
                    jhu_in = f.read()
                    jhu_in = re.sub(r'(?m)^ *#.*\n?', '',jhu_in)
                    jhu_wfe = str(re.findall(r'(WriteFailedEvents.*(?=\s))',jhu_in))
                    if (not jhu_wfe or jhu_wfe.isspace()) or (jhu_wfe and not jhu_wfe.isspace() and "2" not in jhu_wfe): 
                        print("[ERROR] WriteFailedEvents should be set to 2 in JHUGen.input in jhugen+powheg samples.")
                        error += 1
                    else:
                        WriteFailedEvents_flag = 1
                        print("[OK] "+str(jhu_wfe)+" for this jhugen+powheg sample.")
        for ind, word in enumerate(MEname):
            if fsize == 0: break
            if ind == 3: break
            if word in dn.lower():
                if ind == 2 :
                    knd = 1
                else :
                    knd = ind
                check.append(int(os.popen('grep -c pythia8'+ME[knd]+'Settings '+pi).read()))
                check.append(int(os.popen('grep -c "from Configuration.Generator.Pythia8'+ME[knd]+'Settings_cfi import *" '+pi).read()))
                check.append(int(os.popen('grep -c "pythia8'+ME[knd]+'SettingsBlock," '+pi).read()))
                if check[2] == 1: mcatnlo_flag = 1
                if ind == 0:
                    if gp_size == 0:
                        break
                    file_pwg_check =  my_path+'/'+pi+'/'+'pwhg_checklimits'
                    print(file_pwg_check)
                    if os.path.isfile(file_pwg_check) is True :
                        print("grep from powheg pwhg_checklimits files")
                        nemit = os.popen('grep emitter '+file_pwg_check+' | grep process | head -n 1').read().replace('process','').replace('\n','').split(',')
                        nemitsplit = nemit[1].split()
                        nemitsplit_pr = nemitsplit[2:]
                        nemitsplit = [x for x in nemitsplit_pr if x!=nemitsplit[0] and x!=nemitsplit[1]]
                        nemitsplit = [100 if x == "***" else x for x in nemitsplit]
                        nemitsplit_wo_leptons = [int(x) for x in nemitsplit]
                        nemitsplit_wo_leptons = [abs(x) for x in nemitsplit_wo_leptons]
                        nemitsplit_wo_leptons = [x for x in nemitsplit_wo_leptons if x < 11 or x > 18]
                        nfinstatpar = len(nemitsplit_wo_leptons)-nemitsplit_wo_leptons.count(0)
                        if nfinstatpar == nFinal : print("[OK] nFinal(="+str(nFinal) + ") is equal to the number of final state particles before decays (="+str(nfinstatpar)+")")
                        if nfinstatpar != nFinal :
                            print("[WARNING] nFinal(="+str(nFinal) + ") may not be equal to the number of final state particles before decays (="+str(nfinstatpar)+")")
                            warning += 1
                    if os.path.isfile(my_path+'/'+pi+'/'+'runcmsgrid.sh') is True: 
                        runcmsgrid_file = my_path+'/'+pi+'/'+'runcmsgrid.sh'
                        with open(runcmsgrid_file,'r+') as f:
                            content = f.read()
                            error += check_replace(runcmsgrid_file)
                            match = re.search(r"""process=(["']?)([^"']*)\1""", content)
                            warning1,error1 = xml_check_and_patch(f,content,gridpack_eos_path,my_path,pi)
                            warning += warning1
                            error += error1
                            f.close()
                    else:
                        print(("[ERROR] ", my_path+'/'+pi+'/'+'runcmsgrid.sh', "does not exists"))
                        error += 1
                    if os.path.isfile(my_path+'/'+pi+'/'+'external_tarball/runcmsgrid.sh') is True:
                        runcmsgrid_file = my_path+'/'+pi+'/'+'external_tarball/runcmsgrid.sh'
                        with open(runcmsgrid_file,'r+') as f2:
                            content2 = f2.read()
                            error += check_replace(runcmsgrid_file)
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
                    if et_flag == 0 and et_flag_external == 0: powheg_input = os.path.join(my_path, pi, "powheg.input")
                    if et_flag == 1 and et_flag_external == 0: powheg_input = os.path.join(my_path, pi, "external_tarball/powheg.input")
                    if os.path.isfile(powheg_input) is True:
                        with open(powheg_input) as f:
                            for line in f:
                                if line.startswith("!") == False and line.startswith("#") == False:
                                    if "bornonly" in line: bornonly = int(re.split(r'\s+',line)[1])
                                    if "lhans1" in line:
                                        pw_pdf = int(re.split(r'\s+', line)[1])
                                        print("Powheg PDF used is: "+str(pw_pdf))
                                        if "UL" in pi and pw_pdf not in UL_PDFs_N:
                                            print("[WARNING] The gridpack uses PDF="+str(pw_pdf)+" but not the recommended sets for UL requests:")
                                            print("                                            "+str(UL_PDFs_N[0])+" "+str(UL_PDFs[0]))
                                            print("                                            or "+str(UL_PDFs_N[1])+" "+str(UL_PDFs[1]))
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
                                if "scale_variation" in line: scale_var_check0 += 1
                                if "renscfact" in line and "facscfact" in line: scale_var_check1 += 1
                                if "PDF_variation" in line: pdf_var_check0 += 1
                                if str(pw_pdf+1) in line: pdf_var_check1 += 1
                            if not (scale_var_check0 == 1 and scale_var_check1 == 9):
                                print("[WARNING] There may be a problem with scale variations. Please check pwg-rwl.dat")
                                warning += 1
                            if not (pdf_var_check0 > 0 and pdf_var_check1 >= 1):
                                print("[ERROR] There may be a problem with PDF variations. Please check pwg-rwl.dat")
                                error += 1
                    if bornonly == 1:
                        bornonly_frag_check = 0
                        if int(os.popen('grep -c "Pythia8PowhegEmissionVetoSettings" '+pi).read()) == 1: bornonly_frag_check = 1
                        if int(os.popen('grep -c "SpaceShower:pTmaxMatch" '+pi).read()) == 1: bornonly_frag_check = 1
                        if int(os.popen('grep -c "TimeShower:pTmaxMatch" '+pi).read()) == 1: bornonly_frag_check = 1
                        if bornonly_frag_check != 0:
                            print("[ERROR] bornonly = 1 and (Pythia8PowhegEmissionVetoSettings or SpaceShower:pTmaxMatch or  TimeShower:pTmaxMatch)")
                            error += 1
                        else:
                            print("[WARNING] bornonly = ",bornonly)
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
                            for name, match in matches.items():
                                if match:
                                    actualvalue = int(match.group(1))
                                    if actualvalue < desiredvalues[name]:
                                        bad = True
                                        print("[WARNING] {0} = {1}, should be at least {2} (may be ok if hmass < 150 GeV, please check!)".format(name, actualvalue, desiredvalues[name]))
                                        warning += 1
                                else:
                                    bad = True
                                    print("[ERROR] didn't find "+name+" in powheg.input")
                                    error += 1
                            if not bad: print("[OK] integration grid setup looks ok for gg_H_quark-mass-effects")
                    else:
                        print("[WARNING] Didn't find powheg process in runcmsgrid.sh")
                        warning += 1

                if ind > 0 and ind < 3:
                    if gp_size == 0: break
                    filename_pc = my_path+'/'+pi+'/'+'process/madevent/Cards/proc_card_mg5.dat'
                    fname_p2 = my_path+'/'+pi+'/'+'process/Cards/proc_card.dat'
                    fname_p3 = my_path+'/'+pi+'/'+'process/Cards/proc_card_mg5.dat'
                    if os.path.isfile(fname_p2) is True : filename_pc = fname_p2
                    if os.path.isfile(fname_p3) is True : filename_pc = fname_p3
                    if os.path.isfile(filename_pc) is True :
                        mg_nlo = int(os.popen('grep -c "\[QCD\]" '+filename_pc).read())
                        loop_flag = int(os.popen('more '+filename_pc+' | grep -c "noborn=QCD"').read())
                        gen_line = os.popen('grep generate '+filename_pc).read()
                        print(gen_line)
                        proc_line = os.popen('grep process '+filename_pc+' | grep -v set').read()
                        print(proc_line)
                        proc_line = gen_line.replace('generate','') + "\n" + proc_line 
                        print("Simplified process lines:")
                        if (gen_line.count('@') > 0 and gen_line.count('@') <= proc_line.count('@')) or (proc_line.count('add') > 0):
                            proc_line = proc_line.split('add process')
                            print(proc_line)
                            for y in range(0,len(proc_line)):
                                if proc_line[y].startswith("set"): continue
                                zz = proc_line[y] 
                                if "," in proc_line[y]: zz = proc_line[y].split(',')[0]
                                zz = zz.translate(str.maketrans('','',string.punctuation))
                                nbtomatch = zz.count('b') if maxjetflavor > 4 else 0
                                print(zz.count('c'))
                                nc = zz.count('c') if "chi" not in zz else 0
                                if "excl" in zz and nc != 0: nc = nc -1
                                jet_count_tmp.append(zz.count('j') + nbtomatch + nc)
                            jet_count = max(jet_count_tmp)
                        else :
                            jet_line = gen_line.replace('generate','')
                            jet_count = jet_line.count('j') + jet_line.count('b') + jet_line.count('c')
                        if nJetMax == jet_count: print("[OK] nJetMax(="+str(nJetMax) + ") is equal to the number of jets in the process(="+str(jet_count)+")")
                        if nJetMax != jet_count and gen_line.count('@') != 0 and alt_ickkw_c !=0:
                            print("[WARNING] nJetMax(="+str(nJetMax)+") is NOT equal to the number of jets specified in the proc card(="+str(jet_count)+")")
                            warning += 1
                        if nJetMax != jet_count and jet_count > 0 and alt_ickkw_c !=0:
                            print("[WARNING] nJetMax(="+str(nJetMax)+") is NOT equal to the number of jets specified in the proc card(="+str(jet_count)+")")
                            warning += 1
                        if nJetMax != jet_count and str(jet_count)+"jet" in dn.lower() and alt_ickkw_c !=0:
                            print("[WARNING] nJetMax(="+str(nJetMax)+") is not equal to the number of jets specified in the proc card(="+str(jet_count)+").")
                            print("          Is it because this is an exclusive production with additional samples with higher multiplicity generated separately?")
                            warning += 1
                        print("Jet Count = "+str(jet_count))
                        if jet_count >= 2 and alt_ickkw_c == 0:
                            if mg_nlo:
                                vbf_nlo = 1
                                print("VBF process at NLO")
                            else:
                                vbf_lo = 1   
                                print("VBF process at LO")
                        warn_tmp , err_tmp = vbf_dipole_recoil_check(vbf_lo,vbf_nlo,data_f2,pw_gp,dn)
                        warning += warn_tmp
                        error += err_tmp
                    if os.path.isfile(filename_mggpc) is True :
                        ickkw = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "= ickkw"').read()
                        bw = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "= bwcutoff"').read()
                        mg_pdf = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "lhaid"').read()
                        mg_pdf = mg_pdf.split("=")[0].split()[0]
                    version_file = my_path+'/'+pi+'/'+'mgbasedir/VERSION'
                    if os.path.isfile(version_file) is True:
                        mgversion_tmp = os.popen('grep version '+version_file).read()
                        mgversion = mgversion_tmp.split()
                        mgversion = mgversion[2].split(".")
                        mgversion_tmp = mgversion_tmp.split("\n")
                        mg5_aMC_version = int(mgversion[0])*100 + int(mgversion[1])*10 + int(mgversion[2])
                        print("The gridpack is made with mg5_aMC version:"+str(mg5_aMC_version))
                        if "UL" in pi and mg5_aMC_version < 261:
                            if "PPD" in pi:
                                print("[WARNING] You're using MG5_aMC "+str(mg5_aMC_version)+" in an Ultra Legacy Campaign. You should use MG5_aMCv2.6.1+")
                                warning += 1
                            else:
                                print("[ERROR] You're using MG5_aMC "+str(mg5_aMC_version)+" in an Ultra Legacy Campaign. You should use MG5_aMCv2.6.1+")
                                error += 1
                    test_bw = bw.split()
                    if float(test_bw[0]) > 15.:
                        print("[WARNING] bwcutoff set to "+str(test_bw[0])+". Note that large bwcutoff values can cause problems in production.")
                        warning += 1
                    print("The MG5_aMC PDF set is:"+str(mg_pdf))
                    if "UL" in pi and int(mg_pdf) != UL_PDFs_N[0] and int(mg_pdf) != UL_PDFs_N[1]:
                        print("[WARNING] The gridpack uses PDF="+str(mg_pdf)+" but not the recommended sets for UL requests:")
                        print("                                            "+str(UL_PDFs_N[0])+" "+str(UL_PDFs[0]))
                        print("                                            or "+str(UL_PDFs_N[1])+" "+str(UL_PDFs[1]))
                        warning += 1
                    if mg_gp is True:
                        runcmsgrid_file = os.path.join(my_path, pi, "runcmsgrid.sh")
                        with open(runcmsgrid_file) as fmg:
                            fmg_f = fmg.read()
                            error += check_replace(runcmsgrid_file)
                            fmg_f = re.sub(r'(?m)^ *#.*\n?', '',fmg_f)
                            mg_me_pdf_list = re.findall('pdfsets=\S+',fmg_f)
                            if mg5_aMC_version >= 260:
                                mg_lo = int(os.popen('grep "systematics" '+str(runcmsgrid_file)+' | grep -c madevent').read())
                                mg_nlo = int(os.popen('grep "systematics" '+str(runcmsgrid_file)+' | grep -c aMCatNLO').read())
                            if mg5_aMC_version < 260:
                                mg_lo = int(os.popen('grep -c syscalc '+str(runcmsgrid_file)).read())
                                if mg_nlo > 0:
                                    if mg5_aMC_version < 242:
                                        print("[WARNING] No automated PDF check for this version.")
                                        warning += 1
                                        continue
                                    r_scale = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "reweight_scale"').read()
                                    r_scale = r_scale.split()[0]#.split('.')[1]
                                    if "." in r_scale:
                                        r_scale = r_scale.split('.')[1]
                                    if len(r_scale) == 0 or "true" not in str(r_scale).lower():
                                        print("[ERROR] For NLO MG5_aMC version < 260, one should have .true. = reweight_scale")
                                        error += 1
                                    dir_path = os.path.join(my_path, pi, "InputCards")
                                    input_cards_run_card = find_file(dir_path,"run_card.dat")
                                    r_pdf = os.popen('more '+str(input_cards_run_card)+' | tr -s \' \' | grep "reweight_PDF"').read()
                                    r_pdf = r_pdf.split()[0]
                                    if len(r_pdf) == 0 or "$DEFAULT_PDF_MEMBERS" not in r_pdf:
                                        print("[ERROR] For NLO MG5_aMC version < 260, one should have $DEFAULT_PDF_MEMBERS = reweight_PDF")
                                        error += 1
                            if mg_lo > 0 and mg_nlo > 0:
                                "[ERROR] something's wrong - LO and NLO configs together."
                                error += 1
                            if mg_lo > 0: print("The MG5_aMC ME is running at LO")
                            if mg_nlo > 0: print("The MG5_aMC ME is running at NLO")
                            if mg_nlo > 0 and mg5_aMC_version >= 260:
                                if os.path.isfile(filename_mggpc) is True : store_rwgt_info = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "store_rwgt_info"').read()
                                if len(store_rwgt_info) != 0:
                                    store_rwgt_info_a = store_rwgt_info.split('=')
                                    if "false" in store_rwgt_info_a[0].lower():
                                        print("[ERROR] store_rwgt_info set to"+ str(store_rwgt_info_a[0]) +" for MG5_aMC >= 260.")
                                        print("        This is needed to evaluate systematics. See eg. https://hypernews.cern.ch/HyperNews/CMS/get/generators/4513/1/1/1/1/1/2.html")
                                        error += 1
                                if len(store_rwgt_info) == 0:
                                    print("[ERROR] No store_rwgt_info set for MG5_aMC >= 260.")
                                    print("        This is needed to evaluate systematics. See eg. https://hypernews.cern.ch/HyperNews/CMS/get/generators/4513/1/1/1/1/1/2.html")
                                    error += 1
                            if mg_lo > 0 and mg5_aMC_version >= 260:
                                if os.path.isfile(filename_mggpc) is True : use_syst = os.popen('more '+filename_mggpc+' | tr -s \' \' | grep "use_syst"').read()
                                if len(use_syst) != 0:
                                    use_syst_a = use_syst.split('=')
                                    if "false" in use_syst_a[0].lower():
                                        print("[ERROR] use_syst set to"+ str(use_syst_a[0]) +" for MG5_aMC >= 260.")
                                        error += 1
                                if len(use_syst) == 0:
                                    print("[ERROR] No use_syst set for MG5_aMC >= 260.")
                                    error += 1

                            if mg5_aMC_version < 260: continue
                            mg_me_pdf_list = mg_me_pdf_list[0].split('=')[1].split('\"')[1].split(',')
                            var_count = [s for s in mg_me_pdf_list if "@0" in s]
                            if len(var_count) < 1:
                                print("[WARNING] There will be no PDF variations! Please check the runcmsgrid file in the gridpack.")
                                warning += 1
                            if "UL" in pi and mg_me_pdf_list.count(str(UL_PDFs_N[0])) != 1 and mg_me_pdf_list.count(str(UL_PDFs_N[1])) != 1:
                                if mg_me_pdf_list.count(str(UL_PDFs_N[0])) > 1 or mg_me_pdf_list.count(str(UL_PDFs_N[1])) > 1:
                                    print(" [WARNING] At least one of the default PDF sets ("+UL_PDFs_N+") appear as variation as well or listed more than once.")
                                    warning += 1
                                else:
                                    print("[WARNING] pdfsets in runcmsgrid file does not contain one of the recommended sets:")
                                    print("                                            "+str(UL_PDFs_N[0])+"("+str(UL_PDFs[0])+")")
                                    print("                                            or "+str(UL_PDFs_N[1])+"("+str(UL_PDFs[1])+")")
                                    warning += 1
                                print("Your runcmsgrid file contains these sets:")
                                print(mg_me_pdf_list)
                            if (mg_me_pdf_list.count(str(UL_PDFs_N[0])) > 0 and mg_me_pdf_list.count(str(UL_PDFs_N[0])+"@0") != 0) or (mg_me_pdf_list.count(str(UL_PDFs_N[1])) > 0 and mg_me_pdf_list.count(str(UL_PDFs_N[1])+"@0") != 0):
                                print("[WARNING] Main pdf recommended set ("+str(UL_PDFs_N[0])+" or "+str(UL_PDFs_N[1])+") is listed in runcmsgrid file but it is also included as a variation??")
                                warning += 1
                    if alt_ickkw_c > 0:
                        if match_eff == 1:
                            print("[WARNING] Matched sample but matching efficiency is 1!")
                            warning += 1
                    if ind < 2 and mg_nlo != 1:
                        MGpatch.append(int(os.popen('more '+my_path+'/'+pi+'/'+'runcmsgrid.sh | grep -c "FORCE IT TO"').read()))
                        MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'mgbasedir/Template/LO/SubProcesses/refine.sh').read()))
                        MGpatch.append(int(os.popen('grep -c _CONDOR_SCRATCH_DIR '+my_path+'/'+pi+'/'+'process/madevent/SubProcesses/refine.sh').read()))
                        if MGpatch[0] == 1 and MGpatch[1] == 1 and MGpatch[2] == 1: print("[OK] MG5_aMC@NLO leading order patches OK in gridpack")
                        if MGpatch[0] != 1:
                            print("[ERROR] MG5_aMC@NLO multi-run patch missing in gridpack - please re-create a gridpack")
                            print("           using updated genproductions area")
                            error += 1
                        if MGpatch[1] == 0 or MGpatch[2] == 0:
                            if '10_2' not in cmssw and '9_3' not in cmssw and '7_1' not in cmssw :
                                print("[ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.")
                                print("And the request is using a version "+str(cmssw)+" that does not contain the patch.")
                                print("Please use >= 7_1_32_patch1 or CMSSW_9_3_9_patch1 or 10_2_0_pre2")
                                error += 1
                            elif '7_1' in cmssw:
                                test_version = cmssw.split('_')
                                if (len(test_version) == 4 and int(test_version[3]) < 33) or (len(test_version) == 5 and (int(test_version[3]) < 32 or (int(test_version[3]) == 32 and "patch1" not in cmssw))):
                                    print("[ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.")
                                    print("And the request is using a version "+str(cmssw)+" that does not contain the patch.")
                                    print("In this release, please at least use CMSSW_7_1_32_patch1")
                                    error += 1
                            elif '9_3' in cmssw:
                                test_version = cmssw.split('_')
                                if (len(test_version) == 4 and int(test_version[3]) < 10) or (len(test_version) == 5 and (int(test_version[3]) < 9 or (int(test_version[3]) == 9 and "patch1" not in cmssw))):
                                    print("[ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.")
                                    print("And the request is using a version "+str(cmssw)+" that does not contain the patch.")
                                    print("In this release, please at least use CMSSW_9_3_9_patch1")
                                    error += 1
                            elif '10_2' in cmssw:
                                test_version = cmssw.split('_')
                                if len(test_version) == 4 and int(test_version[3]) < 1:
                                    print("[ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.")
                                    print("And the request is using a version "+str(cmssw)+" that does not contain the patch.")
                                    print("In this release, please at least use CMSSW_10_2_0_pre2")
                                    error += 1
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
                                print("[ERROR] MG5_aMC@NLO LO nthreads patch not made in EOS")
                                error += 1
                                if args.apply_many_threads_patch:
                                    print("Patching for nthreads problem... please be patient.")
                                    if slha_flag == 0:
                                        os.system('python2 ../../Utilities/scripts/update_gridpacks_mg242_thread.py --prepid '+pi)
                                    if slha_flag == 1:
                                        os.system('python2 ../../Utilities/scripts/update_gridpacks_mg242_thread.py --gridpack '+gridpack_cvmfs_path)
                            print("-------------------------EOF MG5_aMC LO/MLM Many Threads Patch Check ----------------------------------")
                            print("*")
                if alt_ickkw_c >= 2 and check[0] == 2 and check[1] == 1 and check[2] == 1 :
                    if alt_ickkw_c > 3 and os.path.isfile(file_pwg_check) is False :
                        print("[WARNING] To check manually - This is a Powheg NLO sample. Please check 'nFinal' is")
                        print("              set correctly as number of final state particles (BEFORE THE DECAYS)")
                        print("                                  in the LHE other than emitted extra parton.")
                        warning += 1
                if alt_ickkw_c == 1 and check[0] == 0 and check[1] == 0 and check[2] == 0 and mg_lo > 0:
                    print("[WARNING] To check manually - This is a matched MadGraph LO sample. Please check 'JetMatching:nJetMax' ="+str(nJetMax)+" is OK and")
                    print("           correctly set as number of partons in born matrix element for highest multiplicity.")
                    warning += 1
                if alt_ickkw_c == 0 and word == "mcatnlo" and mg_nlo > 0 and check[0] == 2 and check[1] == 1 and check[2] == 1 and loop_flag != 1:
                    print("[WARNING] This a MadGraph NLO sample without matching. Please check 'TimeShower:nPartonsInBorn'")
                    print("                                                  is set correctly as number of coloured particles")
                    print("                                                 (before resonance decays) in born matrix element.")
                    warning += 1
                if alt_ickkw_c <= 1 and word == "madgraph" and mg_nlo != 1 and amcnlo_gp is False and (check[0] != 0 or check[1] != 0 or check[2] != 0):
                    print("[ERROR] You run MG5_aMC@NLO at LO but you have  Pythia8aMCatNLOSettings_cfi in fragment")
                    error += 1
        if knd == 1 :
             powhegcheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
             if powhegcheck[0] > 0 :
                 print("[ERROR] Please remove POWHEG settings for MG requests.")
                 error += 1
        if knd == -1 :
             purepythiacheck.append(int(os.popen('grep -c -i Pythia8aMCatNLOSettings '+pi).read()))
             purepythiacheck.append(int(os.popen('grep -c -i PowhegEmission '+pi).read()))
             if purepythiacheck[0] > 0 or purepythiacheck[1] >0 :
                 print("[WARNING] Please remove aMCatNLO or POWHEG settings if this is a pure Pythia request.")
                 print("          If it's not a pure request, in the future, please include madgraph/powheg or amcatnlo")
                 print("          in the name of the dataset")
                 warning += 1
        if loop_flag == 1:
            if mcatnlo_flag == 1:
                print("[ERROR] You are using a loop induced process, [noborn=QCD].")
                print("        Please remove all occurances of Pythia8aMCatNLOSettings from the fragment")
                error += 1
            if nPartonsInBorn_flag == 1:
                print("[ERROR] You are using a loop induced process, [noborn=QCD].")
                print("        Please remove all TimeShower:nPartonsInBorn from the fragment")
                error += 1
        for kk in range (0, 8):
            tunecheck.append(int(os.popen('grep -v "#" '+pi+' | grep -v "annotation" | grep -v "name" | grep -c -i '+tune[kk]).read()))
        if tunecheck[6] == 3 or tunecheck[7] == 3:
            if tunecheck[0] != 3:
                print("[WARNING] Check if there is some extra tune setting")
                warning += 1
        if 'sherpa' in dn.lower():
            print("[WARNING] No automated check of Sherpa ps/tune parameters yet")
            warning += 1
        if 3 not in tunecheck and herwig_flag == 0 and sherpa_flag == 0:
            with open(pi) as f:
                tot = f.read()
                n_ext_par += tot.count('MultipartonInteractions')
                n_ext_par += tot.count('ColourReconnection')
                n_ext_par += tot.count('SpaceShower')
                n_ext_par += tot.count('TimeShower')
                n_ext_par += tot.count('reweightGenEmp')
                print("[WARNING] Number of extra or replaced tune parameters is at least "+str(n_ext_par))
                print("          Please check tune configuration carefully (e.g. are the non-replaced parameters the ones you want)")
                warning += 1
        if 3 not in tunecheck and fsize != 0 and n_ext_par == 0 and herwig_flag == 0 and sherpa_flag == 0:
            if  any(tunecheck[0]<3 and it!=0 for it in tunecheck) :
                print(tunecheck)
                print("[ERROR] Tune configuration may be wrong in the fragment")
                print("        or pythia8CUEP8M1Settings are overwritten by some other parameters as in CUETP8M2T4")
                error += 1
            else :
                print("[WARNING] None standard tune - please check the fragment carefully.")
                warning += 1
        if fsize != 0 and herwig_flag == 0 and sherpa_flag == 0:
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) != 1 :
                print("[WARNING] No parton shower weights configuration in the fragment. Since the Fall18 campaign, we recommend to include Parton Shower weights")
                warning += 1
            if int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()) == 1 :
                if (int(str(cmssw_version)[:1]) == 9 and cmssw_version < 93019) or (int(str(cmssw_version)[:1]) > 9 and cmssw_version < 102030) or (int(str(cmssw_version)[:1]) == 7 and cmssw_version < 71047):
                    print("[ERROR] PS weights in config but CMSSW version is < 10_2_3 for CMSSW version >= 10_X_X or is < 9_3_10 for CMSSW version <= 9_X_X - please check!")
                    error += 1
                psweightscheck.append(int(os.popen('grep -c "from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *" '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettingsBlock," '+pi).read()))
                psweightscheck.append(int(os.popen('grep -c "pythia8PSweightsSettings" '+pi).read()))
                if not (psweightscheck[0] == 1 and psweightscheck[1] == 1 and psweightscheck[2] == 2):
                    print("[ERROR] Parton shower weight configuration not OK in the fragment")
                    error += 1
        if evtgen_flag == 1 and fsize != 0:
            w_tmp, err_tmp = evtgen_check(data_f1)
            warning += w_tmp
            error += err_tmp               
        if evtgen_flag == 0 and fsize != 0 and int(os.popen('grep -c -i EvtGen '+pi).read()) != 0:
            print("[ERROR] EvtGen settings within fragment but no evtgen flag at dataset name")
            error += 1
        if int(os.popen('grep -c -i filter '+pi).read()) > 3 and filter_eff == 1:
            print("[WARNING] Filters in the fragment but filter efficiency = 1")
            warning += 1
        if "Run3" in pi and "PbPb" not in pi and "Run3Summer21" not in pi:
            err_tmp = run3_checks(data_f1,dn,pi)
            error += err_tmp
        if args.develop is False:
            os.popen("rm -rf "+my_path+pi).read()
            os.popen("rm -rf "+my_path+'eos/'+pi).read()
        print("***********************************************************************************")
        print("Number of warnings = "+ str(warning))
        print("Number of errors = "+ str(error))
        if error > 0: print("There is at least 1 error. Request won't proceed to VALIDATION")

# Valid range for exit codes is 0-255
        if error > 255 or error < 0:
            error = 255

# Exit with code, 0 - good, not 0 is bad
        if args.ticket is None:
            if args.bypass_validation:
                continue
            else:
                sys.exit(error)
