import os,sys,time
import string
import re
import argparse
import textwrap
os.system('source /afs/cern.ch/cms/PPD/PdmV/tools/McM/getCookie.sh')
os.system('cern-get-sso-cookie -u https://cms-pdmv.cern.ch/mcm/ -o ~/private/prod-cookie.txt --krb --reprocess')
sys.path.append('/afs/cern.ch/cms/PPD/PdmV/tools/McM/')

from rest import McM
from json import dumps
from itertools import groupby
from textwrap import dedent

mcm = McM(dev=False)
page = 0
nrequests = 0;
ntotalrequests = 0;
nevents = 0;
ntotalevents = 0;
mcm_link = "https://cms-pdmv.cern.ch/mcm/"
my_path = '/tmp/'+os.environ['USER']+'/gridpacks/'
os.system('mkdir -p '+my_path)
campaign='prepid=*&member_of_campaign=RunIISummer19UL17wmLHEGEN'
res = mcm.get('requests',query=campaign, page=page)
print len(res)
while len(res) !=0:
        for r in res:
#                print str(r['prepid'])+"  "+str(r['dataset_name'])+"  "+str(r['status'])
                ntotalrequests += 1
                ntotalevents += r['total_events']
                pi = r['prepid']
                os.popen('wget -q '+mcm_link+'public/restapi/requests/get_fragment/'+pi+' -O' +my_path+'/'+ pi).read()
                gridpack_cvmfs_path_tmp = os.popen('grep \/cvmfs '+my_path+'/'+pi).read()
                gp_size = len(gridpack_cvmfs_path_tmp)
                if gp_size != 0:
                   gridpack_cvmfs_path_tmp = re.findall("/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.xz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tgz|/cvmfs/cms\.cern\.ch/phys_generator/gridpacks/.*?tar.gz",gridpack_cvmfs_path_tmp)
                   gridpack_cvmfs_path = gridpack_cvmfs_path_tmp[0]
                   print gridpack_cvmfs_path

        page += 1
        res = mcm.get('requests',query=campaign, page=page)
        time.sleep(0.5)

print "total number of requests "+str(ntotalrequests)
print "total number of events "+str(ntotalevents)

