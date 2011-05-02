#!/usr/bin/env python

import httplib
import urllib
import sys

def makeRequest(url,params):

    encodedParams = urllib.urlencode(params)
    headers  =  {"Content-type": "application/x-www-form-urlencoded",
                 "Accept": "text/plain"}

    conn  =  httplib.HTTPConnection(url)
    conn.request("POST",  "/reqmgr/create/makeSchema", encodedParams, headers)
    response = conn.getresponse()
    data = response.read()
    if response.status != 303:
        print 'could not post request with following parameters:'
        for item in params.keys():
            print item + ": " + str(params[item])
        print 'Response from http call:'
        print 'Status:',response.status,'Reason:',response.reason
        print 'Explanation:'
        print data
        print "Exiting!"
        sys.exit(1) 
    workflow=data.split("'")[1].split('/')[-1]
    print 'Injected workflow:',workflow
    conn.close()
    return workflow
    
def approveRequest(url,workflow):
    params = {"action": "Approve",
              "checkbox"+workflow: "checked"}

    encodedParams = urllib.urlencode(params)
    headers  =  {"Content-type": "application/x-www-form-urlencoded",
                 "Accept": "text/plain"}

    conn  =  httplib.HTTPConnection(url)
    conn.request("POST",  "/reqmgr/approve/handleApprovalPage", encodedParams, headers)
    response = conn.getresponse()
    if response.status != 200:
        print 'could not approve request with following parameters:'
        for item in params.keys():
            print item + ": " + str(params[item])
        print 'Response from http call:'
        print 'Status:',response.status,'Reason:',response.reason
        print 'Explanation:'
        data = response.read()
        print data
        print "Exiting!"
        sys.exit(1)
    conn.close()
    print 'Approved workflow:',workflow
    return
    
        
###########################################################################
    
import optparse
import os

# Here we define an option parser to handle commandline options..
usage='MakeReqMgrRequest.py <options>'
parser = optparse.OptionParser(usage)
parser.add_option('--release',
                  help='Production release' ,
                  default='',
                  dest='release_name')
parser.add_option('--conditions',
                  help='Conditions Global Tag' ,
                  default='',
                  dest='gt_name')
parser.add_option('--request-id',
                  help='Request identifier' ,
                  default='',
                  dest='req_id')
parser.add_option('--input-ds',
                  help='Input Data Set name' ,
                  default='',
                  dest='input_name')
parser.add_option('--pileup-ds',
                  help='Pile-Up input Data Set name' ,
                  default='',
                  dest='pileup_name')
parser.add_option('--step1-cfg',
                  help='step 1 configuration' ,
                  default='',
                  dest='step1_name')
parser.add_option('--step1-output',
                  help='step 1 output' ,
                  default='',
                  dest='step1_out')
parser.add_option('--step2-cfg',
                  help='step 2 configuration' ,
                  default='',
                  dest='step2_name')
parser.add_option('--step2-output',
                  help='step 2 output' ,
                  default='',
                  dest='step2_out')
parser.add_option('--step3-cfg',
                  help='step 3 configuration' ,
                  default='',
                  dest='step3_name')
parser.add_option('--step3-output',
                  help='step 3 output' ,
                  default='',
                  dest='step3_out')
parser.add_option('--keep-raw',
                  help='RAW keeping flag' ,
                  default=False,
                  action='store_true',
                  dest='keep_raw')

(options,args) = parser.parse_args()

# read configs.txt
config = {}

if not os.path.exists('configs.txt'):
    raise('Missing configs.txt file, aborting')

configsfile = open('configs.txt')
for line in configsfile.readlines():
    array = line.split()
    if len(array) == 2:
        config[array[0]] = array[1]
    else :
        print 'Following line in configs.txt could not be decoded:',line
    
    
# define some global parameters
url = "vocms144.cern.ch:8687"
release = options.release_name
globaltag = options.gt_name
scramarch = "slc5_amd64_gcc434"
group='cmsdataops'
requestor='cmsdataops'

# Request ID string
identifier = options.req_id

workflow_params = {
"inputdataset": options.input_name,
"pileupdataset": options.pileup_name,
"configstep1": config[options.step1_name],
"configstep1outputmodule": options.step1_out, # output module label which should be further processed by step2
"configstep2": config[options.step2_name],
"configstep2outputmodule": options.step2_out, # output module label which should be further processed by step3
"configstep3": config[options.step3_name],
"KeepRAW": options.keep_raw, # False or True, selects if RAW after step 1 is written to tape or not, False for chained processing
}

print workflow_params
 
params = {"CMSSWVersion": release,                                                          
          "InputDataset": workflow_params['inputdataset'],                                  
          "StepOneConfigCacheID": workflow_params['configstep1'],                           
          "StepOneRAWOutputModuleName": workflow_params['configstep1outputmodule'],         
          "StepTwoConfigCacheID": workflow_params['configstep2'],                           
          "StepTwoRECOOutputModuleName":  workflow_params['configstep2outputmodule'],       
          "StepThreeConfigCacheID":  workflow_params['configstep3'],                        
          "RunWhitelist": [],                                                               
          "RunBlacklist": [],                                                               
          "BlockWhitelist": [],                                                             
          "BlockBlacklist": [],                                                             
          "GlobalTag": globaltag,                                                           
          "RequestString": identifier,                                                      
          "RequestPriority": 1,                                                             
          "ScramArch": scramarch,                                                           
          "DataPileup": "",                                                                 
          "MCPileup": workflow_params['pileupdataset'],                                     
          "DbsUrl": "http://cmsdbsprod.cern.ch/cms_dbs_prod_global/servlet/DBSServlet",     
          "inputMode": "couchDB",                                                           
          "Scenario": "pp",                                                                 
          "RequestType": "ReDigi",                                                          
          "Requestor": requestor,                                                           
          "KeepRAW": workflow_params['KeepRAW'],                                            
          "Group": group}                                                                   


workflow = makeRequest(url,params)
approveRequest(url,workflow)
