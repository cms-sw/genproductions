# EXAMPLE ON HOW TO RUN
# python ./compute_cross_section.py -f datasets.txt 

from optparse import OptionParser
import os
import sys
import commands
import re
import datetime
from time import sleep

def str_to_bool(s):
    if s == 'True':
         return True
    elif s == 'False':
         return False

if __name__ == "__main__":

    parser = OptionParser()
    parser.add_option('-f', '--datasets'      , dest="inputdataset",  default='/DYToLL_2J_13TeV-amcatnloFXFX-pythia8/RunIISummer16MiniAODv2-PUMoriond17_80X_mcRun2_asymptotic_2016_TrancheIV_v6_ext1-v1/MINIAODSIM',   help='primary dataset names')
    parser.add_option('-c', '--campaign'      , dest="campaign",      default='Moriond17',      help='MC campaign to be searched for (e.g. Moriond17, ...)')
    parser.add_option('-n', '--events'        , dest="events",        default=int(1e6),         help='number of events to calculate the cross section')
    parser.add_option('-d', '--datatier'      , dest="datatier",      default='MINIAODSIM',     help='datatier (e.g. GEN-SIM, MINIAOD, ...)')
    parser.add_option(      '--mcm'           , dest="mcm",           default=False,            help='use McM prepID instead of dataset name')
    parser.add_option('-s', '--skipexisting'  , dest="skipexisting",  default=False,            help='skipexisting existing output files containing xsec results')
    parser.add_option(      '--debug'         , dest="debug",         default=False,            help='use debug options (debug couts...)')

    (args, opts) = parser.parse_args(sys.argv)
    debug = str_to_bool(str(args.debug))
    mcm = str_to_bool(str(args.mcm))
    skipexisting = str_to_bool(str(args.skipexisting))
    if debug: print 'args.mcm',args.mcm,'mcm',mcm,'debug',debug
    if debug: print 'debug is True!'
    if debug and mcm: print 'mcm is True!'
    
    if debug:
        print
        print 'RUNNING PARAMS: '
        print '                debug                 = ' + str(debug)
        print '                dataset               = ' + args.inputdataset
        print '                MC campaign           = ' + args.campaign
        print '                Datatier              = ' + args.datatier
        print '                number of events      = ' + str(args.events)
        print '                use McM prepID        = ' + str(mcm)
        print '                skipexisting          = ' + str(skipexisting)
        print

    das_cmd = "/cvmfs/cms.cern.ch/common/dasgoclient"

    # if mcm is specified, retrieve dataset name from prepID:
    if mcm:
        if "/" in str(args.inputdataset): 
            print "not a McM prepID format, please check"
            sys.exit(1)
        # load McM
        sys.path.append('/afs/cern.ch/cms/PPD/PdmV/tools/McM/')
        from rest import McM
        mcm = McM()
        # retrieve request with given prepid
        temp = sys.stdout
        f = open('/dev/null', 'w')
        sys.stdout = f
        request = mcm.get('requests', str(args.inputdataset))
        sys.stdout = temp
        if debug: print 'request prepid',request['prepid']
        # search dataset name as returned by mcm
        dataset_used = str(request['output_dataset'][0])
        primary_dataset_name = dataset_used.split('/')[1]
    else:
        # search dataset name as name + campaign + datatier
        primary_dataset_name = args.inputdataset.split('/')[1]
        command=das_cmd+" --limit=0 --query=\"dataset dataset=/"+primary_dataset_name+"/*"+args.campaign+"*/"+args.datatier+"\""
        dataset_used = commands.getstatusoutput(command)[1].split("\n")
        if debug: print 'command',command,'\n'
        dataset_used = [x.strip() for x in dataset_used][0]
    
    if skipexisting and os.path.isfile("xsec_"+primary_dataset_name+".log"): 
        print "xsec_"+primary_dataset_name+".log existing and NO skipexisting asked, skipping"
        # sys.exit(0)
    else:
        if debug: print 'dataset_used',dataset_used
        if debug: print 'primary_dataset_name',primary_dataset_name,'\n'
        # pick up only the first dataset of the list
        if debug: print 'dataset_used',dataset_used
        # retrieve filelist
        command=das_cmd+" --limit=100 --query=\"file dataset="+dataset_used+"\" "
        if debug: print 'command',command
        filelist_used = "/store"+commands.getstatusoutput(command)[1].replace("\n",",").split("/store",1)[1] 
        if debug: 
            print 'filelist_used',filelist_used.split(',')[0]
            filelist_used = filelist_used.split(',')[0]
        # compute cross section
        command = 'cmsRun genXsec_cfg.py inputFiles=\"'+filelist_used+'\" maxEvents='+str(args.events)+" 2>&1 | tee xsec_"+primary_dataset_name+".log"
        print command
