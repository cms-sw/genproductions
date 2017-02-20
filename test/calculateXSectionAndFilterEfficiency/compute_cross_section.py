# EXAMPLE ON HOW TO RUN
# python ./compute_cross_section.py -f datasets.txt 

from optparse import OptionParser
import os
import sys
import commands
import re
import datetime
from time import sleep

if __name__ == "__main__":

    parser = OptionParser()
    parser.add_option('-f', '--datasets'      , dest="inputdataset",  default='/DYToLL_2J_13TeV-amcatnloFXFX-pythia8/RunIISummer16MiniAODv2-PUMoriond17_80X_mcRun2_asymptotic_2016_TrancheIV_v6_ext1-v1/MINIAODSIM',   help='primary dataset names')
    parser.add_option('-c', '--campaign'      , dest="campaign",      default='Moriond17',      help='MC campaign to be searched for (e.g. Moriond17, ...)')
    parser.add_option('-n', '--events'        , dest="events",        default=int(1e6),         help='number of events to calculate the cross section')
    parser.add_option('-d', '--datatier'      , dest="datatier",      default='MINIAODSIM',     help='datatier (e.g. GEN-SIM, MINIAOD, ...)')
    parser.add_option(      '--debug'         , dest="debug",         default=False,            help='datatier (e.g. GEN-SIM, MINIAOD, ...)')

    (args, opts) = parser.parse_args(sys.argv)
    if args.debug == 'False': args.debug = False
      
    if args.debug:
        print
        print 'RUNNING PARAMS: '
        print '                debug                 = ' + args.debug
        print '                dataset               = ' + args.inputdataset
        print '                MC campaign           = ' + args.campaign
        print '                Datatier              = ' + args.datatier
        print '                number of events      = ' + str(args.events)
        print

    # for dataset in args.inputdataset:
    primary_dataset_name = args.inputdataset.split('/')[1]
    if args.debug: print 'primary_dataset_name',primary_dataset_name,'\n'
    command="/cvmfs/cms.cern.ch/common/das_client --limit=0 --query=\"dataset dataset=/"+primary_dataset_name+"/*"+args.campaign+"*/"+args.datatier+"\""
    if args.debug: print 'command',command,'\n'
    dataset_used = commands.getstatusoutput(command)[1].split("\n")
    # pick up only the first dataset of the list
    dataset_used = [x.strip() for x in dataset_used][0]
    if args.debug: print 'dataset_used',dataset_used
    # retrieve filelist
    command="/cvmfs/cms.cern.ch/common/das_client --limit=100 --query=\"file dataset="+dataset_used+"\" "
    filelist_used = "/store"+commands.getstatusoutput(command)[1].replace("\n",",").split("/store",1)[1] 
    if args.debug: print 'filelist_used',filelist_used.split(',')[0]
    # compute cross section
    command = 'cmsRun ana.py inputFiles=\"'+filelist_used+'\" maxEvents='+str(args.events)+" 2>&1 | tee xsec_"+primary_dataset_name+".log"
    print command
