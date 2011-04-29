#! /usr/bin/env python

from PREPtools import *

from optparse import OptionParser, Option

import random


#
# main
#
if __name__ == '__main__':

  usage = 'usage: %prog [options]\n'
  parser = OptionParser(usage=usage,option_class=MyOption)
  parser.add_option("-p", "--prepids", action="extend", help="comma separated list of PREP-IDs, REQUIRED", default=None)
  parser.add_option("-o", "--outputdir", action="store", help="output directory (default=randomstring)", default=''.join(random.choice(string.ascii_uppercase + string.digits) for x in range(10)))
  parser.add_option("-f", "--force", action="store_true", help="overwrite old results (default=False)", default=False)
  parser.add_option("-u", "--url", action="store", help="the url of the DB, (default=http://cms.cern.ch/iCMS/prep/)", default='http://cms.cern.ch/iCMS/prep/')


  (options, args) = parser.parse_args()

  if (options.prepids is None):
    print "\n[Config error] option -p or --prepids is required\n"
    parser.print_help()
    sys.exit(1)

  if (len(options.prepids) == 0):
     parser.print_help()
     sys.exit(1)

  workingdir = '/tmp' #os.environ['PWD']

  outputdir = workingdir+'/'+options.outputdir

  # create logger
  logger = logging.getLogger("logger")
  logger.setLevel(logging.DEBUG)
  # create console handler and set level to info
  ch = logging.StreamHandler()
  ch.setLevel(logging.INFO)
  # create console handler and set level to debug
  logfilename=outputdir+'.log'
  fh = logging.FileHandler(logfilename)
  fh.setLevel(logging.DEBUG)
  # create formatter
  formatter = logging.Formatter("%(levelname)s - %(asctime)s - %(message)s")
  # add formatter to ch and fh
  ch.setFormatter(formatter)
  fh.setFormatter(formatter)
  # add ch and fh to logger
  logger.addHandler(ch)
  logger.addHandler(fh)

  logger.info('version 1.9')
  logger.info('full debugging output in '+logfilename)

  #create the parameter scan
  if os.path.exists(outputdir) and options.force is False:
    print 'Directory '+outputdir+' exists already, doing nothing.'
    print 'You can overwrite with the --force option'  
    sys.exit(1)  

  os.mkdir(outputdir)
  summary = open(outputdir+'/summary.txt', 'w')
  infile='' 
  infile += 'request ID\tRelease\tEventcontent\tPriority\tEvents\ttime\tsize\tfilterEff\tmatchingEff\tdatasetName\tGlobalTag\tconfigurations\n'

  injectionScript = open(outputdir+'/upload_configs.sh', 'w')
  inInjectionFile = '#!/bin/sh\n'  

  submissionScript = open(outputdir+'/injectAndApprove.sh', 'w')
  inSubmissionScript = '#!/bin/sh\n' 

  infos = []
  key = 0
  totevts=0
  for requestId in options.prepids:
    reqInfo = requestInfo(requestId, str(key), options.url)
    ret = reqInfo.execute(outputdir, reqInfo, infos)
    if ret[1]:
      injectionCommand = reqInfo.prepareCouchDBInject()
      inInjectionFile += injectionCommand
      submissionCommand = reqInfo.prepareRequestAndApprove()
      inSubmissionScript += submissionCommand
      infile += ret[0]
      infos.append(reqInfo)
      my_line=ret[0]
      my_list = my_line.split('\t')
      totevts=totevts+int(my_list[4])
    key = key+1

  #add the total events
  infile += '\n Total evts = ' + str(totevts)

  #add the summary to the log
  logger.info(infile)

  #wrap up  
  summary.write(infile)
  summary.close()
  injectionScript.write(inInjectionFile)
  injectionScript.close()
  submissionScript.write(inSubmissionScript)
  submissionScript.close()
  tarfilename=options.outputdir+'.tgz'
  os.system('rm setup.sh') 
  os.system('cd '+workingdir+'; tar -czf '+tarfilename+' '+options.outputdir);
  #os.system('mv '+tarfilename+' ');
  #os.system('cp '+tarfilename+' /afs/cern.ch/cms/generators/www/ProductionConfigurations/ ')
  #os.system('cp '+logfilename+' /afs/cern.ch/cms/generators/www/ProductionConfigurations/ ')
  os.system('rm -r '+outputdir)

  logger.info('configuration in '+workingdir+'/'+tarfilename) 
