#! /usr/bin/env python

from PREPtools import *

from optparse import OptionParser, Option

import urllib, random, json

def getIdsFromCampaign(campaign):

  #get the logger
  logger = logging.getLogger("logger")
  #get the full campaign in xml format
  url = 'http://cms.cern.ch/iCMS/prep/requestxml?campid='+campaign
  dom = minidom.parse(urllib2.urlopen(url))
  #parse for the element "request"
  requests = dom.getElementsByTagName('request')
  logger.info( 'Campaing %s currently consists of %d requests' % (campaign, len(requests))) 
  #prepare the container for output
  outputIds = []

  for request in requests:
    #code = request.getElementByTagName('code')
    outputIds.append(request.attributes['code'].value)

  return outputIds

#
# main
#
if __name__ == '__main__':

  usage = 'usage: %prog [options]\n'
  parser = OptionParser(usage=usage,option_class=MyOption)
  parser.add_option("-p", "--prepids", action="extend", help="comma separated list of PREP-IDs, REQUIRED", default=None)
  parser.add_option('-c', '--campid', action='store', help='execute for all requests in the specidied camapign', default=None)


  (options, args) = parser.parse_args()

  if (options.prepids is None and options.campid is None):
    print "\n[Config error] option --prepids or --campid is required\n"
    parser.print_help()
    sys.exit(1)

  if (options.prepids != None and options.campid != None):  
    print "\n[Config error] only one between --prepids and --campid is allowed\n"
    parser.print_help()
    sys.exit(1)

  #if (len(options.prepids) == 0):
  #   parser.print_help()
  #   sys.exit(1)


  # create logger
  logger = logging.getLogger("logger")
  logger.setLevel(logging.DEBUG)
  # create console handler and set level to info
  ch = logging.StreamHandler()
  ch.setLevel(logging.INFO)
  # create console handler and set level to debug
  logfilename='datasets.log'
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

  #get the latest DAS cli
  

  prepids = []
  if options.prepids != None:
    prepids = options.prepids
  else :
    prepids = getIdsFromCampaign(options.campid) 

  buffer = ''

  key = 0 
  host = 'https://cmsweb.cern.ch'
  #host = 'https://dastest.cern.ch'
  totEvents = 0
  for requestId in prepids:
      reqInfo = requestInfo(requestId, str(key))
      if reqInfo.status == 'SUBMIT':
        print requestId+' '+reqInfo.mcdbid 
