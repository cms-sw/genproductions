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
  parser.add_option('-t', '--data-tier', action="store", help='which data tier, REQUIRED', default=None)
  parser.add_option('-d', "--dbfilename", action="store", help="dbfilename", default='ID-DatasetName_Mapping.txt' )
  parser.add_option("-f", "--force", action="store_true", help="overwrite old results (default=False)", default=False)
  parser.add_option('--www-cli', action="store", help='location of the DAS cli, (default https://cmsweb.cern.ch/das/cli)', default='https://cmsweb.cern.ch/das/cli')


  (options, args) = parser.parse_args()

  if (options.prepids is None and options.campid is None):
    print "\n[Config error] option --prepids or --campid is required\n"
    parser.print_help()
    sys.exit(1)

  if (options.prepids != None and options.campid != None):  
    print "\n[Config error] only one between --prepids and --campid is allowed\n"
    parser.print_help()
    sys.exit(1)

  if options.data_tier is None:
    print "\n[Config error] option -t or --data-tier is required\n"
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
  try:
    urllib.urlretrieve(options.www_cli, filename='DAScli.py')
    from DAScli import get_data, get_value 
  except Exception, e:
    logger.error("error downloading %s: %s" % (options.www_cli, e))
    sys.exit(1)

  

  prepids = []
  if options.prepids != None:
    prepids = options.prepids
  else :
    prepids = getIdsFromCampaign(options.campid) 

  buffer = ''

  key = 0 
  #host = 'https://cmsweb.cern.ch'
  host = 'https://dastest.cern.ch'
  totEvents = 0
  for requestId in prepids:
    try:
      reqInfo = requestInfo(requestId, str(key))
      query='dataset=/'
      query+=reqInfo.dataset+'/'
      query+=reqInfo.campaign+'-' 
      query+=reqInfo.gt+'-v*/'
      query+=options.data_tier
      logger.info('Executing query: '+query)
      data = get_data(host, query, 0, 10, False)
      #print data
      jsondict = json.loads(data)
      logger.debug(json.dumps(jsondict, sort_keys=True, indent=4))
      mongo_query = jsondict['mongo_query']
      rows = []
      if  mongo_query.has_key('fields'):
        filters = mongo_query['fields']
        data = jsondict['data']
        if isinstance(data, list):
          for row in data:
            rows.extend([r for r in get_value(row, filters)])
  
      #print '*****************'
      #print rows
      #print '*****************'

      matches = []
      events  = []
      for row in rows:
        rowsplit = row.split(',') 
        datasetname = rowsplit[3].lstrip('u\'name\': u\'').rstrip('\',')
        cevents = rowsplit[1].lstrip('u\'nevents\': u\'').rstrip('\',')
        matches.append(datasetname)
        events.append(cevents)

      if len(matches) > 0:
        logger.info('matches found for '+requestId+': '+' '.join(matches)) 
      else: 
        logger.info('no dataset found for id '+requestId)
        continue
    
      #if more than one match look for the latest version
      largestversion = 0 
      choice = 0
      for matchindex in range(len(matches)):
        #take the second part of the dataset name
        datasetpart2 = matches[matchindex].split('/')[2]
        version = int(datasetpart2.split('-')[2].lstrip('v'))
        logger.info("version %d, matching index %d" %(version, matchindex))
        if version > largestversion:
          choice = matchindex
          largestversion = version

      logger.info(requestId+' '+matches[choice]+' '+events[choice])
      buffer += requestId+' '+matches[choice]+' '+events[choice]+'\n'
      totEvents += int(events[choice])

      key += 1
    except KeyboardInterrupt:
      print 'you issued ctrl-c, trying to exit gracefully'
      break;

  buffer += 'total events = %d' % (totEvents) 
      

  dbfile = open(options.dbfilename, 'w')
  dbfile.write(buffer)
  dbfile.close()
