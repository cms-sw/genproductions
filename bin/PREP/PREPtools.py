#! /usr/bin/env python

import os, sys, logging, re, string
from subprocess import Popen, PIPE
import urllib2
from xml.dom import minidom
from optparse import OptionParser, Option

class requestInfo:

  #temporary hack to for new cmsDriver customise syntax
  def hackCustomise(self,command): #, custFile, custFunction):
    commandsplit = command.split()
    #remove the genfragment from step1 in reprocessing campaign
    if self.type == 'MCReproc' and 'GenProduction' in commandsplit[1]:
      commandsplit[1] = 'REDIGI'
    cust = ''
    func = ''
    custidx = -1
    funcidx = -1
    for i in range(len(commandsplit)):
      if commandsplit[i] == '--customise':
        cust = commandsplit[i+1]
        custidx = i
      if commandsplit[i] == '--cust_function':
        func = commandsplit[i+1]
        funcidx = i
    if cust == '' and funcidx == '':
      return command
    
    commandsplit[funcidx] = ''
    commandsplit[funcidx+1] = ''

    newcustomise = cust.rstrip('py')
    newcustomise += func
    commandsplit[custidx+1] = newcustomise

    command = ' '.join(commandsplit)
    return command   

  def __init__(self, requestId, key, serverurl='http://cms.cern.ch/iCMS/prep/'):
    self.errorState=False
    self.key=key
    self.reqId=requestId
    self.events=0
    self.time=0
    self.size=0
    self.filterEff=0
    self.matchEff=0
    self.dataset=None
    self.release=None
    self.genProdTag=None 
    self.sequence1=None
    self.sequence2=None
    self.command0=None
    self.command1=None
    self.command2=None
    self.genFragment=None
    self.inputDataset=None
    self.type=None
    self.mcdbid=None
    self.gt=None
    self.eventcont=None
    self.mother=None
    self.priority=-99
    self.campaign=None 
    self.inputDataset=None
    #needed to hack customise in 4_2
    #self.customisename1=None
    #self.customisefunc1=None
    #self.customisename2=None
    #self.customisefunc2=None

    self.url=serverurl+'/requestxml?code='+self.reqId
    self.name0='config_'+self.key+"_0_cfg.py"
    self.name1='config_'+self.key+"_1_cfg.py"
    self.name2='config_'+self.key+"_2_cfg.py"
    self.campaign = self.executeQuery(self.url,'campaign_id', True)

    #self.customisename1=self.executeQuery(self.url,'request_customizename1',False)
    #self.customisefunc1=self.executeQuery(self.url,'request_customizefunction1',False)
    #self.customisename2=self.executeQuery(self.url,'request_customizename2',False)
    #self.customisefunc2=self.executeQuery(self.url,'request_customizefunction2',False)

    self.type       = self.executeQuery(self.url,'campaign_type', True) 
    self.gt         = self.executeQuery(self.url,'request_conditions', True) 
    if self.type == 'Prod':
      self.genProdTag = self.executeQuery(self.url,'request_genproductiontag', True) 
      self.inputDataset= self.executeQuery(self.url,'request_inputfilename', False)
    else :
      self.genProdTag = self.executeQuery(self.url,'request_genproductiontag', False)
      self.inputDataset= self.executeQuery(self.url,'request_inputfilename', True)

    self.sequence1 = self.executeQuery(self.url,'request_sequence1', False)
    if self.sequence1 != '':
      self.command1 = self.executeQuery(serverurl+'/cmsdriverrequest?code='+self.reqId+'&step=1&xml=yes', 'cmsDriver', True).replace('\n', ' ').replace('\\', ' ')   
    self.sequence2 = self.executeQuery(self.url,'request_sequence2', False)
    #this is fragile
    if self.sequence2 != '': #',ALCA:NONE':
      self.command2 = self.executeQuery(serverurl+'/cmsdriverrequest?code='+self.reqId+'&step=2&xml=yes', 'cmsDriver', True).replace('\n', ' ').replace('\\', ' ').replace(',ALCA:NONE', '')
   
    #this should be set only for production campaigns
    if self.type == 'Prod':
      self.genFragment = self.executeQuery(self.url,'request_genfragment', True)
      self.mcdbid = self.executeQuery(self.url, 'request_mcdbid', True)
      if (str(self.mcdbid) != "-1" ):
        self.command0 = self.executeQuery(serverurl+'/cmsdriverrequest?code='+self.reqId+'&step=0&xml=yes', 'cmsDriver', True).replace('\n', ' ').replace('\\', ' ')

    if self.type == 'MCReproc':
      #input dataset is mandatory for reprocessing campaigns
      self.inputDataset = self.executeQuery(self.url,'request_inputfilename', True)
    else:
      self.inputDataset = self.executeQuery(self.url,'request_inputfilename', False)
    self.events    = self.executeQuery(self.url,'request_nbEvent', True)
    self.time      = self.executeQuery(self.url,'request_timeEvent', True)
    self.size      = self.executeQuery(self.url,'request_sizeEvent', True)
    self.filterEff = self.executeQuery(self.url,'request_filterEff', True)
    self.matchEff  = self.executeQuery(self.url,'request_matchEff', True)
    self.dataset   = self.executeQuery(self.url,'request_datasetname', True)
    self.release   = self.executeQuery(self.url,'request_swrelease', True)
    self.eventcont = self.executeQuery(self.url,'request_eventcontent', True)
    self.priority  = self.executeQuery(self.url,'request_priority', True)
    #if this is a clone, then this is the ID of the original request
    self.mother    = self.executeQuery(self.url,'request_cloneof', False)

    #hacks
    #if 'CMSSW_4_1' not in self.release:
    self.command1 = self.hackCustomise(self.command1) #,self.customisename1, self.customisefunc1)
    self.command2 = self.hackCustomise(self.command2) #,self.customisename2, self.customisefunc2)


  def mother(self):
    if self.mother == None:
      return None
    return requestInfo(self.mother)


  def prepareCouchDBInject(self):
    couchUrl="http://cmst1:3AlpesLos@128.142.194.21:5984"
    database_name="wmagent_configcache"
    user_name="cmsdataops"
    group_name="cmsdataops"

    command=''
    if self.command0 != None:
      command += 'inject-to-config-cache %s %s %s %s %s ciao \"ciao\" | grep DocID | awk \'{print \"%s \"$2}\'\n' %(couchUrl, database_name, user_name,group_name,self.name0,self.name0)
    if self.command1 != None:
      command += 'inject-to-config-cache %s %s %s %s %s ciao \"ciao\" | grep DocID | awk \'{print \"%s \"$2}\'\n' %(couchUrl, database_name, user_name,group_name,self.name1,self.name1)
    if self.command2 != None:
      command += 'inject-to-config-cache %s %s %s %s %s ciao \"ciao\" | grep DocID | awk \'{print \"%s \"$2}\'\n' %(couchUrl, database_name, user_name,group_name,self.name2,self.name2)  
  
    return command
  def prepareRequestAndApprove(self):
    #extract step1 data tier
    eventcontent1 = '' 
    eventcontent2 = ''
    commandsplit1 = self.command1.split()
    for i1 in range(len(commandsplit1)):
      if commandsplit1[i1] == '--eventcontent':
        eventcontent1 = commandsplit1[i1+1]
        break;
    eventcontent1split = eventcontent1.split(',')
    eventcontent1 = ''
    for item in eventcontent1split:
      eventcontent1 += item+'output,'
    eventcontent1 = eventcontent1.rstrip(',')  
        
    commandsplit2 = self.command2.split()
    for i2 in range(len(commandsplit2)):
      if commandsplit2[i2] == '--eventcontent':
        eventcontent2 = commandsplit2[i2+1]
        break;     
    eventcontent2split = eventcontent2.split(',')
    eventcontent2 = ''
    for item in eventcontent2split:
      eventcontent2 += item+'output,'
    eventcontent2 = eventcontent2.rstrip(',') 

    command =  'python MakeReqMgrRequest.py --release %s' %(self.release)
    command += ' --conditions %s' %(self.gt)
    command += ' --request-id %s' %(self.reqId)
    command += ' --input-ds %s' %(self.inputDataset) 
    command += ' --pileup-ds /MinBias_TuneZ2_7TeV-pythia6/Summer11-START311_V2-v1/GEN-SIM' 
    command += ' --step1-cfg %s' %(self.name1)
    command += ' --step1-output %s' %(eventcontent1)
    command += ' --step2-cfg %s' %(self.name2)
    command += ' --step2-output %s\n' %(eventcontent2)
    #comamnd += ' --step3-cfg %s'
    #comamnd += ' --step3-output %s'
    #comamnd += ' --keep-raw ' 
    return command

  def executeQuery(self,url, parameter, mandatory):
    logger = logging.getLogger("logger")
    dom = minidom.parse(urllib2.urlopen(url))

    element = dom.getElementsByTagName(parameter)

    if len(element) == 0:
      logger.error('parameter '+parameter+' not found for request '+self.reqId)
      self.errorState=True
      return None

    if len(element[0].childNodes) == 0:
      if mandatory:
        logger.error('mandatory parameter '+parameter+' not found for request '+self.reqId)
        self.errorState=True
        return None
      else:
        return None

    return element[0].firstChild.data

  def printInfo(self):
    logger = logging.getLogger("logger")
    logger.debug('Request '+str(self.reqId))
    summary = ''
    summary += 'requestId:\t'+str(self.reqId)+'\n'
    summary += 'key in batch:\t'+str(self.key)+'\n'
    summary += 'events:\t'+str(self.events)+'\n'
    summary += 'time:\t'+str(self.time)+'\n'
    summary += 'size:\t'+str(self.size)+'\n'
    summary += 'filterEff:\t'+str(self.filterEff)+'\n'
    summary += 'matchEff:\t'+str(self.matchEff)+'\n'
    summary += 'dataset:\t'+str(self.dataset)+'\n'
    summary += 'release:\t'+str(self.release)+'\n'
    summary += 'genProdTag:\t'+str(self.genProdTag)+'\n'
    summary += 'genFragment:\t'+str(self.genFragment)+'\n'
    summary += 'command0:\t'+str(self.command0)+'\n'
    summary += 'command1:\t'+str(self.command1)+'\n'
    summary += '***********************************\n'
    logger.debug(summary)   

  def compareConfig(self, reqInfo, commandId):
    base = self.release == reqInfo.release and  self.genProdTag == reqInfo.genProdTag 
    if (base):
      if commandId == 0:
        return self.command0 == reqInfo.command0
      if commandId == 1:
        return self.command1 == reqInfo.command1
      elif commandId ==2:
        return self.command2 == reqInfo.command2
      else :
        print "unknown step "+str(commandId) 
    return False      

  def execute(self, dir, summaryfile, infos):
    logger = logging.getLogger('logger')
    #if at this stage we are in error condition it meas something went wrong with the parsing of requests  
    if self.errorState:
      logger.error(self.reqId+" FAIL")
      return '',False
      
    summarystring  = str(self.reqId)
    summarystring += '\t'+str(self.release)
    summarystring += '\t'+str(self.eventcont)
    summarystring += '\t'+str(self.priority)
    summarystring += '\t'+str(self.events)
    summarystring += '\t'+str(self.time)
    summarystring += '\t'+str(self.size)
    summarystring += '\t'+str(self.filterEff)
    summarystring += '\t'+str(self.matchEff)
    summarystring += '\t'+str(self.dataset)
    summarystring += '\t'+str(self.gt)
    step0 = ''
    step1 = ''
    step2 = ''
    for info in infos:
      if self.compareConfig(info, 0):
        logger.debug("found existing step0 for request "+self.reqId+": "+info.reqId)
        step0 = info.name0
        break
    for info in infos:
      if self.compareConfig(info, 1):
        logger.debug("found existing step1 for request "+self.reqId+": "+info.reqId)
        step1 = info.name1
        break
    for info in infos:
      if self.compareConfig(info, 2):
        logger.debug("found existing step2 for request "+self.reqId+": "+info.reqId)
        step2 = info.name2
        break

    if step0 == '' or step1 == '' or step2 == '':      
      try:
        script = open('setup.sh', 'w')
        infile=''
        infile += '#!/bin/bash\n'
        infile += 'cd '+ dir+'\n'
        infile += 'source  /afs/cern.ch/cms/cmsset_default.sh\n' 
        if 'CMSSW_4' in self.release:
          infile += 'export SCRAM_ARCH=slc5_amd64_gcc434\n'
        else:   
          infile += 'export SCRAM_ARCH=slc5_ia32_gcc434\n'
        infile += 'scram p CMSSW '+self.release+'\n'
        infile += 'cd '+self.release+'/src\n'
        infile += 'eval `scram runtime -sh`\n'
        if self.genFragment != None:
          infile += 'cvs co -r '+self.genProdTag+' '+self.genFragment+'\n'
        infile += 'scram b\n'
        infile += 'cd ../../\n'
        run0 = ''
        run1 = ''
        run2 = ''
        if step0 == '':
          if self.command0 != None:
            run0 = self.command0+' --python_filename '+self.name0+' --fileout step0.root --no_exec --dump_python'
            logger.debug(run0)
          step0 = self.name0   
        if step1 == '':
          if self.command1 != None:
            run1 = self.command1+' --python_filename '+self.name1+' --fileout step1.root --no_exec --dump_python'
            if self.command0 != None:
              run1 += ' --filein file:step0.root '
              run1 += ' --lazy_download ' 
            logger.debug(run1)
          step1 = self.name1
        if step2 == '':
          if self.command2 != None:
            run2 = self.command2+' --python_filename '+self.name2+' --fileout step2.root --filein file:step1.root --no_exec --dump_python'
            logger.debug(run2)
          step2 = self.name2
        script.write(infile)
        os.chmod('setup.sh', 0755)
        script.close()
        fullcommand = 'source setup.sh'
        if run0 != '':
          fullcommand += '&&'+run0
        if run1 != '':  
          fullcommand += '&&'+run1
        if run2 != '':
          fullcommand += '&&'+run2
        logger.debug('\n'+fullcommand+'\n')
        p = Popen([fullcommand], stdout=PIPE, stderr=PIPE, shell=True)
        p.wait()
        retcode =  p.poll() 
        output = p.stdout.read()
        output += p.stderr.read()
        logger.debug(output)
        if retcode:
          logger.error(self.reqId+" FAIL")
          self.errorState=True
          return '',False

      except Exception, e:
        logger.error(str(e))
        logger.error(self.reqId+" FAIL")
        #cleanu any output that may have been produced  
        os.system('rm '+self.name0+'; rm '+self.name1+'; rm '+self.name2)
        self.errorState=True
        return '', False
        #print "Error: %s" % str(e)


    #cleanup 
    if (os.path.exists(dir+'/'+self.release)):
      os.system('rm -r '+dir+'/'+self.release)
    
    logger.info(self.reqId+" OK")
    if self.command0 != None:
      summarystring += '\t'+step0
    if self.command1 != None:  
      summarystring += '\t'+step1
    if self.command2 != None:  
      summarystring += '\t'+step2
    if self.inputDataset != None:
      summarystring += '\t'+self.inputDataset
    return summarystring+'\n',True 

#allows comma separated options to optparser
class MyOption (Option):
  ACTIONS = Option.ACTIONS + ("extend",)
  STORE_ACTIONS = Option.STORE_ACTIONS + ("extend",)
  TYPED_ACTIONS = Option.TYPED_ACTIONS + ("extend",)
  ALWAYS_TYPED_ACTIONS = Option.ALWAYS_TYPED_ACTIONS + ("extend",)

  def take_action(self, action, dest, opt, value, values, parser):
    if action == "extend":
      lvalue = value.split(",")
      values.ensure_value(dest, []).extend(lvalue)
    else:
      Option.take_action(self, action, dest, opt, value, values, parser)

