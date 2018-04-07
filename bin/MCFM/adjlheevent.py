#!/usr/bin/env python
import random
import sys
import os

emptyevt = """<event>
	0	processNUMBER	0	0	0	0							
</event>"""


def getlheeventstring():
	strtoreturn=[]
	for lhefile in lhefiles:
		with open(lhefile,'r') as f:
			inevent = False
			currentevent = []
			for line in f:
				if '<event>' not in line and not inevent:
					continue
				elif '<event>' in line and not inevent: 
					inevent = True
					currentevent.append(line)
				elif '</event>' in line and inevent:
					currentevent.append(line)
					strtoreturn += currentevent
					currentevent = []
				elif '</LesHouchesEvents>' in line:	continue
				else:
					currentevent.append(line)
#	print strtoreturn
	return strtoreturn


def getlheheadfoot():
	strtoreturn=''
	with open(lhefiles[0],'r') as f:
		foundevent=False
		for line in f:
			if '<event>' not in line and not foundevent:
				strtoreturn+=line
			elif '<event>' in line and not foundevent:
				foundevent = True
			elif foundevent and '</LesHouchesEvents>' not in line:
				continue
			else:
				strtoreturn+='<lheventstobefilled>\n'
				strtoreturn+=line
#	print strtoreturn
	return strtoreturn

import argparse
parser = argparse.ArgumentParser()
parser.add_argument("requestednumofevents", type=int)
parser.add_argument("foundnumofevents", type=int)
parser.add_argument("lhefiles", nargs="+")
args = parser.parse_args()
requestednumofevents = args.requestednumofevents
foundnumofevents = args.foundnumofevents
lhefiles = args.lhefiles
lheeventstr=getlheeventstring()
lheheadfoot=getlheheadfoot()

class lheevent(object):
	def __init__(self,lhenum):
		self.lhenum = lhenum

	@property
	def emptyevent(self):
		assert self.lhenum < 1
		tmpemptyevt = emptyevt.replace('processNUMBER',self.processNum)
		return tmpemptyevt

	@property
	def processNum(self):
		if self.lhenum < 1:
                        returnevt = lheeventstr[1]
                        processNum = returnevt.split()[1]
		return processNum

	@property
	def lhecontent(self):	
		if self.lhenum > 0:
			start = 11*(self.lhenum-1)
			end  = 11*(self.lhenum)
			returnevt = lheeventstr[start:end]			
			returnevt =  ''.join(returnevt)
		else:
			returnevt = self.emptyevent 
		assert type(returnevt) == str
		return returnevt
	

trimmedeventsstr=''.join(lheeventstr)
assert trimmedeventsstr.count('<event>') == foundnumofevents, trimmedeventsstr.count('<event>')
if foundnumofevents>requestednumofevents:
	numlist = sorted(random.sample(xrange(1,foundnumofevents+1),requestednumofevents))
	trimmedeventsstr=''
	for num in numlist:
		event = lheevent(num)
		trimmedeventsstr += event.lhecontent
else:
	eventsneeded = requestednumofevents - foundnumofevents
	dummyevent = lheevent(-1)
	for _ in range(eventsneeded):
		trimmedeventsstr+=dummyevent.lhecontent
		trimmedeventsstr+='\n'

finalstr = lheheadfoot.replace('<lheventstobefilled>',trimmedeventsstr)
finalnumofevents = finalstr.count('<event>')
if finalnumofevents != requestednumofevents:
	print finalnumofevents
	raise Exception("final number of events does not match requested number of events")
else:
	with open('cmsgrid_final.lhe','w+') as fout:
		fout.write(finalstr)

