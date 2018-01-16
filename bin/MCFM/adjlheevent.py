#!/bin/env python
import random
import sys
import os

def getlheeventstring():
	strtoreturn=[]
	with open('cmsgrid_final.lhe','r') as f:
		foundevent = False
		for line in f:
			if '<event>' not in line and not foundevent:
				continue
			elif '<event>' in line and not foundevent: 
				foundevent = True
				strtoreturn.append(line)
			elif '</LesHouchesEvents>' in line:	continue
			else:
				strtoreturn.append(line)
#	print strtoreturn
	return strtoreturn


def getlheheadfoot():
	strtoreturn=''
	with open('cmsgrid_final.lhe','r') as f:
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

lheeventstr=getlheeventstring()
lheheadfoot=getlheheadfoot()
requestednumofevents = int(sys.argv[1])
foundnumofevents = int(sys.argv[2])

class lheevent(object):
	def __init__(self,lhenum):
		self.lhenum = lhenum
		self.lhecontent = self._getContent(self.lhenum)

	def _getContent(self,lhenum):
		start = 11*(lhenum-1)
		end  = 11*(lhenum)
		return lheeventstr[start:end]		
	
	def concentrate(self):
		return ''.join(self.lhecontent)
		
numlist = random.sample(xrange(1,foundnumofevents), requestednumofevents)
trimmedeventsstr=''
for num in numlist:
	event = lheevent(num)
	trimmedeventsstr+=event.concentrate()
finalstr = lheheadfoot.replace('<lheventstobefilled>',trimmedeventsstr)
finalnumofevents = finalstr.count('<event>')
if finalnumofevents != requestednumofevents:
	raise Exception("final number of events does not match requested number of events")
else:
	os.system('rm cmsgrid_final.lhe')
	with open('cmsgrid_final.lhe','w+') as fout:
		fout.write(finalstr)

