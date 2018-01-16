#!/bin/env python
from optparse import OptionParser
from argparse import ArgumentParser
import os, sys

def checkallfiles():
	if not os.path.exists('./runcmsgrid_template.sh'):
		os.system('wget https://raw.githubusercontent.com/carolhungwt/genproductions/bc81e783add413edf33d24e4f068ecaf362eb9f1/bin/MCFM/runcmsgrid_template.sh')
	if not os.path.exists('./adjlheevent.py'):
		os.system('wget https://raw.githubusercontent.com/carolhungwt/genproductions/c68777f521ab38c55a8dddbef39c93fa06f8634c/bin/MCFM/adjlheevent.py')
	if not os.path.exists('./ACmdataConfig.py'):
		os.system('wget https://raw.githubusercontent.com/carolhungwt/genproductions/64796590df6a1f068fca32b12d9c2b9c87189fb2/bin/MCFM/ACmdataConfig.py')
		

def getInputBase(inputcard):
	try:
		inputcard = inputcard.split('/')[-1]
		cardbase = inputcard.split('.DAT')[0]
		return cardbase
	except:
		raise IOError('Inputcard %s is not in the right format'%(inputcard))

class RunMcfmOP():

	def __init__(self):
		self.parser = ArgumentParser()
		self.parser.add_argument("-i", "--inputcard", dest="inputcard",type=str,
								help="InputCard",
								required=True)
		self.parser.add_argument('-d','--datasetname',dest='datasetname',type=str,
								help='Output grid name',
								required=True)
		self.parser.add_argument('--coupling', type=str, 
								help="coupling: '0PH','0M','0PM','0PL1','0PHf05ph0','0Mf05ph0',0PL1f05ph0'", 
								required=True)
		self.parser.add_argument('--gridfile', dest='gridfile',type=str,
								help='Input/Output gridfile name.', 
								default='grid')
		self.parser.add_argument("-m", "--method", dest="method",type=str,
								help="Method in running MCFM",
								default='mdata')
		self.parser.add_argument('-n','--nevent', dest='nevent',type=int,
								help='Number of events to run. Default is 1.',
								default='1')
		self.parser.add_argument('-s', '--seed',dest='seed',type=int,
								help='Random seed running the job.',
								default='123456')
		self.parser.add_argument('-q','--queue',dest='queue',type=str,
								help='bqueue for submission. Default is 1nw',
								default='1nw')
		self.parser.add_argument('-c','--cmssw',dest='cmssw',type=str,
								help='CMSSW release to install. Default is CMSSW_9_3_0',
								default='CMSSW_9_3_0')
		self.parser.add_argument('--scram',dest='scram_arch',type=str,
								help="Specify scram arch version if needed. Default=slc6_amd64_gcc630",
								default='slc6_amd64_gcc630')
		self.parser.add_argument('--gridonly', dest='gridonly',type=int,
								help='Write gridpack only',
								default='0')
		self.parser.add_argument('--runtestonly',dest='runtestonly',type=int,
								help='Run test job only.',
								default='0')
		self.parser.add_argument('--setupenvonly', dest='setupenvonly', type=int, 
								help='Setup the environment and compile MCFM only',
								default='0')

		self.args = self.parser.parse_args()
		self.curdir = os.getcwd()
		if not self.args.setupenvonly:	
			try:			self.cardbase = getInputBase(self.args.inputcard)
			except:			raise KeyError('Inputcard must be provided')	
		self.mcfmdir =  self.givemcfmdir()
		print self.mcfmdir
		if not os.path.isdir(self.mcfmdir):
			self.downloadmcfm()

	def execute(self):
		checkallfiles()
		if(self.args.setupenvonly == 1):
			self.editmakefile()
			self.appendtocompile()
		elif(self.args.gridonly == 1):
			self.writeruncmsgrid()
			self.replaceInputDat('w')
			self.replaceInputDat('r')
		elif(self.args.setupenvonly == 0 and self.args.runtestonly == 0 and self.args.gridonly==0):
			self.writeruncmsgrid()
			self.replaceInputDat('w')
			self.replaceInputDat('r')
			self.editmakefile()
			self.appendtocompile()
		self.writesubmissionbash()
		self.submittoqueue()

	def givemcfmdir(self):
		if self.args.runtestonly or self.args.setupenvonly or self.args.gridonly:
			return 'MCFM-7.0_JHUGen'
		else:
			return 'MCFM-7.0_JHUGen_%s' % (self.datasetname)

	def editmakefile(self):
		fmakefile = open('%s/makefile' % (self.mcfmdir))
		tempstr = fmakefile.read()
		tempstr = tempstr.replace('CERNLIB     = ','CERNLIB     =/usr/lib64/cernlib/2006/lib/ ',1)
		tempstr = tempstr.replace('LHAPDFLIB   = ','LHAPDFLIB   =/cvmfs/cms.cern.ch/%s/external/lhapdf/6.2.1/lib/' % (self.args.scram_arch),1)
		tempstr = tempstr.replace('PDFROUTINES = NATIVE','PDFROUTINES = LHAPDF',1)
		fnewmake = open('%s/makefile' % (self.mcfmdir), 'w')
		fnewmake.write(tempstr)
		fnewmake.close()

	def appendtocompile(self):
		with open('%s/compile.sh' % (self.mcfmdir),'a') as f:
			f.write('echo DONE>DONE\n')
		os.system('chmod 755 %s/compile.sh' % (self.mcfmdir))

	def downloadmcfm(self):
		tempdir=os.path.join(self.curdir+self.mcfmdir)
		if os.path.isdir(tempdir):	
			print '%s already exists'%(self.mcfmdir)
		else:
			os.system('git clone https://github.com/usarica/MCFM-7.0_JHUGen.git %s'%(self.mcfmdir))
		assert os.path.isdir(self.mcfmdir)==1

	def replaceInputDat(self,action):
		readin,writeout=0,0
		if (action == 'read' or action=='r'):		
			readin=1 
			filename = 'readInput.DAT'
		elif (action == 'write' or action== 'w'):	
			writeout=1
			filename = 'writeInput.DAT'
		filepath = os.path.join(self.mcfmdir,filename)
		finput =open(filepath,'w')
		try: 		ftemp = open(self.args.inputcard,'r')
		except IOError:		
			raise IOError(self.args.inputcard+' does not exists')
		tempstr = ftemp.readlines()
		for templine in tempstr:
			line = templine	
			if (readin):
				if('[readin]' in line):			line = '.true.		[readin] \n'	
				if('[writeout]' in line):		line = '.false.		[writeout] \n'
				if('[ingridfile]' in line):		line = "'%s_grid'		[ingridfile] \n" %(self.datasetname)
				if('[outgridfile' in line):		line = "''		[outgridfile] \n"
			if(writeout):
				line = line.replace('NEVENT',str(self.args.nevent),1)
				line = line.replace('SEED', str(self.args.seed),1)
				if('[readin]' in line):			line = '.false.		[readin] \n '
				if('[writeout]' in line):		line = '.true.		[writeout] \n'
				if('[ingridfile]' in line):		line = "''		[ingridfile] \n" 
				if('[outgridfile' in line):		line = "'grid'		[outgridfile] \n" 
			if('[LHAPDF group]' in line):	line = "'NNPDF31_lo_as_0130'  [LHAPDF group]\n"
			finput.write(line)

	def writesubmissionbash(self):
		substr = '#!/bin/bash \n'
		substr+='basedir=%s\n'%(self.curdir)
		substr+='mcfmdir=%s/%s \n'%(self.curdir, self.mcfmdir)
		substr+='cd ${mcfmdir} \n scram_arch_version=%s \n'%(self.args.scram_arch)
		substr+='cmssw_version=%s\n'%(self.args.cmssw)
		substr+="export VO_CMS_SW_DIR=/cvmfs/cms.cern.ch\nsource $VO_CMS_SW_DIR/cmsset_default.sh\nexport SCRAM_ARCH=${scram_arch_version} \n"	
		if (not self.args.runtestonly and not self.args.gridonly):
			mcfmsubmitfile = 'MCFM_submit_setupenvonly.sh'
			substr+='scramv1 project CMSSW ${cmssw_version} \n'
			substr+='cd ${mcfmdir}/${cmssw_version}/src \neval `scramv1 runtime -sh`\n cd ${mcfmdir}\n'
			substr+='python ../ACmdataConfig.py --coupling %s --mcfmdir ${mcfmdir}\n'%(self.args.coupling)
			substr+='sed -i "s~InitPDFset(checkpath([\']PDFsets/[\']//PDFname))~InitPDFsetByName(PDFname)~" src/Parton/pdfwrap_lhapdf.f\n'
			substr+='chmod 755 compile.sh \n'
			substr+='./compile.sh \n'
			substr+='while [ ! -f DONE ]; do sleep 2m; done \n'
		if(not self.args.setupenvonly):
			mcfmsubmitfile = 'MCFM_submit_{0}.sh'.format(self.datasetname)
			substr+='cd ${mcfmdir}/${cmssw_version}/src \neval `scramv1 runtime -sh`\n' 
			substr+='cd ${mcfmdir}\n'
			substr+='ln -sf ./Bin/process.DAT process.DAT \n ln -sf ./Bin/hto_output.dat hto_output.dat \n ln -sf ./Bin/ffwarn.dat ffwarn.dat \n ln -sf ./Bin/ffperm5.dat ffperm5.dat \n ln -sf ./Bin/fferr.dat fferr.dat \n ln -sf ./Bin/dm_parameters.DAT dm_parameters.DAT \n ln -sf ./Bin/br.sm1 br.sm1 \n ln -sf ./Bin/br.sm2 br.sm2 \n \n'  
			substr+='./Bin/mcfm writeInput.DAT \n'
			substr+='gridfileexists=false \nwhile [ ${gridfileexists} = false ]; do gridfile=($(ls|grep _grid)); if [ ${#gridfile[@]} -eq 1 ]; then gridfileexists=true; else sleep 2m; fi; done \n'
			substr+="mv ${gridfile[0]} %s_grid \nchmod 755 runcmsgrid.sh \nrm *.lhe \n"%(self.datasetname)
 			substr+='cp ../adjlheevent.py ./ \n'
			substr+='echo tarball will be found at ${basedir} \n'
			substr+='tar -cvzf ${basedir}/MCFM_%s_%s_%s_%s.tgz ./ \n' % (self.args.method, self.args.scram_arch, self.args.cmssw, self.datasetname)
			substr+='#cleaning up part\n'
			substr+='cd ${basedir} \n' 
			substr+='rm -rf ${mcfmdir}\n'
		fsub = open(mcfmsubmitfile,'w')
		fsub.write(substr)
		fsub.close()
		os.system('chmod 755 %s'%(mcfmsubmitfile))

	def submittoqueue(self):
		fsubbash ='MCFM_submit_%s.sh'%(self.datasetname)
#		os.system('%s' % (fsubbash))
		os.system('bsub -q %s %s' % (self.args.queue,fsubbash))

	def writeruncmsgrid(self):
		if not os.path.isfile('runcmsgrid_template.sh'):
			print 'cannot find runcmsgrid_template.sh, downloading from github...'
			os.system('wget https://raw.githubusercontent.com/cms-sw/genproductions/master/bin/MCFM/runcmsgrid_template.sh')
		with open('runcmsgrid_template.sh','r') as ftemp:
			with open(os.path.join(self.mcfmdir,'runcmsgrid.sh'),'w') as fout:
				for templine in ftemp.readlines():
					line  = templine
					if('CMSSW_VERSION_REPLACE' in line):		line = '        cmssw_version=%s \n' % (self.args.cmssw)
					if('SCRAM_ARCH_VERSION_REPLACE' in line):	line = '        scram_arch_version=%s \n' % (self.args.scram_arch)
					if('./mcfm' in line and 'INPUT.DAT' in line):	line = './Bin/mcfm readInput.DAT \n'	
					line = line.replace('INPUT.DAT','readInput.DAT')
					fout.write(line)
			fout.close()
		ftemp.close()		


#if __name__=='__main__':
domcfm = RunMcfmOP()
domcfm.execute()

