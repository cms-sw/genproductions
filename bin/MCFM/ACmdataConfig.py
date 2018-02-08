#ACmdataConfig.py

parameters=['kappa_top','kappa_bot','ghz1','ghz2','ghz4','ghz1_prime2']

class ACConfig(object):
	def __init__(self,coupling):
		self.coupling = coupling

	def giveCouplingPara(self, parameter):
		p0p1 = []
		if parameter == 'kappa_top':			p0p1=[1,0]
		elif parameter == 'kappa_bot':			p0p1=[1,0]
		elif parameter == 'ghz1':			p0p1=self.ghz1
		elif parameter == 'ghz2':			p0p1=self.ghz2
		elif parameter == 'ghz4':			p0p1=self.ghz4
		elif parameter == 'ghz1_prime2':		p0p1=self.ghz1_prime2
		returnstr = 'data {parameter} / ({p0}d0,{p1}d0) /'.format(parameter=parameter, p0=p0p1[0], p1=p0p1[1])
		return returnstr

	@property
	def ghz1(self):
		if self.coupling in ['0PM','0PHf05ph0','0PL1f05ph0','0Mf05ph0']:
			return [1,0]
		else:
			return [0,0]

	@property
	def ghz2(self):
		if self.coupling in ['0PH']:
			return [1,0]
		elif self.coupling in ['0PHf05ph0']:
			return [0.358691,0]
		else:
			return [0,0]

	@property
	def ghz4(self):
		if self.coupling in ['0M']:
			return [1,0]
		elif self.coupling in ['0Mf05ph0']:
			return [0.366354,0]
		else:
			return [0,0]

	@property 
	def ghz1_prime2(self):
		if self.coupling in ['0PL1']:
			return [1,0]
		elif self.coupling in ['0PL1f05ph0']:
			return [-5921.92, 0]
		else:
			return [0,0]

	@classmethod
	def getAllCouplings(cls):
		for coupling in ['0PM','0PL1','0PL1f05ph0', '0PH','0PHf05ph0', '0M','0Mf05ph0']:
			yield cls(coupling)

def getOrigstr(parameter):
	if parameter == 'ghz1':
		return 'data ghz1 / (1d0,0d0) /'
	else:
		return 'data {parameter} / (0d0,0d0) /'.format(parameter=parameter)

def Configmdata(ACConfigObj, mcfmdir):
	assert type(ACConfigObj) is ACConfig
	mdatapath = os.path.join(mcfmdir,'src','User','mdata.f')
	with open(mdatapath,'r') as fin:
		finstr = fin.read()
		origstr = 'data AllowAnomalousCouplings / 0 /'
		replstr = 'data AllowAnomalousCouplings / 1 /'
		foutstr = finstr.replace(origstr,replstr)
		for parameter in parameters:
			origstr = getOrigstr(parameter)
			assert origstr in finstr
			replstr = ACConfigObj.giveCouplingPara(parameter)
			foutstr = foutstr.replace(origstr,replstr)
	with open(mdatapath,'w') as fout:
		fout.write(foutstr)


import argparse,os

if __name__=="__main__":
	parser = argparse.ArgumentParser()
	parser.add_argument('--coupling',type=str)
	parser.add_argument('--mcfmdir', type=str)
	args = parser.parse_args()
	tmplobj = ACConfig(args.coupling)
	mcfmdir = os.path.abspath(args.mcfmdir)
	Configmdata(tmplobj, mcfmdir)

"""
	data AllowAnomalousCouplings / 0 /
	data kappa_top / (0d0,0d0) /
	data kappa_bot / (0d0,0d0) / 
	data ghz1 / (1d0,0d0) /
	data ghz1_prime2 / (0d0,0d0) /

"""
