#!/usr/bin/env python
import os

def insertSLHA(outputName, massValue):
	with open(outputName, 'r+') as f:
			for x in range(5):
				f.readline()
			pos = f.tell()
			f_remainder = f.read()
			f.seek(pos)
			with open('slha/AMSB_chargino_%dGeV_Isajet780.slha' % massValue, 'r') as slhaFile:
				f.write(slhaFile.read())
			f.write(f_remainder)

def findMassValue(fileName, particleName):
	inputFile = open(fileName, 'r')
	for line in inputFile:
		if particleName in line:
			return line.split()[1]

tuneName = '_TuneCP5_'
if os.environ["CMSSW_VERSION"].startswith('CMSSW_10_'):
	tuneName = '_TuneCP5_PSweights_'

baseConfigFile = 'AMSB_chargino_M-XXXGeV_CTau-YYYcm' + tuneName + '13TeV_pythia8_cff.py'
baseParticleFile = 'geant4_AMSB_chargino.slha'

c = 299792458.0 * 100.0 # cm/s

# xsecs[mass in GeV] = xsec (pb)
xsecs = {
	100 : 34.282,
	200 : 2.709959,
	300 : 0.577095,
	400 : 0.179644,
	500 : 0.06848,
	600 : 0.029636,
	700 : 0.013949,
	800 : 0.0069704,
	900 : 0.00364968,
	1000 : 0.001965386,
	1100 : 0.001082998
}

ctaus = [10, 100, 1000, 10000] # cm

if not os.path.exists('test/'):
	os.mkdir('test')

for mass in xsecs:
	for ctau in ctaus:
		outputConfigFile = ('test/AMSB_chargino_M-%dGeV_CTau-%dcm' % (mass, ctau)) + tuneName + '13TeV_pythia8_cff.py'
		outputParticleFile = 'test/geant4_AMSB_chargino_%dGeV_ctau%dcm.slha' % (mass, ctau)

		os.system('sed "s/XXX/' + str(mass) + '/g" ' + baseConfigFile + ' > ' + outputConfigFile)
		os.system('sed -i "s/YYY/' + str(int(ctau * 10.0)) + '/g" ' + outputConfigFile) # mm
		os.system('sed -i "s/ZZZ/' + str(xsecs[mass]) + '/g" ' + outputConfigFile)
		
		tau = ctau / c * 1.e9 # ns
		width = (1.97326979e-14 / ctau) # GeV

		if os.environ["CMSSW_VERSION"].startswith('CMSSW_10_'):
			if not os.path.exists('slha_withDecay/'):
				os.mkdir('slha_withDecay')
			if not os.path.exists('slha_withDecay/%dcm/' % ctau):
				os.mkdir('slha_withDecay/%dcm' % ctau)
			baseSLHA   = 'slha/AMSB_chargino_%dGeV_Isajet780.slha' % mass
			outputSLHA = 'slha_withDecay/%scm/AMSB_chargino_%sGeV_%scm_Isajet780.slha' % (ctau, mass, ctau)
			os.system('sed "s/%.9g # chargino decay/' + str(width) + ' # chargino decay/g" ' + baseSLHA + ' > ' + outputSLHA)
			mW1ss = findMassValue(outputSLHA, 'w1ss')
			mZ1ss = findMassValue(outputSLHA, 'z1ss')
		else:
			insertSLHA(outputConfigFile, mass)
			mW1ss = findMassValue(outputConfigFile, 'w1ss')
			mZ1ss = findMassValue(outputConfigFile, 'z1ss')

		os.system('sed "s/_MW1SS/' + str(mW1ss) + '/g" ' + baseParticleFile + ' > ' + outputParticleFile)
		os.system('sed -i "s/_MZ1SS/' + str(mZ1ss) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_CTAU/' + str(ctau) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_TAU/' + str(tau) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_WIDTH/' + str(width) + '/g" ' + outputParticleFile)

print 'Created configuration fragments and particle files in directory: ' + os.getcwd() + '/test/'
