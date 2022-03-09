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

baseConfigFile_wino = 'AMSB_chargino_M-XXXGeV_CTau-YYYcm_TuneCP5_PSweights_13TeV_pythia8_cff.py'
baseParticleFile_wino = 'geant4_AMSB_chargino.slha'

baseConfigFile_higgsino = 'Higgsino_M-XXXGeV_CTau-YYYcm_TuneCP5_PSweights_13TeV_pythia8_cff.py'
baseParticleFile_higgsino = 'geant4_higgsinoAMSB_chargino.slha'

c = 299792458.0 * 100.0 # cm/s

ctaus = [1, 10, 100, 1000, 10000] # cm

# https://twiki.cern.ch/twiki/bin/view/LHCPhysics/SUSYCrossSections13TeVn2x1wino
# plus
# https://twiki.cern.ch/twiki/bin/view/LHCPhysics/SUSYCrossSections13TeVx1x1wino
xsecs_wino = { # [mass in GeV] : xsec (pb)
	100  : 34.282,
	200  : 2.709959,
	300  : 0.577095,
	400  : 0.179644,
	500  : 0.06848,
	600  : 0.029636,
	700  : 0.013949,
	800  : 0.0069704,
	900  : 0.00364968,
	1000 : 0.001965386,
	1100 : 0.001082998
}

# https://twiki.cern.ch/twiki/bin/view/LHCPhysics/SUSYCrossSections13TeVn2x1hino (times 2 for degenerate N1/N2)
# plus
# https://twiki.cern.ch/twiki/bin/view/LHCPhysics/SUSYCrossSections13TeVx1x1hino
xsecs_higgsino = { # [mass in GeV] : xsec (pb)
	100  : 13.53557,
	200  : 1.092653,
	300  : 0.2342024,
	400  : 0.0731865,
	500  : 0.02798925,
	600  : 0.0121587,
	700  : 0.00572751,
	800  : 0.002878806,
	900  : 0.001502429
}

if not os.path.exists('test'):
	os.mkdir('test')

for mass in xsecs_wino:
	for ctau in ctaus:
		outputConfigFile = 'test/AMSB_chargino_M-%dGeV_CTau-%dcm_TuneCP5_PSweights_13TeV_pythia8_cff.py' % (mass, ctau)
		outputParticleFile = 'test/geant4_AMSB_chargino_%dGeV_ctau%dcm.slha' % (mass, ctau)

		os.system('sed "s/XXX/' + str(mass) + '/g" ' + baseConfigFile_wino + ' > ' + outputConfigFile)
		os.system('sed -i "s/YYY/' + str(int(ctau * 10.0)) + '/g" ' + outputConfigFile) # mm
		os.system('sed -i "s/ZZZ/' + str(xsecs_wino[mass]) + '/g" ' + outputConfigFile)
		insertSLHA(outputConfigFile, mass)

		mW1ss = findMassValue(outputConfigFile, 'w1ss')
		mZ1ss = findMassValue(outputConfigFile, 'z1ss')

		tau = ctau / c * 1.e9 # ns
		width = (1.97326979e-14 / ctau) # GeV

		os.system('sed "s/_MW1SS/' + str(mW1ss) + '/g" ' + baseParticleFile_wino + ' > ' + outputParticleFile)
		os.system('sed -i "s/_MZ1SS/' + str(mZ1ss) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_CTAU/' + str(ctau) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_TAU/' + str(tau) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_WIDTH/' + str(width) + '/g" ' + outputParticleFile)

for mass in xsecs_higgsino:
	for ctau in ctaus:
		outputConfigFile = 'test/Higgsino_M-%dGeV_CTau-%dcm_TuneCP5_PSweights_13TeV_pythia8_cff.py' % (mass, ctau)
		outputParticleFile = 'test/geant4_higgsinoAMSB_chargino_%dGeV_ctau%dcm.slha' % (mass, ctau)

		os.system('sed "s/XXX/' + str(mass) + '/g" ' + baseConfigFile_higgsino + ' > ' + outputConfigFile)
		os.system('sed -i "s/YYY/' + str(int(ctau * 10.0)) + '/g" ' + outputConfigFile) # mm
		os.system('sed -i "s/ZZZ/' + str(xsecs_higgsino[mass]) + '/g" ' + outputConfigFile)
		insertSLHA(outputConfigFile, mass)

		mW1ss = findMassValue(outputConfigFile, 'w1ss')
		mZ1ss = findMassValue(outputConfigFile, 'z1ss')
		mZ2ss = findMassValue(outputConfigFile, 'z2ss')

		tau = ctau / c * 1.e9 # ns
		width = (1.97326979e-14 / ctau) # GeV

		os.system('sed "s/_MW1SS/' + str(mW1ss) + '/g" ' + baseParticleFile_higgsino + ' > ' + outputParticleFile)
		os.system('sed -i "s/_MZ1SS/' + str(mZ1ss) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_MZ2SS/' + str(mZ2ss) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_CTAU/' + str(ctau) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_TAU/' + str(tau) + '/g" ' + outputParticleFile)
		os.system('sed -i "s/_WIDTH/' + str(width) + '/g" ' + outputParticleFile)

print 'Created configuration fragments and particle files in directory: ' + os.getcwd() + '/test/'
