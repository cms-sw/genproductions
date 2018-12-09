#!/usr/bin/env python
import os

def insertSLHA(outputName, massValue):
	with open(outputName, 'r+') as f:
			for x in range(6):
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

baseConfigFile = 'AMSB_gluinoToChargino_M-XXXGeV_M-YYYGeV_CTau-ZZZcm_TuneCP5_13TeV_pythia8_cff.py'
baseParticleFile = 'geant4_AMSB_chargino.slha'

c = 299792458.0 * 100.0 # cm/s

charginoMasses = range (400, 901, 100)

# xsecs[mass in GeV] = xsec (pb)
xsecs = {
        700   :  0.432E+01,
        800   :  0.181E+01,
        900   :  0.812E+00,
        1000  :  0.385E+00,
        1100  :  0.191E+00,
        1200  :  0.985E-01,
        1300  :  0.522E-01,
        1400  :  0.284E-01,
        1500  :  0.157E-01,
        1600  :  0.887E-02,
        1700  :  0.507E-02,
        1800  :  0.293E-02,
        1900  :  0.171E-02,
        2000  :  0.101E-02,
        2100  :  0.598E-03,
        2200  :  0.356E-03,
}

ctaus = [10, 100, 1000, 10000] # cm

for mass in xsecs:
        for charginoMass in charginoMasses:
                if charginoMass >= mass:
                        continue
                for ctau in ctaus:
                        outputConfigFile = 'test/AMSB_gluinoToChargino_M-%dGeV_M-%dGeV_CTau-%dcm_TuneCP5_13TeV_pythia8_cff.py' % (mass, charginoMass, ctau)
                        outputParticleFile = 'test/geant4_AMSB_chargino_%dGeV_ctau%dcm.slha' % (charginoMass, ctau)

                        os.system('sed "s/XXX/' + str(mass) + '/g" ' + baseConfigFile + ' > ' + outputConfigFile)
                        os.system('sed -i "s/YYY/' + str(charginoMass) + '/g" ' + outputConfigFile) # mm
                        os.system('sed -i "s/ZZZ/' + str(int(ctau * 10.0)) + '/g" ' + outputConfigFile) # mm
                        os.system('sed -i "s/WWW/' + str(xsecs[mass]) + '/g" ' + outputConfigFile)
                        insertSLHA(outputConfigFile, charginoMass)

                        mW1ss = findMassValue(outputConfigFile, 'w1ss')
                        mZ1ss = findMassValue(outputConfigFile, 'z1ss')

                        tau = ctau / c * 1.e9 # ns
                        width = (1.97326979e-14 / ctau) # GeV

                        os.system('sed "s/_MW1SS/' + str(mW1ss) + '/g" ' + baseParticleFile + ' > ' + outputParticleFile)
                        os.system('sed -i "s/_MZ1SS/' + str(mZ1ss) + '/g" ' + outputParticleFile)
                        os.system('sed -i "s/_CTAU/' + str(ctau) + '/g" ' + outputParticleFile)
                        os.system('sed -i "s/_TAU/' + str(tau) + '/g" ' + outputParticleFile)
                        os.system('sed -i "s/_WIDTH/' + str(width) + '/g" ' + outputParticleFile)

print 'Created configuration fragments and particle files in directory: ' + os.getcwd() + '/test/'
