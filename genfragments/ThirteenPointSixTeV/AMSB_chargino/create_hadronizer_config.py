#!/usr/bin/env python
import os
import sys

c = 299792458.0 * 100.0 # cm/s

baseDir = os.getcwd() #os.environ['CMSSW_BASE'] + '/src/DisappTrks/SignalMC/'

if not os.path.exists(baseDir + '/geant4'):
	os.mkdir(baseDir + '/geant4')
if not os.path.exists(baseDir + '/geant4_higgsino'):
	os.mkdir(baseDir + '/geant4_higgsino')
if not os.path.exists(baseDir + '/hadronizers'):
        os.mkdir(baseDir + '/hadronizers')

def findMassValue(fileName, particleName):
	inputFile = open(fileName, 'r')
	for line in inputFile:
		if particleName in line:
			return line.split()[1]

################################################################
# script runs in two steps:
#	first create the fragments, then the user needs to scram b
# 	then the step1 configs are created
################################################################
scriptStep = 1
if len(sys.argv) > 1:
	scriptStep = 2

# xsecsWino[mass in GeV] = xsec (pb)
xsecsWino = { m : -1. for m in range(100, 2100, 100) }

# xsecsHiggsino[mass in GeV] = xsec (pb)
xsecsHiggsino = { m : -1. for m in range(100, 1600, 100) }

ctaus = [1, 10, 100, 1000, 10000] # cm

#xqcut varies depending on the mass of the chargino
xqcut = 0.0

################################################################
# step 1: create the gen fragments
################################################################
if scriptStep == 1:
	# first wino-like LSP case
	baseConfigFile   = baseDir + '/AMSB_chargino_M-XXXGeV_CTau-YYYcm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py'
	baseParticleFile = baseDir + '/geant4_AMSB_chargino_XXXGeV_ctauYYYcm.slha'
	for mass in xsecsWino:
		if mass >= 50 and mass <= 400:
			xqcut = 40.0
		if mass >= 500 and mass <= 900:
			xqcut = 50.0
		if mass >= 1000 and mass <= 1900:
			xqcut = 60.0
		if mass == 2000:
			xqcut = 70.0
		for ctau in ctaus:
			if not os.path.exists(baseDir + '/geant4/'+str(ctau)+'cm'):
				os.mkdir(baseDir + '/geant4/'+str(ctau)+'cm')
			if not os.path.exists(baseDir + '/hadronizers/'+str(ctau)+'cm'):
				os.mkdir(baseDir + '/hadronizers/'+str(ctau)+'cm')
			outputConfigFile   = baseDir + '/hadronizers/'+str(ctau)+'cm/AMSB_chargino_M%dGeV_CTau%dcm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py' % (mass, ctau)
			outputParticleFile = baseDir + '/geant4/'+str(ctau)+'cm/geant4_AMSB_chargino_%dGeV_ctau%dcm.slha' % (mass, ctau)
			slhaFile           = baseDir + '/SLHA_withDecay/AMSB_chargino_%dGeV_CTAUcm_Isajet780.slha' % (mass)
			os.system('sed "s/XXX/' + str(mass) + '/g" ' + baseConfigFile + ' > ' + outputConfigFile)
			os.system('sed -i "s/YYY/' + str(int(ctau * 10.0)) + '/g" ' + outputConfigFile) # mm
			os.system('sed -i "s/ZZZ/' + str(xsecsWino[mass]) + '/g" ' + outputConfigFile)
			os.system('sed -i "s/AAA/' + str(xqcut) + '/g" ' + outputConfigFile)
			mW1ss = findMassValue(slhaFile, 'w1ss')
			mZ1ss = findMassValue(slhaFile, 'z1ss')
			tau = ctau / c * 1.e9 # ns
			width = (1.97326979e-14 / ctau) # GeV
			os.system('sed "s/_MW1SS/' + str(mW1ss) + '/g" ' + baseParticleFile + ' > ' + outputParticleFile)
			os.system('sed -i "s/_MZ1SS/' + str(mZ1ss) + '/g" ' + outputParticleFile)
			os.system('sed -i "s/_CTAU/' + str(ctau) + '/g" ' + outputParticleFile)
			os.system('sed -i "s/_TAU/' + str(tau) + '/g" ' + outputParticleFile)
			os.system('sed -i "s/_WIDTH/' + str(width) + '/g" ' + outputParticleFile)
	print('Created electroweak (wino-like) configuration fragments and particle files in directory: ' + baseDir + '/hadronizers')

	# now higgsino-like LSP case
	baseConfigFile   = baseDir + '/Higgsino_M-XXXGeV_CTau-YYYcm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py'
	baseParticleFile = baseDir + '/geant4_higgsino_XXXGeV_ctauYYYcm.slha'
	for mass in xsecsHiggsino:
		if mass >= 100 and mass <= 400:
			xqcut = 40.0
		if mass >= 500 and mass <= 900:
			xqcut = 50.0
		if mass >= 1000 and mass <= 1900:
			xqcut = 60.0
		if mass == 2000:
			xqcut = 70.0
		for ctau in ctaus:
			if not os.path.exists(baseDir + '/geant4_higgsino/'+str(ctau)+'cm'):
                                os.mkdir(baseDir + '/geant4_higgsino/'+str(ctau)+'cm')
			if not os.path.exists(baseDir + '/hadronizers/'+str(ctau)+'cm'):
				os.mkdir(baseDir + '/hadronizers/'+str(ctau)+'cm')
			outputConfigFile   = baseDir + '/hadronizers/'+str(ctau)+'cm/Higgsino_M%dGeV_CTau%dcm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py' % (mass, ctau)
			outputParticleFile = baseDir + '/geant4_higgsino/'+str(ctau)+'cm/geant4_higgsino_%dGeV_ctau%dcm.slha' % (mass, ctau)
			slhaFile           = baseDir + '/SLHA_withDecay/Higgsino_%dGeV_CTAUcm_Isajet780.slha' % (mass)
			os.system('sed "s/XXX/' + str(mass) + '/g" ' + baseConfigFile + ' > ' + outputConfigFile)
			os.system('sed -i "s/YYY/' + str(int(ctau * 10.0)) + '/g" ' + outputConfigFile) # mm
			os.system('sed -i "s/ZZZ/' + str(xsecsHiggsino[mass]) + '/g" ' + outputConfigFile)
			os.system('sed -i "s/AAA/' + str(xqcut) + '/g" ' + outputConfigFile)
			mW1ss = findMassValue(slhaFile, 'w1ss')
			mZ1ss = findMassValue(slhaFile, 'z1ss')
			mZ2ss = findMassValue(slhaFile, 'z2ss')
			tau = ctau / c * 1.e9 # ns
			width = (1.97326979e-14 / ctau) # GeV
			os.system('sed "s/_MW1SS/' + str(mW1ss) + '/g" ' + baseParticleFile + ' > ' + outputParticleFile)
			os.system('sed -i "s/_MZ1SS/' + str(mZ1ss) + '/g" ' + outputParticleFile)
			os.system('sed -i "s/_MZ2SS/' + str(mZ2ss) + '/g" ' + outputParticleFile)
			os.system('sed -i "s/_CTAU/' + str(ctau) + '/g" ' + outputParticleFile)
			os.system('sed -i "s/_TAU/' + str(tau) + '/g" ' + outputParticleFile)
			os.system('sed -i "s/_WIDTH/' + str(width) + '/g" ' + outputParticleFile)
	print('Created electroweak (higgsino-like) configuration fragments and particle files in directory: ' + baseDir + '/hadronizers')

	print
	print('Now "scram b" and run this again with argument "2" to create the step 1 configs')
	print

################################################################
# step 2: create the GEN-SIM configs
# https://its.cern.ch/jira/browse/PDMVMCPROD-62
# https://its.cern.ch/jira/browse/PDMVMCPROD-71
################################################################
if scriptStep == 2:

	if not os.path.exists(baseDir + '/configs'):
		os.mkdir(baseDir + '/configs')
	if not os.path.exists(baseDir + '/configsEE'):
		os.mkdir(baseDir + '/configsEE')

	# first wino-like LSP case
	cmd = 'cmsDriver.py hadronizers/{0}cm/AMSB_chargino_M{1}GeV_CTau{0}cm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py'
	cmd += ' --fileout file:AMSB_chargino{1}GeV_ctau{0}cm_step1.root'
	cmd += ' --mc --eventcontent RAWSIM'
	cmd += ' --customise Configuration/DataProcessing/Utils.addMonitoring,SimG4Core/CustomPhysics/Exotica_HSCP_SIM_cfi'
	cmd += ' --datatier GEN-SIM --conditions 124X_mcRun3_2022_realistic_v12'
	cmd += ' --beamspot Realistic25ns13p6TeVEarly2022Collision --step LHE,GEN,SIM --geometry DB:Extended'
	cmd += ' --era Run3'
	cmd += ' --python_filename configs/{0}cm/AMSB_chargino_M{1}GeV_CTau{0}cm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_step1.py'
	cmd += ' --no_exec -n 10'

	for mass in xsecsWino:
		for ctau in ctaus:
			if not os.path.exists(baseDir + '/configs/'+str(ctau)+'cm'):
				os.mkdir(baseDir + '/configs/'+str(ctau)+'cm')
			os.system(cmd.format(ctau, mass))

	print('Created electroweak (wino-like) GEN-SIM configuration files in directory: ' + os.getcwd() + '/configs')

	# now higgsino-like LSP case
	cmd = 'cmsDriver.py hadronizers/{0}cm/Higgsino_M{1}GeV_CTau{0}cm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py'
	cmd += ' --fileout file:Higgsino_M{1}GeV_ctau{0}cm_step1.root'
	cmd += ' --mc --eventcontent RAWSIM'
	cmd += ' --customise Configuration/DataProcessing/Utils.addMonitoring,SimG4Core/CustomPhysics/Exotica_HSCP_SIM_cfi'
	cmd += ' --datatier GEN-SIM --conditions 124X_mcRun3_2022_realistic_v12'
	cmd += ' --beamspot Realistic25ns13p6TeVEarly2022Collision --step LHE,GEN,SIM --geometry DB:Extended'
	cmd += ' --era Run3'
	cmd += ' --python_filename configs/{0}cm/Higgsino_M{1}GeV_CTau{0}cm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_step1.py'
	cmd += ' --no_exec -n 10'

	for mass in xsecsHiggsino:
		for ctau in ctaus:
			if not os.path.exists(baseDir + '/configs/'+str(ctau)+'cm'):
				os.mkdir(baseDir + '/configs/'+str(ctau)+'cm')
			os.system(cmd.format(ctau, mass))

	print('Created electroweak (higgsino-like) GEN-SIM configuration files in directory: ' + os.getcwd() + '/configs')

	# now wino-like LSP EE case
	cmd = 'cmsDriver.py hadronizers/{0}cm/AMSB_chargino_M{1}GeV_CTau{0}cm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py'
	cmd += ' --fileout file:AMSB_chargino{1}GeV_ctau{0}cm_step1EE.root'
	cmd += ' --mc --eventcontent RAWSIM'
	cmd += ' --customise Configuration/DataProcessing/Utils.addMonitoring,SimG4Core/CustomPhysics/Exotica_HSCP_SIM_cfi'
	cmd += ' --datatier GEN-SIM --conditions 124X_mcRun3_2022_realistic_postEE_v1'
	cmd += ' --beamspot Realistic25ns13p6TeVEarly2022Collision --step LHE,GEN,SIM --geometry DB:Extended'
	cmd += ' --era Run3'
	cmd += ' --python_filename configsEE/{0}cm/AMSB_chargino_M{1}GeV_CTau{0}cm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_step1.py'
	cmd += ' --no_exec -n 10'

	for mass in xsecsWino:
		for ctau in ctaus:
			if not os.path.exists(baseDir + '/configsEE/'+str(ctau)+'cm'):
				os.mkdir(baseDir + '/configsEE/'+str(ctau)+'cm')
			os.system(cmd.format(ctau, mass))

	print('Created electroweak (wino-like) GEN-SIM configuration files in directory: ' + os.getcwd() + '/configsEE')

	# now higgsino-like LSP EE case
	cmd = 'cmsDriver.py hadronizers/{0}cm/Higgsino_M{1}GeV_CTau{0}cm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py'
	cmd += ' --fileout file:Higgsino_M{1}GeV_ctau{0}cm_step1EE.root'
	cmd += ' --mc --eventcontent RAWSIM'
	cmd += ' --customise Configuration/DataProcessing/Utils.addMonitoring,SimG4Core/CustomPhysics/Exotica_HSCP_SIM_cfi'
	cmd += ' --datatier GEN-SIM --conditions 124X_mcRun3_2022_realistic_postEE_v1'
	cmd += ' --beamspot Realistic25ns13p6TeVEarly2022Collision --step LHE,GEN,SIM --geometry DB:Extended'
	cmd += ' --era Run3'
	cmd += ' --python_filename configsEE/{0}cm/Higgsino_M{1}GeV_CTau{0}cm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_step1.py'
	cmd += ' --no_exec -n 10'

	for mass in xsecsHiggsino:
		for ctau in ctaus:
			if not os.path.exists(baseDir + '/configsEE/'+str(ctau)+'cm'):
				os.mkdir(baseDir + '/configsEE/'+str(ctau)+'cm')
			os.system(cmd.format(ctau, mass))

	print('Created electroweak (higgsino-like) GEN-SIM configuration files in directory: ' + os.getcwd() + '/configsEE')
