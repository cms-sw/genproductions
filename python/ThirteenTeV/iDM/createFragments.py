#!/usr/bin/env python
from __future__ import print_function
import os

process = 'iDM'
#template = 'iDM_DarkPhotonToMuMu_ctau-XXX_TuneCP5_13TeV_pythia8_cff.py'
template = 'iDM_DarkPhotonToMuMu_ctau-XXX_TuneCP5_13TeV_MLM_4f_max2j_pythia8_cff.py'

verbose = False

paramList = [
	1,
	10,
	100,
	1000
]

if __name__ == '__main__':
    
    for ctau in paramList:
        #fragName = 'iDM_DarkPhotonToMuMu_ctau-{0}_TuneCP5_13TeV_pythia8_cff.py'.format(str(ctau).replace('.', 'p'))
        fragName = 'iDM_DarkPhotonToMuMu_ctau-{0}_TuneCP5_13TeV_MLM_4f_max2j_pythia8_cff.py'.format(str(ctau).replace('.', 'p'))
        if os.path.isfile(fragName): continue
        os.system('cp {0} {1}'.format(template, fragName))
        cmd = 'sed -i "s#X__CTAU__X#{0}#g" {1}'.format(ctau, fragName)
        if verbose: print(cmd)
        os.system(cmd)
