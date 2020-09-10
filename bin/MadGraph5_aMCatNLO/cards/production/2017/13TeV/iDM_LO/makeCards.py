#!/usr/bin/env python
from __future__ import print_function
import os
import math

process = 'iDM'
nameTag = 'Mchi-{0}_dMchi-{1}'

verbose = True

paramList = [
	(1.2, 0.4),
	(3.15, 0.3),
	(3.6, 1.2),
	(5.25, 0.5),
	(6.0, 2.0),
	(10.5, 1.0),
	(12.0, 4.0),
	(21.0, 2.0),
	(24.0, 8.0),
	(31.5, 3.0),
	(36.0, 12.0),
	(42.0, 4.0),
	(48.0, 16.0),
	(52.5, 5.0),
	(60.0, 20.0),
	(63.0, 6.0),
	(72.0, 24.0),
	(84.0, 8.0),
	(96.0, 32.0)
        ]

if __name__ == '__main__':

    for mchi, dmchi in paramList: #, ctau in paramList:

	#width = 1.97 * 1e-13 / ctau
	mchi1 = -dmchi/2 + mchi 
	mdp = 3 * mchi1

        if verbose: print(mchi, dmchi)#, ctau)

        mchi_str = str(mchi).replace('.', 'p')
        dmchi_str = str(dmchi).replace('.', 'p')
        #ctau_str = str(ctau).replace('.', 'p')
        toDir = process+'_'+nameTag.format(mchi_str, dmchi_str)#, ctau_str)

        if verbose: print(toDir)

        if os.path.isdir(toDir):
            os.system('rm -r {0}'.format(toDir))
        os.system('cp -r templates {0}'.format(toDir))

	# Don't need to set ctau (or width) in the MG gridpack
	# Lifetime replacement happens at the Pythia hadronizer
	# configuration

        cmd0 = 'sed -i "s#_MCHI_#{0}#g" {1}'.format(mchi, toDir+'/'+process+'_customizecards.dat')
        cmd1 = 'sed -i "s#_DMCHI_#{0}#g" {1}'.format(dmchi, toDir+'/'+process+'_customizecards.dat')
	cmd2 = 'sed -i "s#_MDP_#{0}#g" {1}'.format(mdp, toDir+'/'+process+'_customizecards.dat')
        #cmd3 = 'sed -i "s#_WCHI2_#{0}#g" {1}'.format(width, toDir+'/'+process+'_customizecards.dat')

        for cmd in [cmd0, cmd1, cmd2]:#, cmd3]:
            if verbose: print(cmd)
            os.system(cmd)
