#!/usr/bin/env python
from __future__ import print_function
import os
import math

process = 'iDM'
nameTag = 'mchi-{0}_dmchi-{1}_ctau-{2}'

verbose = True

paramList = [
	(1.05, 0.1, 1),
	(1.05, 0.1, 10),
	(1.05, 0.1, 100),
	(1.05, 0.1, 1000),
	(1.2, 0.4, 1),
	(1.2, 0.4, 10),
	(1.2, 0.4, 100),
	(1.2, 0.4, 1000),
	(3.15, 0.3, 1),
	(3.15, 0.3, 10),
	(3.15, 0.3, 100),
	(3.15, 0.3, 1000),
	(3.6, 1.2, 1),
	(3.6, 1.2, 10),
	(3.6, 1.2, 100),
	(3.6, 1.2, 1000),
	(5.25, 0.5, 1),
	(5.25, 0.5, 10),
	(5.25, 0.5, 100),
	(5.25, 0.5, 1000),
	(6, 2, 1),
	(6, 2, 10),
	(6, 2, 100),
	(6, 2, 1000),
	(10.5, 1, 1),
	(10.5, 1, 10),
	(10.5, 1, 100),
	(10.5, 1, 1000),
	(12, 4, 1),
	(12, 4, 10),
	(12, 4, 100),
	(12, 4, 1000),
	(21, 2, 1),
	(21, 2, 10),
	(21, 2, 100),
	(21, 2, 1000),
	(24, 8, 1),
	(24, 8, 10),
	(24, 8, 100),
	(24, 8, 1000),
	(31.5, 3, 1),
	(31.5, 3, 10),
	(31.5, 3, 100),
	(31.5, 3, 1000),
	(36, 12, 1),
	(36, 12, 10),
	(36, 12, 100),
	(36, 12, 1000),
	(42, 4, 1),
	(42, 4, 10),
	(42, 4, 100),
	(42, 4, 1000),
	(48, 16, 1),
	(48, 16, 10),
	(48, 16, 100),
	(48, 16, 1000),
	(52.5, 5, 1),
	(52.5, 5, 10),
	(52.5, 5, 100),
	(52.5, 5, 1000),
	(60, 20, 1),
	(60, 20, 10),
	(60, 20, 100),
	(60, 20, 1000),
	(63, 6, 1),
	(63, 6, 10),
	(63, 6, 100),
	(63, 6, 1000),
	(72, 24, 1),
	(72, 24, 10),
	(72, 24, 100),
	(72, 24, 1000),
	(84, 8, 1),
	(84, 8, 10),
	(84, 8, 100),
	(84, 8, 1000),
	(96, 32, 1),
	(96, 32, 10),
	(96, 32, 100),
	(96, 32, 1000),
	(105, 10, 1),
	(105, 10, 10),
	(105, 10, 100),
	(105, 10, 1000),
	(120, 40, 1),
	(120, 40, 10),
	(120, 40, 100),
	(120, 40, 1000)
        ]

if __name__ == '__main__':

    for mchi, dmchi, ctau in paramList:

	width = 1.97 * 1e-13 / ctau
	mchi1 = -dmchi/2 + mchi 
	mdp = 3 * mchi1

        if verbose: print(mchi, dmchi, ctau)

        mchi_str = str(mchi).replace('.', 'p')
        dmchi_str = str(dmchi).replace('.', 'p')
        ctau_str = str(ctau).replace('.', 'p')
        toDir = process+'__'+nameTag.format(mchi_str, dmchi_str, ctau_str)

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
