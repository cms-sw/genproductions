#!/usr/bin/env python
from __future__ import print_function
import os
import math

process = 'BsTo2DpTo4Mu'
nameTag = 'MBs-{0}_MDp-{1}_ctau-{2}'

verbose = False

paramList = [
        (10, 0.3, 2.4),
        (10, 0.3, 24),
        (10, 0.3, 240),
        (10, 1.2, 9.6),
        (10, 1.2, 96),
        (10, 1.2, 960),
        (20, 0.3, 1.2),
        (20, 0.3, 12),
        (20, 0.3, 120),
        (20, 1.2, 4.8),
        (20, 1.2, 48),
        (20, 1.2, 480),
        (40, 0.3, 0.6),
        (40, 0.3, 6),
        (40, 0.3, 60),
        (40, 1.2, 2.4),
        (40, 1.2, 24),
        (40, 1.2, 240),
        (200, 0.3, 0.12),
        (200, 0.3, 1.2),
        (200, 0.3, 12),
        (200, 1.2, 0.48),
        (200, 1.2, 4.8),
        (200, 1.2, 48),
        (400, 0.3, 0.06),
        (400, 0.3, 0.6),
        (400, 0.3, 6),
        (400, 1.2, 0.24),
        (400, 1.2, 2.4),
        (400, 1.2, 24),
        (1000, 0.3, 0.024),
        (1000, 0.3, 0.24),
        (1000, 0.3, 2.4),
        (1000, 1.2, 0.096),
        (1000, 1.2, 0.96),
        (1000, 1.2, 9.6)
        ]

if __name__ == '__main__':

    for mBs, mDp, cTau in paramList:
        ## ctau = 0.08 * (0.1/mDp) * (1e-4/epsilon)**2  [mm]
        epsilon = math.sqrt(80/mDp/cTau) * 1e-6
        if verbose: print(mBs, mDp, epsilon)

        mBs_str = str(mBs)
        mDp_str = str(mDp).replace('.', 'p')
        ctau_str = str(cTau).replace('.', 'p')
        toDir = process+'__'+nameTag.format(mBs_str, mDp_str, ctau_str)
        if verbose: print(toDir)

        if os.path.isdir(toDir):
            os.system('rm -r {0}'.format(toDir))
        os.system('cp -r templates {0}'.format(toDir))
        cmd0 = 'sed -i "s#XMASS#{0}#g" {1}'.format(mBs, toDir+'/'+process+'_customizecards.dat')
        cmd1 = 'sed -i "s#MED#{0}#g" {1}'.format(mDp, toDir+'/'+process+'_customizecards.dat')
        cmd2 = 'sed -i "s#EPSILON#{0}#g" {1}'.format('{0:6e}'.format(epsilon), toDir+'/'+process+'_customizecards.dat')
        for cmd in [cmd0, cmd1, cmd2]:
            if verbose: print(cmd)
            os.system(cmd)
